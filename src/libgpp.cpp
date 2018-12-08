// File:      gpp.cpp  -- generic preprocessor
// Author:    Denis Auroux, Tristan Miller, Giumo Clanjor
// Contact:   Giumo Clanjor <cjxgm2@gmail.com>
//
// GPP:
// . Copyright (C) 1996, 1999, 2001 Denis Auroux
// . Copyright (C) 2003-2017 Tristan Miller
//
// libgpp:
// . Copyright (C) 2018 Giumo Clanjor
//
// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "libgpp.hpp"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <cstdint>

#include <string>
#include <utility>      // for std::move
#include <stdexcept>
#include <algorithm>

namespace
{
    namespace autocast
    {
        // Autocast is written by Giumo Clanjor and
        // has been contributed to the public domain.
        // Inspired by JAI's `xx` operator.
        #define AUTOCAST_TIGHT      ::autocast::Caster_Builder{} /
        #define AUTOCAST_LOOSE      ::autocast::Caster_Builder{} ==

        template <class T>
        struct Caster
        {
            constexpr Caster(T value): value{std::move(value)} {}

            template <class Target>
            constexpr operator Target () && { return Target(std::move(value)); }

            template <class Target>
            constexpr operator Target () const& { return Target(value); }

        private:
            T value;
        };

        struct Caster_Builder
        {
            template <class T>
            constexpr auto operator / (T value) const { return Caster<T>{value}; }

            template <class T>
            constexpr auto operator == (T value) const { return Caster<T>{value}; }
        };
    }

    #define XX      AUTOCAST_TIGHT
    #define XXX     AUTOCAST_LOOSE

    constexpr auto max_stack_depth = 50;
    constexpr auto max_arg_count = 100;
    constexpr auto max_digit_count_of_number = 15;

    struct Syntax_Mode {
        char const* macro_start;        // before macro name
        char const* macro_end;          // end macro without arg
        char const* macro_arg_start;    // start 1st argument
        char const* macro_arg_sep;      // separate arguments
        char const* macro_arg_end;      // end last argument
        char const* macro_arg_ref;      // how to refer to arguments in a def
        char quotechar;                 // quote next char
        char const* stackchar;          // characters to stack
        char const* unstackchar;        // characters to unstack
    };

    // translation for delimiters :
    //
    // > \001 = \b = ' ' = one or more spaces           \201 = \!b = non-space
    // > \002 = \w = zero or more spaces
    // > \003 = \B = one or more spaces or \n           \203 = \!B = non-space nor \n
    // > \004 = \W = zero or more spaces or \n
    // > \005 = \a = alphabetic (a-z, A-Z)              \205 = \!a = non-alphabetic
    // > \006 = \A = alphabetic or space/\n             \206 = \!A
    // > \007 = \# = numeric (0-9)                      \207 = \!#
    // > \010 = \i = identifier (a-zA-Z0-9_)            \210 = \!i
    // > \011 = \t                                      \211 = \!t
    // > \012 = \n                                      \212 = \!n
    // > \013 = \o = operator (+-*\/^<>=`~:.?@#&!%|)    \213 = \!o
    // > \014 = \O = operator or ()[]{}                 \214 = \!O

    // -                                         st   end   args    sep     arge  ref  quot  stk  unstk
    constexpr Syntax_Mode default_user_syntax = {"",  "",   "(",    ",",    ")",  "#", '\\', "(", ")" };
    constexpr Syntax_Mode default_meta_syntax = {"#", "\n", "\001", "\001", "\n", "#", '\\', "(", ")" };

    #define DEFAULT_OP_STRING (unsigned char const*)"+-*/\\^<>=`~:.?@#&!%|"
    #define DEFAULT_OP_PLUS   (unsigned char const*)"()[]{}"
    #define DEFAULT_ID_STRING (unsigned char const*)"\005\007_" // or equiv. "A-Za-z0-9_"

    #define LOG_32_BITS 5
    #define CHARSET_SUBSET_LEN (256 >> LOG_32_BITS)
    using Charset_Subset = std::uint32_t *;

    struct Comment {
        char *start;        // how the comment/string starts
        char *end;          // how it ends
        char quote;         // how to prevent it from ending
        char warn;          // a character that shouldn't be in there
        int flags[3];       // meta, user, text
        Comment *next;
    };

    #define OUTPUT_TEXT     0x1   // what's inside will be output
    #define OUTPUT_DELIM    0x2   // the delimiters will be output
    #define PARSE_MACROS    0x4   // macros inside will be parsed
    #define FLAG_IGNORE     0x40

    #define FLAG_STRING    (OUTPUT_TEXT|OUTPUT_DELIM)
    #define FLAG_COMMENT   0

    #define FLAG_META 0
    #define FLAG_USER 1
    #define FLAG_TEXT 2

    // some stuff I removed because it made for some impossible situations :
    //
    // > #define PARSE_COMMENTS  0x8
    // >   comments inside comments will not be parsed because nesting comments is
    // >   too complicated (syntax conflicts, esp. to find a comment's end)
    // >   -- of course, unless the comment is ignored.
    //
    // > #define MACRO_FRIENDLY  0x20
    // >   a comment-end is to be processed even if an unfinished macro call has
    // >   started inside the comment, otherwise it's too hard do decide in advance
    // >   where a comment ends. in particular foo('bar((((') is valid.
    //
    // > #define PREVENT_DELIM   0x10
    // >   all comments will prevent macro delimitation, i.e. foo('bar) is invalid.
    // >   -- of course, unless the comment is ignored.
    // >   too bad, #define foo '...    terminates only at following "'".
    // >   unless one adds quotechars like in #define foo \' ...
    //
    // ALSO NOTE : comments are not allowed before the end of the first argument
    // to a meta-macro. E.g. this is legal :   #define foo <* blah *> 3
    // this is not legal :                     #define <* blah *> foo 3
    // if a comment occurs here, the behavior depends on the actual meta-macro :
    // most will yield an error and stop gpp (#define, #undef, #ifdef/ifndef,
    // #defeval, #mode) ; #if and #eval should be ok ;
    // #ifeq will always fail while #ifneq will always succeed ;

    struct Specs {
        Syntax_Mode user, meta;
        Comment *comments;
        Specs *stack_next;
        Charset_Subset op_set, ext_op_set, id_set;
    };

    struct Macro {
        char *username, *macrotext, **argnames;
        int macrolen, nnamedargs;
        Specs *define_specs;
        int defined_in_comment;
    };

    using Output_Character_Fn = auto (char ch, void* data) -> void;
    struct Output_Context {
        char *buf;
        int len, bufsize;
        Output_Character_Fn* output_char;
        void* output_char_data;
    };

    struct Input_Context {
        char *buf;
        char *malloced_buf; // what was actually malloc-ed (buf may have shifted)
        int len, bufsize;
        int lineno;
        int read_stdin;
        int argc;
        char **argv;
        char **namedargs;
        Output_Context *out;
        int eof;
        int in_comment;
        int ambience; // FLAG_TEXT, FLAG_USER or FLAG_META
        int may_have_args;
    };

    // strdup() and my_strcasecmp() are not ANSI C, so here we define our own
    // versions in case the compiler does not support them
    char *my_strdup(const char *s) {
        size_t len = strlen(s) + 1;
        char *newstr = XX malloc(len);
        return newstr ? (char *) memcpy(newstr, s, len) : nullptr ;
    }

    int my_strcasecmp(const char *s, const char *s2) {
        do {
            char c1 = tolower(*s);
            char c2 = tolower(*s2);
            if (c1 > c2)
                return 1;
            if (c1 < c2)
                return -1;
        } while (*s++ && *s2++);
        return 0;
    }

    [[noreturn]]
    void bug(const char *s) {
        throw std::runtime_error{s};
    }

    void warning(const char *s) {
        fprintf(stderr, "warning: %s\n", s);
    }

    Specs *clone_specs(const Specs *Q) {
        Specs *P;
        Comment *x, *y;

        P = XX malloc(sizeof *P);
        if (P == nullptr )
            bug("out of memory.");
        memcpy(P, Q, sizeof(Specs));
        P->stack_next = nullptr;
        if (Q->comments != nullptr )
            P->comments = XX malloc(sizeof *(P->comments));
        for (x = Q->comments, y = P->comments; x != nullptr ;
                x = x->next, y = y->next) {
            memcpy(y, x, sizeof(Comment));
            y->start = my_strdup(x->start);
            y->end = my_strdup(x->end);
            if (x->next != nullptr )
                y->next = XX malloc(sizeof *(y->next));
        }
        return P;
    }

    void free_comments(Specs *Q) {
        Comment *p;

        while (Q && Q->comments != nullptr ) {
            p = Q->comments;
            Q->comments = p->next;
            free(p->start);
            free(p->end);
            free(p);
        }
    }

    int is_delim(unsigned char c) {
        if (c >= 128)
            return 0;
        if ((c >= '0') && (c <= '9'))
            return 0;
        if ((c >= 'A') && (c <= 'Z'))
            return 0;
        if ((c >= 'a') && (c <= 'z'))
            return 0;
        if (c == '_')
            return 0;
        return 1;
    }

    int is_white(char c) {
        if (c == ' ')
            return 1;
        if (c == '\t')
            return 1;
        if (c == '\n')
            return 1;
        return 0;
    }

    // replace "\\n" by "\n" for C strings in-place
    char *str_nl(char *s, int check_delim) {
        char *u;
        int neg;
        u = s;
        if (check_delim && !is_delim(*s))
            bug("character not allowed to start a syntax specifier");
        while (*s != '"') {
            if (((*s & 0x60) == 0) && (*s != '\n') && (*s != '\t'))
                bug("character not allowed in syntax specifier");
            if (*s == '\\') {
                neg = (s[1] == '!');
                switch (s[neg + 1]) {
                case 'n':
                case 'r':
                    *u = '\n';
                    break;
                case 't':
                    *u = '\t';
                    break;
                case 'b': // one or more spaces
                    *u = '\001';
                    break;
                case 'w': // zero or more spaces
                    if (neg)
                        bug("\\w and \\W cannot be negated");
                    *u = '\002';
                    break;
                case 'B': // one or more spaces or \n
                    *u = '\003';
                    break;
                case 'W': // zero or more spaces or \n
                    if (neg)
                        bug("\\w and \\W cannot be negated");
                    *u = '\004';
                    break;
                case 'a': // alphabetic
                    *u = '\005';
                    break;
                case 'A': // alphabetic + space
                    *u = '\006';
                    break;
                case '#': // numeric
                    *u = '\007';
                    break;
                case 'i': // identifier
                    *u = '\010';
                    break;
                case 'o': // operator
                    *u = '\013';
                    break;
                case 'O': // operator/parenthesis
                    *u = '\014';
                    break;
                case '"':
                case '\\':
                    if (!neg) {
                        *u = s[1];
                        break;
                    }
                    // fallthrough
                default:
                    bug("unknown escape sequence in syntax specifier");
                }
                if (neg > 0)
                    *u += (char) 128;
                s += neg + 1;
            } else if (*s == ' ')
                *u = '\001';
            else
                *u = *s;
            if (*s == 0)
                bug("unterminated string in #mode command");
            s++;
            u++;
        }
        *u = 0;
        return (s + 1);
    }

    int is_whitesep(const char *s) {
        while (is_white(*s) || (*s == '\001') || (*s == '\002') || (*s == '\003') || (*s == '\004'))
            s++;
        return (*s == 0);
    }

    int nowhite_strcmp(char *s, char *t) {
        char *p;

        while (is_white(*s))
            s++;
        while (is_white(*t))
            t++;
        if ((*s == 0) || (*t == 0))
            return strcmp(s, t);
        p = s + strlen(s) - 1;
        while (is_white(*p))
            *(p--) = 0;
        p = t + strlen(t) - 1;
        while (is_white(*p))
            *(p--) = 0;
        return strcmp(s, t);
    }

    int parse_comment_specif(char c) {
        switch (c) {
        case 'I':
        case 'i':
            return FLAG_IGNORE;
        case 'c':
            return FLAG_COMMENT;
        case 's':
            return FLAG_STRING;
        case 'q':
            return OUTPUT_TEXT;
        case 'S':
            return FLAG_STRING | PARSE_MACROS;
        case 'Q':
            return OUTPUT_TEXT | PARSE_MACROS;
        case 'C':
            return FLAG_COMMENT | PARSE_MACROS;
        default:
            bug("invalid comment/string modifier");
            return 0;
        }
    }

    void add_comment(Specs *S, const char *specif, char *start, char *end, char quote, char warn) {
        Comment *p;

        if (*start == 0)
            bug("comment/string start delimiter must be non-empty");
        for (p = S->comments; p != nullptr ; p = p->next)
            if (!strcmp(p->start, start)) {
                if (strcmp(p->end, end)) // already exists with a different end
                    bug("conflicting comment/string delimiter specifications");
                free(p->start);
                free(p->end);
                break;
            }

        if (p == nullptr ) {
            p = XX malloc(sizeof *p);
            p->next = S->comments;
            S->comments = p;
        }
        p->start = start;
        p->end = end;
        p->quote = quote;
        p->warn = warn;
        if (strlen(specif) != 3)
            bug("invalid comment/string modifier");
        p->flags[FLAG_META] = parse_comment_specif(specif[0]);
        p->flags[FLAG_USER] = parse_comment_specif(specif[1]);
        p->flags[FLAG_TEXT] = parse_comment_specif(specif[2]);
    }

    void delete_comment(Specs *S, char *start) {
        Comment *p, *q;

        q = nullptr;
        for (p = S->comments; p != nullptr ; p = p->next) {
            if (!strcmp(p->start, start)) {
                if (q == nullptr )
                    S->comments = p->next;
                else
                    q->next = p->next;
                free(p->start);
                free(p->end);
                free(p);
                free(start);
                return;
            } else
                q = p;
        }
        free(start);
    }

    int is_in_charset(Charset_Subset x, int c) {
        return (x[c >> LOG_32_BITS] & 1L << (c & ((1 << LOG_32_BITS) - 1))) != 0;
    }

    void add_to_charset(Charset_Subset x, int c) {
        x[c >> LOG_32_BITS] |= 1L << (c & ((1 << LOG_32_BITS) - 1));
    }

    Charset_Subset make_charset_subset(unsigned char const* s) {
        Charset_Subset x;
        int i;
        unsigned char c;

        x = XX malloc(CHARSET_SUBSET_LEN * sizeof(std::uint32_t));
        for (i = 0; i < CHARSET_SUBSET_LEN; i++)
            x[i] = 0;
        while (*s != 0) {
            if (!((*s) & 0x60)) { // special sequences
                if ((*s) & 0x80)
                    bug("negated special sequences not allowed in charset specifications");
                switch ((*s) & 0x1f) {
                case '\002': // \w, \W, \i, \o, \O not allowed
                case '\004':
                case '\010':
                case '\013':
                case '\014':
                    bug("special sequence not allowed in charset specification");
                case '\003':
                    add_to_charset(x, '\n');
                    // fallthrough
                case '\001':
                    add_to_charset(x, ' ');
                    // fallthrough
                case '\011':
                    add_to_charset(x, '\t');
                    break;
                case '\006':
                    add_to_charset(x, '\n');
                    add_to_charset(x, ' ');
                    add_to_charset(x, '\t');
                    // fallthrough
                case '\005':
                    for (c = 'A'; c <= 'Z'; c++)
                        add_to_charset(x, c);
                    for (c = 'a'; c <= 'z'; c++)
                        add_to_charset(x, c);
                    break;
                case '\007':
                    for (c = '0'; c <= '9'; c++)
                        add_to_charset(x, c);
                    break;
                case '\012':
                    add_to_charset(x, '\n');
                    break;
                }
            } else if ((s[1] == '-') && ((s[2] & 0x60) != 0) && (s[2] >= *s)) {
                for (c = *s; c <= s[2]; c++)
                    add_to_charset(x, c);
                s += 2;
            } else
                add_to_charset(x, *s);
            s++;
        }
        return x;
    }

    int id_equal(const char *b, int l, const char *s) {
        int i;

        if ((int) strlen(s) != l)
            return 0;
        for (i = 0; i < l; i++)
            if (b[i] != s[i])
                return 0;
        return 1;
    }

    int splice_infix(const char *buf, int pos1, int pos2, char const* sep, int *spl1, int *spl2) {
        int pos, numpar, l;
        const char *p;

        numpar = 0;
        l = strlen(sep);
        for (pos = pos2 - 1, p = buf + pos; pos >= pos1; pos--, p--) {
            if (*p == ')')
                numpar++;
            if (*p == '(')
                numpar--;
            if (numpar < 0)
                return 0;
            if ((numpar == 0) && (pos2 - pos >= l) && !strncmp(p, sep, l)) {
                *spl1 = pos;
                *spl2 = pos + l;
                return 1;
            }
        }
        return 0;
    }

    int do_arithm_eval(char *buf, int pos1, int pos2, int *result) {
        int spl1, spl2, result1, result2, l;
        char c, *p;

        while ((pos1 < pos2) && is_white(buf[pos1]))
            pos1++;
        while ((pos1 < pos2) && is_white(buf[pos2 - 1]))
            pos2--;
        if (pos1 == pos2)
            return 0;

        // look for C operators starting with lowest precedence

        if (splice_infix(buf, pos1, pos2, "||", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2))
                return 0;
            *result = result1 || result2;
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "&&", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2))
                return 0;
            *result = result1 && result2;
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "|", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2))
                return 0;
            *result = result1 | result2;
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "^", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2))
                return 0;
            *result = result1 ^ result2;
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "&", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2))
                return 0;
            *result = result1 & result2;
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "!=", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2)) {
                // revert to string comparison
                while ((pos1 < spl1) && is_white(buf[spl1 - 1]))
                    spl1--;
                while ((pos2 > spl2) && is_white(buf[spl2]))
                    spl2++;
                if (spl1 - pos1 != pos2 - spl2)
                    *result = 1;
                else
                    *result = (strncmp(buf + pos1, buf + spl2, spl1 - pos1) != 0);
            } else
                *result = (result1 != result2);
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "==", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2)) {
                // revert to string comparison
                while ((pos1 < spl1) && is_white(buf[spl1 - 1]))
                    spl1--;
                while ((pos2 > spl2) && is_white(buf[spl2]))
                    spl2++;
                if (spl1 - pos1 != pos2 - spl2)
                    *result = 0;
                else
                    *result = (strncmp(buf + pos1, buf + spl2, spl1 - pos1) == 0);
            } else
                *result = (result1 == result2);
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "=~", &spl1, &spl2)) {
            bug("globbing not allowed");
        }

        if (splice_infix(buf, pos1, pos2, ">=", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2)) {
                // revert to string comparison
                while ((pos1 < spl1) && is_white(buf[spl1 - 1]))
                    spl1--;
                while ((pos2 > spl2) && is_white(buf[spl2]))
                    spl2++;
                l = spl1 - pos1;
                if (l > pos2 - spl2)
                    l = pos2 - spl2;
                result1 = strncmp(buf + pos1, buf + spl2, l);
                *result = (result1 > 0)
                        || ((result1 == 0) && (spl1 - pos1 >= pos2 - spl2));
            } else
                *result = (result1 >= result2);
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, ">", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2)) {
                // revert to string comparison
                while ((pos1 < spl1) && is_white(buf[spl1 - 1]))
                    spl1--;
                while ((pos2 > spl2) && is_white(buf[spl2]))
                    spl2++;
                l = spl1 - pos1;
                if (l > pos2 - spl2)
                    l = pos2 - spl2;
                result1 = strncmp(buf + pos1, buf + spl2, l);
                *result = (result1 > 0)
                        || ((result1 == 0) && (spl1 - pos1 > pos2 - spl2));
            } else
                *result = (result1 > result2);
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "<=", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2)) {
                // revert to string comparison
                while ((pos1 < spl1) && is_white(buf[spl1 - 1]))
                    spl1--;
                while ((pos2 > spl2) && is_white(buf[spl2]))
                    spl2++;
                l = spl1 - pos1;
                if (l > pos2 - spl2)
                    l = pos2 - spl2;
                result1 = strncmp(buf + pos1, buf + spl2, l);
                *result = (result1 < 0)
                        || ((result1 == 0) && (spl1 - pos1 <= pos2 - spl2));
            } else
                *result = (result1 <= result2);
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "<", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2)) {
                // revert to string comparison
                while ((pos1 < spl1) && is_white(buf[spl1 - 1]))
                    spl1--;
                while ((pos2 > spl2) && is_white(buf[spl2]))
                    spl2++;
                l = spl1 - pos1;
                if (l > pos2 - spl2)
                    l = pos2 - spl2;
                result1 = strncmp(buf + pos1, buf + spl2, l);
                *result = (result1 < 0)
                        || ((result1 == 0) && (spl1 - pos1 < pos2 - spl2));
            } else
                *result = (result1 < result2);
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "+", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2))
                return 0;
            *result = result1 + result2;
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "-", &spl1, &spl2))
            if (spl1 != pos1) {
                if (!do_arithm_eval(buf, pos1, spl1, &result1)
                        || !do_arithm_eval(buf, spl2, pos2, &result2))
                    return 0;
                *result = result1 - result2;
                return 1;
            }

        if (splice_infix(buf, pos1, pos2, "*", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2))
                return 0;
            *result = result1 * result2;
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "/", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2))
                return 0;
            if (result2 == 0)
                bug("division by zero in expression");
            *result = result1 / result2;
            return 1;
        }

        if (splice_infix(buf, pos1, pos2, "%", &spl1, &spl2)) {
            if (!do_arithm_eval(buf, pos1, spl1, &result1)
                    || !do_arithm_eval(buf, spl2, pos2, &result2))
                return 0;
            if (result2 == 0)
                bug("division by zero in expression");
            *result = result1 % result2;
            return 1;
        }

        if (buf[pos1] == '~') {
            if (!do_arithm_eval(buf, pos1 + 1, pos2, &result1))
                return 0;
            *result = ~result1;
            return 1;
        }

        if (buf[pos1] == '!') {
            if (!do_arithm_eval(buf, pos1 + 1, pos2, &result1))
                return 0;
            *result = !result1;
            return 1;
        }

        if (buf[pos1] == '-') {
            if (!do_arithm_eval(buf, pos1 + 1, pos2, &result1))
                return 0;
            *result = -result1;
            return 1;
        }

        // add the length() builtin to measure the length of the macro expansion
        if (strncmp(buf + pos1, "length(", strlen("length(")) == 0) {
            if (buf[pos2 - 1] != ')')
                return 0;
            *result = pos2 - pos1 - strlen("length()");
            return 1;
        }

        if (buf[pos1] == '(') {
            if (buf[pos2 - 1] != ')')
                return 0;
            return do_arithm_eval(buf, pos1 + 1, pos2 - 1, result);
        }

        c = buf[pos2];
        buf[pos2] = 0;
        *result = (int) strtol(buf + pos1, &p, 0);
        buf[pos2] = c;
        return (p == buf + pos2);
    }

    struct Non_Relocatable
    {
        Non_Relocatable() = default;
        Non_Relocatable(Non_Relocatable const&) = delete;
        Non_Relocatable(Non_Relocatable     &&) = delete;
        auto operator = (Non_Relocatable const&) -> Non_Relocatable& = delete;
        auto operator = (Non_Relocatable     &&) -> Non_Relocatable& = delete;
    };

    struct Preprocessor final: Non_Relocatable
    {
        Preprocessor()
        {
            nalloced = 31;
            macros = XX malloc(nalloced * sizeof *macros);

            S = XX malloc(sizeof *S);
            S->user = default_user_syntax;
            S->meta = default_meta_syntax;
            S->comments = nullptr;
            S->stack_next = nullptr;
            S->op_set = make_charset_subset(DEFAULT_OP_STRING);
            S->ext_op_set = make_charset_subset(DEFAULT_OP_PLUS);
            S->id_set = make_charset_subset(DEFAULT_ID_STRING);

            add_comment(S, "ccc", my_strdup("/*"), my_strdup("*/"), 0, 0);
            add_comment(S, "ccc", my_strdup("//"), my_strdup("\n"), 0, 0);
            add_comment(S, "ccc", my_strdup("\\\003"), my_strdup(""), 0, 0);
            add_comment(S, "sss", my_strdup("\""), my_strdup("\""), '\\', '\n');
            add_comment(S, "sss", my_strdup("'"), my_strdup("'"), '\\', '\n');

            C = XX malloc(sizeof *C);
            C->argc = 0;
            C->argv = nullptr;
            C->out = XX malloc(sizeof *(C->out));
            C->out->bufsize = 0;
            C->lineno = 1;
            C->namedargs = nullptr;
            C->in_comment = 0;
            C->ambience = FLAG_TEXT;
            C->may_have_args = 0;
        }

        ~Preprocessor()
        {
            if (macros) {
                while (nmacros > 0)
                    delete_macro(nmacros-1);
                free(macros);
            }

            if (S) {
                free(S->op_set);
                free(S->ext_op_set);
                free(S->id_set);
                free_comments(S);
                free(S);
            }

            if (C) {
                free(C->malloced_buf);
                free(C->out);
                free(C);
            }
        }

        auto preprocess_stdin_to_stdout() -> void
        {
            C->read_stdin = 1;
            C->eof = 0;
            C->bufsize = 80;
            C->len = 0;
            C->buf = C->malloced_buf = XX malloc(C->bufsize);
            C->out->output_char = [] (char x, void*) { putchar(x); };
            process_context();
        }

        auto preprocess_to_buffer(std::string const& source, std::string& output) -> void
        {
            C->read_stdin = 0;
            C->eof = 0;
            C->buf = C->malloced_buf = XX malloc(source.size() + 2);    // prepend '\n', append '\0'

            auto first = &C->buf[1];
            auto last = std::copy_if(begin(source), end(source), first, [] (char x) { return (x != '\r'); });
            *last = '\0';
            C->buf[0] = '\n';
            C->len = XXX last - C->buf;
            C->bufsize = C->len + 1;

            C->out->output_char_data = &output;
            C->out->output_char = [] (char x, void* raw_output) {
                auto& output = *(std::string*)(raw_output);
                output += x;
            };
            process_context();
        }

    private:
        Specs* S{};

        Macro* macros{};
        int nmacros{}, nalloced{};

        short warning_level = 2;

        Input_Context* C{};

        int commented[max_stack_depth]{}, iflevel{};
        // commented = 0: output, 1: not output,
        // 2: not output because we're in a #elif and we've already gone through
        // the right case (so #else/#elif can't toggle back to output)

        void push_specs(const Specs *X) {
            Specs *P;

            P = clone_specs(X);
            P->stack_next = S;
            S = P;
        }

        void pop_specs() {
            Specs *P;

            P = S;
            S = P->stack_next;
            free_comments(P);
            free(P);
            if (S == nullptr )
                bug("#mode restore without #mode save");
        }

        void newmacro(const char *s, int len, int hasspecs) {
            if (nmacros == nalloced) {
                nalloced = 2 * nalloced + 1;
                macros = XX realloc(macros, nalloced * sizeof *macros);
                if (macros == nullptr )
                    bug("out of memory");
            }
            macros[nmacros].username = XX malloc(len + 1);
            strncpy(macros[nmacros].username, s, len + 1);
            macros[nmacros].username[len] = 0;
            macros[nmacros].argnames = nullptr;
            macros[nmacros].nnamedargs = 0;
            macros[nmacros].defined_in_comment = 0;
            if (hasspecs)
                macros[nmacros].define_specs = clone_specs(S);
            else
                macros[nmacros].define_specs = nullptr;
        }

        void lookup_arg_refs(int n) {
            int i, l;
            char *p;

            if (macros[n].argnames != nullptr )
                return; // don't mess with those
            macros[n].nnamedargs = -1;
            l = strlen(S->user.macro_arg_ref);
            for (i = 0, p = macros[n].macrotext; i < macros[n].macrolen; i++, p++) {
                if ((*p != 0) && (*p == S->user.quotechar)) {
                    i++;
                    p++;
                } else if (!strncmp(p, S->user.macro_arg_ref, l))
                    if ((p[l] >= '1') && (p[l] <= '9')) {
                        macros[n].nnamedargs = 0;
                        return;
                    }
            }
        }

        void outchar(char c) {
            if (C->out->bufsize) {
                if (C->out->len + 1 == C->out->bufsize) {
                    C->out->bufsize = C->out->bufsize * 2;
                    C->out->buf = XX realloc(C->out->buf, C->out->bufsize);
                    if (C->out->buf == nullptr )
                        bug("out of memory");
                }
                C->out->buf[C->out->len++] = c;
            } else {
                if (c != 13) {
                    C->out->output_char(c, C->out->output_char_data);
                }
            }
        }

        void sendout(const char *s, int l, int proc) // only process the quotechar, that's all
        {
            int i;

            if (!commented[iflevel])
                for (i = 0; i < l; i++) {
                    if (proc && (s[i] != 0) && (s[i] == S->user.quotechar)) {
                        i++;
                        if (i == l)
                            return;
                    }
                    if (s[i] != 0)
                        outchar(s[i]);
                }
        }

        void extend_buf(int pos) {
            char *p;
            if (C->bufsize <= pos) {
                C->bufsize += pos; // approx double
                p = XX malloc(C->bufsize);
                memcpy(p, C->buf, C->len);
                free(C->malloced_buf);
                C->malloced_buf = C->buf = p;
                if (C->buf == nullptr )
                    bug("out of memory");
            }
        }

        char get_char(int pos) {
            static int lastchar = -666;
            int c;

            if (lastchar == -666 && !strcmp(S->meta.macro_end, "\n"))
                lastchar = '\n';

            if (!C->read_stdin) {
                if (pos >= C->len)
                    return 0;
                else
                    return C->buf[pos];
            }
            extend_buf(pos);
            while (pos >= C->len) {
                do {
                    c = getchar();
                } while (c == 13);
                if (lastchar == '\n')
                    C->lineno++;
                lastchar = c;
                if (c == EOF)
                    c = 0;
                C->buf[C->len++] = (char) c;
            }
            return C->buf[pos];
        }

        int whiteout(int *pos1, int *pos2) // remove whitespace on both sides
        {
            while ((*pos1 < *pos2) && is_white(get_char(*pos1)))
                (*pos1)++;
            while ((*pos1 < *pos2) && is_white(get_char(*pos2 - 1)))
                (*pos2)--;
            return (*pos1 < *pos2);
        }

        int identifier_end(int start) {
            char c;

            c = get_char(start);
            if (c == 0)
                return start;
            if (c == S->user.quotechar) {
                c = get_char(start + 1);
                if (c == 0)
                    return (start + 1);
                if (is_delim(c))
                    return (start + 2);
                start += 2;
                c = get_char(start);
            }
            while (!is_delim(c))
                c = get_char(++start);
            return start;
        }

        int iter_identifier_end(int start) {
            int x;
            while (1) {
                x = identifier_end(start);
                if (x == start)
                    return x;
                start = x;
            }
        }

        int match_sequence(const char *s, int *pos) {
            int i = *pos;
            int match;
            char c;

            while (*s != 0) {
                if (!((*s) & 0x60)) { // special sequences
                    match = 1;
                    switch ((*s) & 0x1f) {
                    case '\001':
                        c = get_char(i++);
                        if ((c != ' ') && (c != '\t')) {
                            match = 0;
                            break;
                        }
                        // fallthrough
                    case '\002':
                        i--;
                        do {
                            c = get_char(++i);
                        } while ((c == ' ') || (c == '\t'));
                        break;
                    case '\003':
                        c = get_char(i++);
                        if ((c != ' ') && (c != '\t') && (c != '\n')) {
                            match = 0;
                            break;
                        }
                        // fallthrough
                    case '\004':
                        i--;
                        do {
                            c = get_char(++i);
                        } while ((c == ' ') || (c == '\t') || (c == '\n'));
                        break;
                    case '\006':
                        c = get_char(i++);
                        match = ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'))
                                || (c == ' ') || (c == '\t') || (c == '\n');
                        break;
                    case '\005':
                        c = get_char(i++);
                        match = ((c >= 'a') && (c <= 'z'))
                                || ((c >= 'A') && (c <= 'Z'));
                        break;
                    case '\007':
                        c = get_char(i++);
                        match = ((c >= '0') && (c <= '9'));
                        break;
                    case '\010':
                        c = get_char(i++);
                        match = is_in_charset(S->id_set, c);
                        break;
                    case '\011':
                        c = get_char(i++);
                        match = (c == '\t');
                        break;
                    case '\012':
                        c = get_char(i++);
                        match = (c == '\n');
                        break;
                    case '\013':
                        c = get_char(i++);
                        match = is_in_charset(S->op_set, c);
                        break;
                    case '\014':
                        c = get_char(i++);
                        match = is_in_charset(S->ext_op_set, c)
                                || is_in_charset(S->op_set, c);
                        break;
                    }
                    if ((*s) & 0x80)
                        match = !match;
                    if (!match)
                        return 0;
                } else if (get_char(i++) != *s)
                    return 0;
                s++;
            }
            *pos = i;
            return 1;
        }

        int match_end_sequence(const char *s, int *pos) {
            if (*s == 0)
                return 1;
            // if terminator is \n and we're at end of input, let it be...
            if (get_char(*pos) == 0 && s[0] == '\n' && s[1] == 0)
                return 1;
            if (!match_sequence(s, pos))
                return 0;
            return 1;
        }

        int match_start_sequence(const char *s, int *pos) {
            char c;
            int match;

            if (!((*s) & 0x60)) { // special sequences from prev. context
                c = get_char(*pos - 1);
                match = 1;
                if (*s == 0)
                    return 1;
                switch ((*s) & 0x1f) {
                case '\001':
                    if ((c != ' ') && (c != '\t')) {
                        match = 0;
                        break;
                    }
                case '\002':
                    break;
                case '\003':
                    if ((c != ' ') && (c != '\t') && (c != '\n')) {
                        match = 0;
                        break;
                    }
                case '\004':
                    break;
                case '\006':
                    if ((c == ' ') || (c == '\t') || (c == '\n'))
                        break;
                    // fallthrough
                case '\005':
                    match = ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'));
                    break;
                case '\007':
                    match = ((c >= '0') && (c <= '9'));
                    break;
                case '\010':
                    match = is_in_charset(S->id_set, c);
                    break;
                case '\011':
                    match = (c == '\t');
                    break;
                case '\012':
                    match = (c == '\n');
                    break;
                case '\013':
                    match = is_in_charset(S->op_set, c);
                    break;
                case '\014':
                    match = is_in_charset(S->ext_op_set, c) || is_in_charset(S->op_set, c);
                    break;
                }
                if ((*s) & 0x80)
                    match = !match;
                if (!match)
                    return 0;
                s++;
            }
            return match_sequence(s, pos);
        }

        int find_ident(const char *b, int l) {
            int i;

            for (i = 0; i < nmacros; i++)
                if (id_equal(b, l, macros[i].username))
                    return i;
            return -1;
        }

        int find_named_arg(const char *b, int l) {
            char *s;
            int i;

            for (i = 0;; i++) {
                s = C->namedargs[i];
                if (s == nullptr )
                    return -1;
                if (id_equal(b, l, s))
                    return i;
            }
        }

        void shift_in(int l) {
            int i;

            if (l <= 1)
                return;
            l--;
            if (l >= C->len)
                C->len = 0;
            else {
                if (C->len - l > 100) { // we want to shrink that buffer
                    C->buf += l;
                    C->bufsize -= l;
                } else
                    for (i = l; i < C->len; i++)
                        C->buf[i - l] = C->buf[i];
                C->len -= l;
                C->eof = (C->buf[0] == 0);
            }
            if (C->len <= 1) {
                if (C->read_stdin)
                    C->eof = feof(stdin);
                else
                    C->eof = 1;
            }
        }

        int find_comment_end(const char *endseq, char quote, char warn, int pos,
                int flags) {
            int i;
            char c;

            while (1) {
                c = get_char(pos);
                i = pos;
                if (match_end_sequence(endseq, &i))
                    return pos;
                if (c == 0)
                    bug("input ended while scanning a comment/string");
                if (c == warn) {
                    warn = 0;
                    if (warning_level > 1)
                        warning("possible comment/string termination problem");
                }
                if (c == quote)
                    pos += 2;
                else if ((flags & PARSE_MACROS) && (c == S->user.quotechar))
                    pos += 2;
                else
                    pos++;
            }
        }

        void skip_possible_comments(int *pos, int cmtmode, int silentonly) {
            int found;
            Comment *c;

            if (C->in_comment)
                return;
            do {
                found = 0;
                if (get_char(*pos) == 0)
                    return; // EOF
                for (c = S->comments; c != nullptr ; c = c->next)
                    if (!(c->flags[cmtmode] & FLAG_IGNORE))
                        if (!silentonly || (c->flags[cmtmode] == FLAG_COMMENT))
                            if (match_start_sequence(c->start, pos)) {
                                *pos = find_comment_end(c->end, c->quote, c->warn, *pos,
                                        c->flags[cmtmode]);
                                match_end_sequence(c->end, pos);
                                found = 1;
                                break;
                            }
            } while (found);
        }

        // look for a possible user macro.
        // input :  idstart = scan start
        // .        idcheck = check id for long macro forms before splicing args ?
        // .        cmtmode = comment mode (FLAG_META or FLAG_USER)
        // output : idstart/idend = macro name location
        // .        sh_end/lg_end = macro form end (-1 if no match)
        // .        argb/arge     = argument locations for long form
        // .        argc          = argument count for long form
        // .        id            = macro id, if idcheck was set at input
        int splice_possible_user(int *idstart, int *idend, int *sh_end, int *lg_end,
                int *argb, int *arge, int *argc, int idcheck, int *id, int cmtmode) {
            int match, k, pos;

            if (!match_start_sequence(S->user.macro_start, idstart))
                return 0;
            *idend = identifier_end(*idstart);
            if ((*idend) && !get_char(*idend - 1))
                return 0;

            // look for args or no args
            *sh_end = *idend;
            if (!match_end_sequence(S->user.macro_end, sh_end))
                *sh_end = -1;
            pos = *idend;
            match = match_sequence(S->user.macro_arg_start, &pos);

            if (idcheck) {
                *id = find_ident(C->buf + *idstart, *idend - *idstart);
                if (*id < 0)
                    match = 0;
            }
            *lg_end = -1;

            if (match) {
                *argc = 0;
                while (1) {
                    if (*argc >= max_arg_count)
                        bug("too many macro parameters");
                    argb[*argc] = pos;
                    k = 0;
                    while (1) { // look for macro_arg_end, macro_arg_sep, or comment-start
                        pos = iter_identifier_end(pos);
                        skip_possible_comments(&pos, cmtmode, 0);
                        if (get_char(pos) == 0)
                            return (*sh_end >= 0); // EOF
                        if (strchr(S->user.stackchar, get_char(pos)))
                            k++;
                        if (k) {
                            if (strchr(S->user.unstackchar, get_char(pos)))
                                k--;
                        } else {
                            arge[*argc] = pos;
                            if (match_sequence(S->user.macro_arg_sep, &pos)) {
                                match = 0;
                                break;
                            }
                            if (match_end_sequence(S->user.macro_arg_end, &pos)) {
                                match = 1;
                                break;
                            }
                        }
                        pos++; // nothing matched, go forward
                    }
                    (*argc)++;
                    if (match) { // no more args
                        *lg_end = pos;
                        break;
                    }
                }
            }
            return ((*lg_end >= 0) || (*sh_end >= 0));
        }

        int find_meta_args(int start, int *p1b, int *p1e, int *p2b, int *p2e, int *endm,
                int *argc, int *argb, int *arge) {
            int pos, k;
            int hyp_end1, hyp_end2;

            // look for macro_end or macro_arg_start
            pos = start;
            if (!match_sequence(S->meta.macro_arg_start, &pos)) {
                if (!match_end_sequence(S->meta.macro_end, &pos))
                    return -1;
                *endm = pos;
                return 0;
            }
            *p1b = pos;

            // special syntax for #define : 1st arg is a macro call
            if ((*argc)
                    && splice_possible_user(&pos, p1e, &hyp_end1, &hyp_end2, argb, arge,
                            argc, 0, nullptr, FLAG_META)) {
                *p1b = pos;
                if (hyp_end2 >= 0)
                    pos = hyp_end2;
                else {
                    pos = hyp_end1;
                    *argc = 0;
                }
                if (!match_sequence(S->meta.macro_arg_sep, &pos)) {
                    if (!match_end_sequence(S->meta.macro_arg_end, &pos))
                        bug(
                                "#define/#defeval requires an identifier or a single macro call");
                    *endm = pos;
                    return 1;
                }
            } else {
                *argc = 0;
                k = 0;
                while (1) { // look for macro_arg_end, macro_arg_sep, or comment-start
                    pos = iter_identifier_end(pos);
                    skip_possible_comments(&pos, FLAG_META, 0);
                    if (get_char(pos) != 0 && strchr(S->meta.stackchar, get_char(pos)))
                        k++;
                    if (k) {
                        if (get_char(pos) != 0
                                && strchr(S->meta.unstackchar, get_char(pos)))
                            k--;
                    } else {
                        *p1e = pos;
                        if (match_sequence(S->meta.macro_arg_sep, &pos))
                            break;
                        if (match_end_sequence(S->meta.macro_arg_end, &pos)) {
                            *endm = pos;
                            return 1;
                        }
                    }
                    if (get_char(pos) == 0)
                        bug("unfinished macro argument");
                    pos++; // nothing matched, go forward
                }
            }

            *p2b = pos;
            k = 0;
            while (1) { // look for macro_arg_end or comment-start
                pos = iter_identifier_end(pos);
                skip_possible_comments(&pos, FLAG_META, 0);
                if (get_char(pos) != 0 && strchr(S->meta.stackchar, get_char(pos)))
                    k++;
                if (k) {
                    if (get_char(pos) != 0 && strchr(S->meta.unstackchar, get_char(pos)))
                        k--;
                } else {
                    *p2e = pos;
                    if (match_end_sequence(S->meta.macro_arg_end, &pos))
                        break;
                }
                if (get_char(pos) == 0)
                    bug("unfinished macro");
                pos++; // nothing matched, go forward
            }
            *endm = pos;
            return 2;
        }

        char *process_text(const char *buf, int l, int ambience) {
            char *s;
            Input_Context *T;

            if (l == 0) {
                s = XX malloc(1);
                s[0] = 0;
                return s;
            }
            s = XX malloc(l + 2);
            s[0] = '\n';
            memcpy(s + 1, buf, l);
            s[l + 1] = 0;
            T = C;
            C = XX malloc(sizeof *C);
            C->out = XX malloc(sizeof *(C->out));
            C->read_stdin = 0;
            C->argc = T->argc;
            C->argv = T->argv;
            C->out->buf = XX malloc(80);
            C->out->len = 0;
            C->out->bufsize = 80;
            C->out->output_char = T->out->output_char;
            C->out->output_char_data = T->out->output_char_data;
            C->lineno = T->lineno;
            C->bufsize = l + 2;
            C->len = l + 1;
            C->buf = C->malloced_buf = s;
            C->eof = 0;
            C->namedargs = T->namedargs;
            C->in_comment = T->in_comment;
            C->ambience = ambience;
            C->may_have_args = T->may_have_args;

            process_context();
            outchar(0); // note that outchar works with the half-destroyed context !
            s = C->out->buf;
            free(C->out);
            free(C);
            C = T;
            return s;
        }

        void delete_macro(int i) {
            int j;
            nmacros--;
            free(macros[i].username);
            free(macros[i].macrotext);
            if (macros[i].argnames != nullptr ) {
                for (j = 0; j < macros[i].nnamedargs; j++)
                    free(macros[i].argnames[j]);
                free(macros[i].argnames);
                macros[i].argnames = nullptr;
            }
            free_comments(macros[i].define_specs);
            free(macros[i].define_specs);
            memcpy(macros + i, macros + nmacros, sizeof(Macro));
        }

        char *arithm_eval(int pos1, int pos2) {
            char *s, *t;
            int i;

            // first define the defined(...) operator
            i = find_ident("defined", strlen("defined"));
            if (i >= 0)
                warning("the defined(...) macro is already defined");
            else {
                newmacro("defined", strlen("defined"), 1);
                macros[nmacros].macrolen = 0;
                macros[nmacros].macrotext = XX malloc(1);
                macros[nmacros].macrotext[0] = 0;
                macros[nmacros].nnamedargs = -2; // trademark of the defined(...) macro
                nmacros++;
            }
            // process the text in a usual way
            s = process_text(C->buf + pos1, pos2 - pos1, FLAG_META);
            // undefine the defined(...) operator
            if (i < 0) {
                i = find_ident("defined", strlen("defined"));
                if ((i < 0) || (macros[i].nnamedargs != -2))
                    warning("the defined(...) macro was redefined in expression");
                else
                    delete_macro(i);
            }

            if (!do_arithm_eval(s, 0, strlen(s), &i))
                return s; // couldn't compute
            t = XX malloc(max_digit_count_of_number);
            sprintf(t, "%d", i);
            free(s);
            return t;
        }

        int comment_or_white(int start, int end, int cmtmode) {
            char c;

            while (start < end) {
                skip_possible_comments(&start, cmtmode, 1);
                if (start < end) {
                    c = get_char(start++);
                    if ((c != ' ') && (c != '\n') && (c != '\t'))
                        return 0;
                }
            }
            return 1;
        }

        char *remove_comments(int start, int end, int cmtmode) {
            char *s, *t;

            t = s = XX malloc(end - start + 1);
            while (start < end) {
                skip_possible_comments(&start, cmtmode, 1);
                if (start < end) {
                    *t = get_char(start++);
                    if ((*t == S->user.quotechar) && (start < end)) {
                        *(++t) = get_char(start++);
                    }
                    t++;
                }
            }
            *t = 0;
            return s;
        }

        void process_mode_command(int p1start, int p1end, int p2start, int p2end) {
            Specs *P;
            char *s, *p;
            char const* opt;
            int nargs, check_is_delim;
            char const* args[10]; // can't have more than 10 arguments

            whiteout(&p1start, &p1end);
            if ((p1start == p1end) || (identifier_end(p1start) != p1end))
                bug("invalid #mode syntax");
            if (p2start < 0)
                s = my_strdup("");
            else
                s = process_text(C->buf + p2start, p2end - p2start, FLAG_META);

            // argument parsing
            p = s;
            opt = nullptr;
            while (is_white(*p))
                p++;
            if ((*p != '"') && (*p != 0)) {
                opt = p;
                while ((*p != 0) && !is_white(*p))
                    p++;
                if (*p != 0) {
                    *(p++) = 0;
                    while (is_white(*p))
                        p++;
                }
            }
            nargs = 0;
            check_is_delim = !id_equal(C->buf + p1start, p1end - p1start, "charset");
            while (*p != 0) {
                if (nargs == 10)
                    bug("too many arguments in #mode command");
                if (*(p++) != '"')
                    bug("syntax error in #mode command (missing \" or trailing data)");
                args[nargs++] = p;
                p = str_nl(p, check_is_delim);
                while (is_white(*p))
                    p++;
            }

            if (id_equal(C->buf + p1start, p1end - p1start, "quote")) {
                if (opt || (nargs > 1))
                    bug("syntax error in #mode quote command");
                if (nargs == 0)
                    args[0] = "";
                S->stack_next->user.quotechar = args[0][0];
            } else if (id_equal(C->buf + p1start, p1end - p1start, "comment")) {
                if ((nargs < 2) || (nargs > 4))
                    bug("syntax error in #mode comment command");
                if (!opt)
                    opt = "ccc";
                if (nargs < 3)
                    args[2] = "";
                if (nargs < 4)
                    args[3] = "";
                add_comment(S->stack_next, opt, my_strdup(args[0]), my_strdup(args[1]),
                        args[2][0], args[3][0]);
            } else if (id_equal(C->buf + p1start, p1end - p1start, "string")) {
                if ((nargs < 2) || (nargs > 4))
                    bug("syntax error in #mode string command");
                if (!opt)
                    opt = "sss";
                if (nargs < 3)
                    args[2] = "";
                if (nargs < 4)
                    args[3] = "";
                add_comment(S->stack_next, opt, my_strdup(args[0]), my_strdup(args[1]),
                        args[2][0], args[3][0]);
            } else if (id_equal(C->buf + p1start, p1end - p1start, "save")
                    || id_equal(C->buf + p1start, p1end - p1start, "push")) {
                if ((opt != nullptr )||nargs)
                    bug("too many arguments to #mode save");
                P = clone_specs(S->stack_next);
                P->stack_next = S->stack_next;
                S->stack_next = P;
            } else if (id_equal(C->buf + p1start, p1end - p1start, "restore")
                    || id_equal(C->buf + p1start, p1end - p1start, "pop")) {
                if ((opt != nullptr )||nargs)
                    bug("too many arguments to #mode restore");
                P = S->stack_next->stack_next;
                if (P == nullptr )
                    bug("#mode restore without #mode save");
                free_comments(S->stack_next);
                free(S->stack_next);
                S->stack_next = P;
            } else if (id_equal(C->buf + p1start, p1end - p1start, "standard")) {
                bug("#mode standard not allowed");
            } else if (id_equal(C->buf + p1start, p1end - p1start, "user")) {
                if ((opt != nullptr )||(nargs!=9))bug("#mode user requires 9 arguments");
                S->stack_next->user.macro_start=my_strdup(args[0]);
                S->stack_next->user.macro_end=my_strdup(args[1]);
                S->stack_next->user.macro_arg_start=my_strdup(args[2]);
                S->stack_next->user.macro_arg_sep=my_strdup(args[3]);
                S->stack_next->user.macro_arg_end=my_strdup(args[4]);
                S->stack_next->user.stackchar=my_strdup(args[5]);
                S->stack_next->user.unstackchar=my_strdup(args[6]);
                S->stack_next->user.macro_arg_ref=my_strdup(args[7]);
                S->stack_next->user.quotechar=args[8][0];
            }
            else if (id_equal(C->buf+p1start,p1end-p1start,"meta")) {
                if ((opt!=nullptr)&&!nargs&&!strcmp(opt,"user"))
                S->stack_next->meta=S->stack_next->user;
                else {
                    if ((opt!=nullptr)||(nargs!=7)) bug("#mode meta requires 7 arguments");
                    S->stack_next->meta.macro_start=my_strdup(args[0]);
                    S->stack_next->meta.macro_end=my_strdup(args[1]);
                    S->stack_next->meta.macro_arg_start=my_strdup(args[2]);
                    S->stack_next->meta.macro_arg_sep=my_strdup(args[3]);
                    S->stack_next->meta.macro_arg_end=my_strdup(args[4]);
                    S->stack_next->meta.stackchar=my_strdup(args[5]);
                    S->stack_next->meta.unstackchar=my_strdup(args[6]);
                }
            }
            else if (id_equal(C->buf+p1start,p1end-p1start,"preservelf")) {
                bug("#mode preservelf not allowed");
            }
            else if (id_equal(C->buf+p1start,p1end-p1start,"nocomment")
                    ||id_equal(C->buf+p1start,p1end-p1start,"nostring")) {
                if ((opt!=nullptr)||(nargs>1))
                bug("syntax error in #mode nocomment/nostring");
                if (nargs==0) free_comments(S->stack_next);
                else delete_comment(S->stack_next,my_strdup(args[0]));
            }
            else if (id_equal(C->buf+p1start,p1end-p1start,"charset")) {
                if ((opt==nullptr)||(nargs!=1)) bug("syntax error in #mode charset");
                if (!my_strcasecmp(opt,"op"))
                S->stack_next->op_set=make_charset_subset((unsigned char *)args[0]);
                else if (!my_strcasecmp(opt,"par"))
                S->stack_next->ext_op_set=make_charset_subset((unsigned char *)args[0]);
                else if (!my_strcasecmp(opt,"id"))
                S->stack_next->id_set=make_charset_subset((unsigned char *)args[0]);
                else bug("unknown charset subset name in #mode charset");
            }
            else bug("unrecognized #mode command");

            free(s);
        }

        int parse_possible_meta() {
            int cklen, nameend;
            int id, expparams, nparam, i, j;
            int p1start{}, p1end{}, p2start{}, p2end{}, macend{};
            int argc, argb[max_arg_count], arge[max_arg_count];
            char *tmpbuf;

            cklen = 1;
            if (!match_start_sequence(S->meta.macro_start, &cklen))
                return -1;
            nameend = identifier_end(cklen);
            if (nameend && !get_char(nameend - 1))
                return -1;

            argc = 0; // for #define with named args
            if (id_equal(C->buf + cklen, nameend - cklen, "define")) // check identifier
            {
                id = 1;
                expparams = 2;
                argc = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "undef")) {
                id = 2;
                expparams = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "ifdef")) {
                id = 3;
                expparams = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "ifndef")) {
                id = 4;
                expparams = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "else")) {
                id = 5;
                expparams = 0;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "endif")) {
                id = 6;
                expparams = 0;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "include")) {
                bug("#include not allowed");
            } else if (id_equal(C->buf + cklen, nameend - cklen, "exec")) {
                bug("#exec not allowed");
            } else if (id_equal(C->buf + cklen, nameend - cklen, "defeval")) {
                id = 9;
                expparams = 2;
                argc = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "ifeq")) {
                id = 10;
                expparams = 2;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "ifneq")) {
                id = 11;
                expparams = 2;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "eval")) {
                id = 12;
                expparams = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "if")) {
                id = 13;
                expparams = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "mode")) {
                id = 14;
                expparams = 2;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "line")) {
                id = 15;
                expparams = 0;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "file")) {
                bug("#file not allowed");
            } else if (id_equal(C->buf + cklen, nameend - cklen, "elif")) {
                id = 17;
                expparams = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "error")) {
                id = 18;
                expparams = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "warning")) {
                id = 19;
                expparams = 1;
            } else if (id_equal(C->buf + cklen, nameend - cklen, "date")) {
                bug("#date not allowed");
            } else
                return -1;

            // #Syntax_Mode magic : define "..." to be C-style strings
            if (id == 14) {
                push_specs(S);
                delete_comment(S, my_strdup("\""));
                add_comment(S, "sss", my_strdup("\""), my_strdup("\""), '\\', '\n');
            }

            nparam = find_meta_args(nameend, &p1start, &p1end, &p2start, &p2end, &macend, &argc, argb, arge);
            if (nparam == -1)
                return -1;

            if ((nparam == 2) && is_whitesep(S->meta.macro_arg_sep))
                if (comment_or_white(p2start, p2end, FLAG_META))
                    nparam = 1;
            if ((nparam == 1) && is_whitesep(S->meta.macro_arg_start))
                if (comment_or_white(p1start, p1end, FLAG_META))
                    nparam = 0;
            if (expparams && !nparam)
                bug("missing argument in meta-macro");

            switch (id) {
            case 1: // DEFINE
                if (!commented[iflevel]) {
                    whiteout(&p1start, &p1end); // recall comments are not allowed here
                    if ((p1start == p1end) || (identifier_end(p1start) != p1end))
                        bug("#define requires an identifier (A-Z,a-z,0-9,_ only)");
                    // buf starts 1 char before the macro
                    i = find_ident(C->buf + p1start, p1end - p1start);
                    if (i >= 0)
                        delete_macro(i);
                    newmacro(C->buf + p1start, p1end - p1start, 1);
                    if (nparam == 1) {
                        p2end = p2start = p1end;
                    }
                    macros[nmacros].macrotext = remove_comments(p2start, p2end,
                            FLAG_META);
                    macros[nmacros].macrolen = strlen(macros[nmacros].macrotext);
                    macros[nmacros].defined_in_comment = C->in_comment;

                    if (argc) {
                        for (j = 0; j < argc; j++)
                            whiteout(argb + j, arge + j);
                        // define with one empty argument
                        if ((argc == 1) && (arge[0] == argb[0]))
                            argc = 0;
                        macros[nmacros].argnames = XX malloc((argc + 1) * sizeof(char *));
                        macros[nmacros].argnames[argc] = nullptr;
                    }
                    macros[nmacros].nnamedargs = argc;
                    for (j = 0; j < argc; j++) {
                        if ((argb[j] == arge[j]) || (identifier_end(argb[j]) != arge[j]))
                            bug(
                                    "#define with named args needs identifiers as arg names");
                        macros[nmacros].argnames[j] = XX malloc(arge[j] - argb[j] + 1);
                        memcpy(macros[nmacros].argnames[j], C->buf + argb[j],
                                arge[j] - argb[j]);
                        macros[nmacros].argnames[j][arge[j] - argb[j]] = 0;
                    }
                    lookup_arg_refs(nmacros++);
                }
                break;

            case 2: // UNDEF
                if (!commented[iflevel]) {
                    if (nparam == 2 && warning_level > 0)
                        warning("extra argument to #undef ignored");
                    whiteout(&p1start, &p1end);
                    if ((p1start == p1end) || (identifier_end(p1start) != p1end))
                        bug("#undef requires an identifier (A-Z,a-z,0-9,_ only)");
                    i = find_ident(C->buf + p1start, p1end - p1start);
                    if (i >= 0)
                        delete_macro(i);
                }
                break;

            case 3: // IFDEF
                iflevel++;
                if (iflevel == max_stack_depth)
                    bug("too many nested #ifdefs");
                commented[iflevel] = commented[iflevel - 1];

                if (!commented[iflevel]) {
                    if (nparam == 2 && warning_level > 0)
                        warning("extra argument to #ifdef ignored");
                    whiteout(&p1start, &p1end);
                    if ((p1start == p1end) || (identifier_end(p1start) != p1end))
                        bug("#ifdef requires an identifier (A-Z,a-z,0-9,_ only)");
                    i = find_ident(C->buf + p1start, p1end - p1start);
                    commented[iflevel] = (i == -1);
                }
                break;

            case 4: // IFNDEF
                iflevel++;
                if (iflevel == max_stack_depth)
                    bug("too many nested #ifdefs");
                commented[iflevel] = commented[iflevel - 1];
                if (!commented[iflevel]) {
                    if (nparam == 2 && warning_level > 0)
                        warning("extra argument to #ifndef ignored");
                    whiteout(&p1start, &p1end);
                    if ((p1start == p1end) || (identifier_end(p1start) != p1end))
                        bug("#ifndef requires an identifier (A-Z,a-z,0-9,_ only)");
                    i = find_ident(C->buf + p1start, p1end - p1start);
                    commented[iflevel] = (i != -1);
                }
                break;

            case 5: // ELSE
                if (!commented[iflevel] && (nparam > 0) && warning_level > 0)
                    warning("extra argument to #else ignored");
                if (iflevel == 0)
                    bug("#else without #if");
                if (!commented[iflevel - 1] && commented[iflevel] != 2)
                    commented[iflevel] = !commented[iflevel];
                break;

            case 6: // ENDIF
                if (!commented[iflevel] && (nparam > 0) && warning_level > 0)
                    warning("extra argument to #endif ignored");
                if (iflevel == 0)
                    bug("#endif without #if");
                iflevel--;
                break;

            case 7: // INCLUDE
                bug("internal meta-macro identification error: #include not allowed");

            case 8: // EXEC
                bug("internal meta-macro identification error: #exec not allowed");

            case 9: // DEFEVAL
                if (!commented[iflevel]) {
                    whiteout(&p1start, &p1end);
                    if ((p1start == p1end) || (identifier_end(p1start) != p1end))
                        bug("#defeval requires an identifier (A-Z,a-z,0-9,_ only)");
                    tmpbuf = process_text(C->buf + p2start, p2end - p2start, FLAG_META);
                    i = find_ident(C->buf + p1start, p1end - p1start);
                    if (i >= 0)
                        delete_macro(i);
                    newmacro(C->buf + p1start, p1end - p1start, 1);
                    if (nparam == 1) {
                        p2end = p2start = p1end;
                    }
                    macros[nmacros].macrotext = tmpbuf;
                    macros[nmacros].macrolen = strlen(macros[nmacros].macrotext);
                    macros[nmacros].defined_in_comment = C->in_comment;

                    if (argc) {
                        for (j = 0; j < argc; j++)
                            whiteout(argb + j, arge + j);
                        // define with one empty argument
                        if ((argc == 1) && (arge[0] == argb[0]))
                            argc = 0;
                        macros[nmacros].argnames = XX malloc((argc + 1) * sizeof(char *));
                        macros[nmacros].argnames[argc] = nullptr;
                    }
                    macros[nmacros].nnamedargs = argc;
                    for (j = 0; j < argc; j++) {
                        if ((argb[j] == arge[j]) || (identifier_end(argb[j]) != arge[j]))
                            bug(
                                    "#defeval with named args needs identifiers as arg names");
                        macros[nmacros].argnames[j] = XX malloc(arge[j] - argb[j] + 1);
                        memcpy(macros[nmacros].argnames[j], C->buf + argb[j],
                                arge[j] - argb[j]);
                        macros[nmacros].argnames[j][arge[j] - argb[j]] = 0;
                    }
                    lookup_arg_refs(nmacros++);
                }
                break;

            case 10: // IFEQ
                iflevel++;
                if (iflevel == max_stack_depth)
                    bug("too many nested #ifeqs");
                commented[iflevel] = commented[iflevel - 1];
                if (!commented[iflevel]) {
                    char *s, *t;
                    if (nparam != 2)
                        bug("#ifeq requires two arguments");
                    s = process_text(C->buf + p1start, p1end - p1start, FLAG_META);
                    t = process_text(C->buf + p2start, p2end - p2start, FLAG_META);
                    commented[iflevel] = (nowhite_strcmp(s, t) != 0);
                    free(s);
                    free(t);
                }
                break;

            case 11: // IFNEQ
                iflevel++;
                if (iflevel == max_stack_depth)
                    bug("too many nested #ifeqs");
                commented[iflevel] = commented[iflevel - 1];
                if (!commented[iflevel]) {
                    char *s, *t;
                    if (nparam != 2)
                        bug("#ifneq requires two arguments");
                    s = process_text(C->buf + p1start, p1end - p1start, FLAG_META);
                    t = process_text(C->buf + p2start, p2end - p2start, FLAG_META);
                    commented[iflevel] = (nowhite_strcmp(s, t) == 0);
                    free(s);
                    free(t);
                }
                break;

            case 12: // EVAL
                if (!commented[iflevel]) {
                    char *s, *t;
                    if (nparam == 2)
                        p1end = p2end; // we really want it all !
                    s = arithm_eval(p1start, p1end);
                    for (t = s; *t; t++)
                        outchar(*t);
                    free(s);
                }
                break;

            case 13: // IF
                iflevel++;
                if (iflevel == max_stack_depth)
                    bug("too many nested #ifs");
                commented[iflevel] = commented[iflevel - 1];
                if (!commented[iflevel]) {
                    char *s;
                    if (nparam == 2)
                        p1end = p2end; // we really want it all !
                    s = arithm_eval(p1start, p1end);
                    commented[iflevel] = ((s[0] == '0') && (s[1] == 0));
                    free(s);
                }
                break;

            case 14: // MODE
                if (nparam == 1)
                    p2start = -1;
                if (!commented[iflevel])
                    process_mode_command(p1start, p1end, p2start, p2end);
                pop_specs();
                break;

            case 15: { // LINE
                char buf[max_digit_count_of_number];
                sprintf(buf, "%d", C->lineno);
                sendout(buf, strlen(buf), 0);
                break;
            }

            case 16: // FILE
                bug("internal meta-macro identification error: #file not allowed");

            case 17: // ELIF
                if (iflevel == 0)
                    bug("#elif without #if");
                if (!commented[iflevel - 1]) {
                    if (commented[iflevel] != 1)
                        commented[iflevel] = 2;
                    else {
                        char *s;
                        commented[iflevel] = 0;
                        if (nparam == 2)
                            p1end = p2end; // we really want it all !
                        s = arithm_eval(p1start, p1end);
                        commented[iflevel] = ((s[0] == '0') && (s[1] == 0));
                        free(s);
                    }
                }
                break;

            case 18: // ERROR
                if (!commented[iflevel])
                    bug(
                            process_text(C->buf + p1start,
                                    (nparam == 2 ? p2end : p1end) - p1start,
                                    FLAG_META));
                break;

            case 19: // WARNING
                if (!commented[iflevel]) {
                    char *s;
                    s = process_text(C->buf + p1start,
                            (nparam == 2 ? p2end : p1end) - p1start, FLAG_META);
                    warning(s);
                    free(s);
                }
                break;

            case 20: // DATE
                bug("internal meta-macro identification error: #date not allowed");

            default:
                bug("internal meta-macro identification error");
            }
            shift_in(macend);
            return 0;
        }

        int parse_possible_user() {
            int idstart, idend, sh_end, lg_end, macend;
            int argc, id, i, l;
            char *argv[max_arg_count];
            int argb[max_arg_count], arge[max_arg_count];
            Input_Context *T;

            idstart = 1;
            id = 0;
            if (!splice_possible_user(&idstart, &idend, &sh_end, &lg_end, argb, arge,
                    &argc, 1, &id, FLAG_USER))
                return -1;
            if ((sh_end >= 0) && (C->namedargs != nullptr )) {
                i = find_named_arg(C->buf + idstart, idend - idstart);
                if (i >= 0) {
                    if (i < C->argc)
                        sendout(C->argv[i], strlen(C->argv[i]), 0);
                    shift_in(sh_end);
                    return 0;
                }
            }

            if (id < 0)
                return -1;
            if (lg_end >= 0)
                macend = lg_end;
            else {
                macend = sh_end;
                argc = 0;
            }

            if (macros[id].nnamedargs == -2) { // defined(...) macro for arithmetic
                char *s, *t;
                if (argc != 1)
                    return -1;
                s = remove_comments(argb[0], arge[0], FLAG_USER);
                t = s + strlen(s) - 1;
                if (*s != 0)
                    while ((t != s) && is_white(*t))
                        *(t--) = 0;
                t = s;
                while (is_white(*t))
                    t++;
                if (find_ident(t, strlen(t)) >= 0)
                    outchar('1');
                else
                    outchar('0');
                free(s);
                shift_in(macend);
                return 0;
            }
            if (!macros[id].macrotext[0]) { // the empty macro
                shift_in(macend);
                return 0;
            }

            for (i = 0; i < argc; i++)
                argv[i] = process_text(C->buf + argb[i], arge[i] - argb[i], FLAG_USER);
            // process macro text
            T = C;
            C = XX malloc(sizeof *C);
            C->out = T->out;
            C->read_stdin = 0;
            C->argc = argc;
            C->argv = argv;
            C->lineno = T->lineno;
            C->may_have_args = 1;
            if ((macros[id].nnamedargs == -1) && (lg_end >= 0)
                    && (macros[id].define_specs->user.macro_end[0] == 0)) {
                // build an aliased macro call
                l = strlen(macros[id].macrotext) + 2
                        + strlen(macros[id].define_specs->user.macro_arg_start)
                        + strlen(macros[id].define_specs->user.macro_arg_end)
                        + (argc - 1) * strlen(macros[id].define_specs->user.macro_arg_sep);
                for (i = 0; i < argc; i++)
                    l += strlen(argv[i]);
                C->buf = C->malloced_buf = XX malloc(l);
                l = strlen(macros[id].macrotext) + 1;
                C->buf[0] = '\n';
                strcpy(C->buf + 1, macros[id].macrotext);
                while ((l > 1) && is_white(C->buf[l - 1]))
                    l--;
                strcpy(C->buf + l, macros[id].define_specs->user.macro_arg_start);
                for (i = 0; i < argc; i++) {
                    if (i > 0)
                        strcat(C->buf, macros[id].define_specs->user.macro_arg_sep);
                    strcat(C->buf, argv[i]);
                }
                strcat(C->buf, macros[id].define_specs->user.macro_arg_end);
                C->may_have_args = 0;
            } else {
                C->buf = C->malloced_buf = XX malloc(strlen(macros[id].macrotext) + 2);
                C->buf[0] = '\n';
                strcpy(C->buf + 1, macros[id].macrotext);
            }
            C->len = strlen(C->buf);
            C->bufsize = C->len + 1;
            C->eof = 0;
            C->namedargs = macros[id].argnames;
            C->in_comment = macros[id].defined_in_comment;
            C->ambience = FLAG_META;
            push_specs(macros[id].define_specs);
            process_context();
            pop_specs();
            free(C);
            C = T;

            for (i = 0; i < argc; i++)
                free(argv[i]);
            shift_in(macend);
            return 0;
        }

        void parse_text() {
            int l, cs, ce;
            char c, *s;
            Comment *p;

            // look for comments first
            if (!C->in_comment) {
                cs = 1;
                for (p = S->comments; p != nullptr ; p = p->next)
                    if (!(p->flags[C->ambience] & FLAG_IGNORE))
                        if (match_start_sequence(p->start, &cs)) {
                            l = ce = find_comment_end(p->end, p->quote, p->warn, cs,
                                    p->flags[C->ambience]);
                            match_end_sequence(p->end, &l);
                            if (p->flags[C->ambience] & OUTPUT_DELIM)
                                sendout(C->buf + 1, cs - 1, 0);
                            if (p->flags[C->ambience] & PARSE_MACROS) {
                                C->in_comment = 1;
                                s = process_text(C->buf + cs, ce - cs, C->ambience);
                                if (p->flags[C->ambience] & OUTPUT_TEXT)
                                    sendout(s, strlen(s), 0);
                                C->in_comment = 0;
                                free(s);
                            } else if (p->flags[C->ambience] & OUTPUT_TEXT)
                                sendout(C->buf + cs, ce - cs, 0);
                            if (p->flags[C->ambience] & OUTPUT_DELIM)
                                sendout(C->buf + ce, l - ce, 0);
                            shift_in(l);
                            return;
                        }
            }

            if (parse_possible_meta() >= 0)
                return;
            if (parse_possible_user() >= 0)
                return;

            l = 1;
            // if matching numbered macro argument and inside a macro
            if (match_sequence(S->user.macro_arg_ref, &l) && C->may_have_args) {
                // process macro arguments referenced as #1,#2,...
                c = get_char(l);
                if ((c >= '1') && (c <= '9')) {
                    c = c - '1';
                    if (c < C->argc)
                        sendout(C->argv[(int) c], strlen(C->argv[(int) c]), 0);
                    shift_in(l + 1);
                    return;
                }
            }

            l = identifier_end(1);
            if (l == 1)
                l = 2;
            sendout(C->buf + 1, l - 1, 1);
            shift_in(l);
        }

        void process_context() {
            if (C->len == 0) {
                C->buf[0] = '\n';
                C->len++;
            }
            while (!C->eof)
                parse_text();
            free(C->malloced_buf);
            C->malloced_buf = nullptr;
        }
    };
}

namespace libgpp
{
    auto preprocess_stdin_to_stdout() -> void
    {
        Preprocessor pp;
        pp.preprocess_stdin_to_stdout();
    }

    auto preprocess_to_buffer(std::string const& source, std::string& output) -> void
    {
        Preprocessor pp;
        return pp.preprocess_to_buffer(source, output);
    }

    auto preprocess(std::string const& source) -> std::string
    {
        std::string result;
        preprocess_to_buffer(source, result);
        return result;
    }
}

