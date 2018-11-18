libgpp - GPP Macro Preprocessor as a library
============================================

`libgpp` forks off `GPP` with git commit `60260df` from
[upstream](https://github.com/logological/gpp).

Changes
-------

### v1.0.0

This is the first release. Differences to GPP `v2.26-60260df`:

- Removed `config.h`. Uses only ANSI C. No platform-specific code/library any more.
  No POSIX support any more.

- Removed deprecated command-line options.

- Removed all IO (except stdio): `#include`, `#exec`, `#file`, `#date` all gone.

- Removed syntax customization through command-line. You can still customize syntax via `#mode` macro.

- Removed mode switching. There is only 1 mode now. The only mode is now
  just the original default mode but also recognizes C strings and comments.
  This is equivalent to:

        #mode user "" "" "(" "," ")" "(" ")" "#" "\\"
        #mode meta "#" "\n" " " " " "\n" "(" ")"
        #mode comment "/*" "*/"
        #mode comment "//" "\n"
        #mode comment "\\\003" ""
        #mode string "\"" "\"" "\\"
        #mode string "'" "'" "\\"

- Removed "Preserve LF". `\n` will never be preserved.

### v1.1.0

- Convert to C++14 compilers.

  - Removed `typedef`s and `struct` prefixes.
  - Use `std::uint32_t` instead of `unsigned long`.
  - Convert to a consistent naming style.
  - Convert `function(void)` to `function()`.

