#include "libgpp.hpp"
#include <stdexcept>
#include <cstdio>
#include <cstdlib>
#include <cstring>

namespace
{
    #define PACKAGE_STRING "GPP v2.26-60260df libgpp v1.1.1"

    void display_version() {
        fprintf(stderr, PACKAGE_STRING "\n");
        fprintf(stderr, "Copyright (C) 1996-2001 Denis Auroux\n");
        fprintf(stderr, "Copyright (C) 2003-2017 Tristan Miller\n");
        fprintf(stderr, "Copyright (C) 2018 Giumo Clanjor\n");
        fprintf(stderr,
            "This is free software; see the source for copying conditions.  There is NO\n"
            "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
        );
    }

    void usage() {
        fprintf(stderr, "Usage:\n");
        fprintf(stderr, "  gpp < FILE\n");
        fprintf(stderr, "  echo MACROS | gpp\n");
        fprintf(stderr, "\n");
        fprintf(stderr, "      macro syntax overview:    #define x y           macro(arg,...)\n");
        fprintf(stderr, "\n");
        fprintf(stderr, " --version  : display version information and exit\n");
        fprintf(stderr, " -h, --help : display this message and exit\n\n");
    }
}

int main(int argc, char *argv[]) try
{
    (void) argc;

    for (auto arg = argv + 1; *arg; arg++) {
        if (strcmp(*arg, "--help") == 0 || strcmp(*arg, "-h") == 0) {
            usage();
            return EXIT_SUCCESS;
        }

        if (strcmp(*arg, "--version") == 0) {
            display_version();
            return EXIT_SUCCESS;
        }

        usage();
        return EXIT_FAILURE;
    }

    libgpp::preprocess_stdin_to_stdout();
}
catch (std::exception const& e) {
    fprintf(stderr, "\e[1;31m ERROR \e[0m %s\n", e.what());
    return EXIT_FAILURE;
}
catch (...) {
    fprintf(stderr, "\e[1;31m UNKNOWN ERROR \e[0m\n");
    return EXIT_FAILURE;
}

