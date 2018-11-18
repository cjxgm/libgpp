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

