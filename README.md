RcppRoll
========

[![CRAN status](https://www.r-pkg.org/badges/version/RcppRoll)](https://cran.r-project.org/package=RcppRoll)
[![R-CMD-check](https://github.com/kevinushey/RcppRoll/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kevinushey/RcppRoll/actions/workflows/R-CMD-check.yaml)

This package provides windowed-versions of commonly-used mathematical
and statistical functions.

Install the latest release from CRAN with:

    install.packages("RcppRoll")

Or, install the development version with:

    install_github("kevinushey/RcppRoll")

## Parallelism

When compiled with OpenMP support, RcppRoll parallelizes its rolling
computations across threads. Results do not depend on the number of threads
used. By default, the thread count is chosen by the OpenMP runtime
(typically controllable through the `OMP_NUM_THREADS` environment
variable); pin it with `options(RcppRoll.threads = <n>)`, or set it to 1 to
disable parallelism. Check what your installation supports with:

    RcppRoll::roll_threads()

which reports the number of threads to be used, or `NA` if the package was
compiled without OpenMP support. The package reports this on attach as
well; use `options(RcppRoll.quiet = TRUE)` to suppress the startup message.

The toolchains normally used on Linux and Windows support OpenMP out of the
box, so installations from sources just work. Apple's macOS toolchain ships
without OpenMP support; the R project documents how to enable it at
<https://mac.r-project.org/openmp/>, which provides OpenMP runtimes matching
Apple's compilers and recommends adding

    CPPFLAGS += -Xclang -fopenmp
    LDFLAGS += -lomp

to `~/.R/Makevars`. Alternatively, with Homebrew's OpenMP runtime
(`brew install libomp`), add the following line to `~/.R/Makevars` instead:

    SHLIB_OPENMP_CXXFLAGS = -Xclang -fopenmp -I/opt/homebrew/opt/libomp/include -L/opt/homebrew/opt/libomp/lib -lomp

Either way, reinstall the package from sources afterwards, and confirm the
result with `roll_threads()`.
