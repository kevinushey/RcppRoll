# Report the OpenMP state once, on attach. Suppressible in the usual way
# through suppressPackageStartupMessages(), or universally with
# options(RcppRoll.quiet = TRUE).
.onAttach <- function(libname, pkgname) {

  if (isTRUE(getOption("RcppRoll.quiet")))
    return(invisible())

  threads <- roll_threads()
  if (is.na(threads)) {
    packageStartupMessage(
      "RcppRoll was compiled without OpenMP support; rolling computations ",
      "will run on a single thread. See the README for enabling OpenMP."
    )
  } else {
    packageStartupMessage(sprintf(
      "RcppRoll: using up to %d thread%s; control with options(RcppRoll.threads = <n>)",
      threads, if (threads == 1L) "" else "s"
    ))
  }

}
