# Carry the last non-missing value forward. Unexported, but kept for callers
# reaching in with ':::'.
na_locf <- function(x) {
  .Call(C_na_locf, x)
}
