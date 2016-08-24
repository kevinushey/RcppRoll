# list the functions we need to generate wrappers for
functions <- c("mean", "median", "min", "max", "prod", "sum", "sd", "var")

cpp_template_body <- readLines("internal/template-body.cpp")
cpp_body <- unlist(lapply(functions, function(x) {
  gsub("%s", x, cpp_template_body, fixed = TRUE)
}))

has_flag <- function(content, flag) {
  any(grepl(flag, content, fixed = TRUE))
}

content <- readLines("src/RcppRoll.cpp")
begin_flag <- "// Begin auto-generated exports (internal/make-exports.R)"
end_flag <- "// End auto-generated exports (internal/make-exports.R)"
if (has_flag(content, begin_flag) && has_flag(content, end_flag)) {
  content <- content[-c(`:`(
    grep(begin_flag, content, fixed = TRUE),
    grep(end_flag, content, fixed = TRUE)
  ))]
}

output <- c(content, begin_flag, "", cpp_body, end_flag)
cat(output, file = "src/RcppRoll.cpp", sep = "\n")

# Write R wrappers
r_template_header <- readLines("internal/template-header.R")
r_template_body   <- readLines("internal/template-body.R")

r_body <- unlist(lapply(functions, function(x) {
  gsub("%s", x, r_template_body, fixed = TRUE)
}))

cat(c(r_template_header, r_body), file = "R/RcppRoll.R", sep = "\n")
