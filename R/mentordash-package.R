#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang :=
## usethis namespace: end
NULL

# The "the" environment is a trick borrowed from the Posit open source team.
# They set up an internal package environment, "the", so they can refer to
# things within the package as "the$thing_i_want". It's intended to improve
# readability.
the <- rlang::new_environment()
