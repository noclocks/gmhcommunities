
#  ------------------------------------------------------------------------
#
# Title : Retry Tests Helper
#    By : Jimmy Briggs
#  Date : 2024-12-10
#
#  ------------------------------------------------------------------------

require(rlang)
require(cli)

retry_test <- function(code, retries = 1) {

  code <- rlang::enquo(code)
  i <- 1

  while (i <= retries) {
    tryCatch(
      {
        return(eval(rlang::get_expr(code), rlang::get_env(code)))
        break
      },
      expectation_failure = function(cnd) NULL
    )
    cli::cli_inform(c(i = "Retry {i}"))
    i <- i + 1
  }

  eval(rlang::get_expr(code), rlang::get_env(code))
}
