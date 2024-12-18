
#  ------------------------------------------------------------------------
#
# Title : GitHub Utilities
#    By : Jimmy Briggs
#  Date : 2024-12-09
#
#  ------------------------------------------------------------------------


# github dates ------------------------------------------------------------

#' GitHub Repository Last Modified Date
#'
#' @description
#' Get the last modified date of a GitHub repository.
#'
#' @param repo A character string of the GitHub repository in the format 'user/repo'.
#' @param format A character string specifying the format of the date to return.
#'
#' @returns A character string of the last modified date of the GitHub repository.
#'
#' @importFrom cli cli_abort
#'
#' @examples
#' get_gh_repo_last_modified("noclocks/gmhleasr")
get_gh_repo_last_modified <- function(repo, format = "%Y-%m-%d") {

  if (!grepl("/", repo)) {
    cli::cli_abort(
      "Invalid {.arg repo} argument. Please provide the repo in the format 'user/repo'."
    )
  }

  base_url <- "https://api.github.com/repos/"
  str <- paste0(base_url, repo)
  read <- readLines(str, warn = FALSE)
  pat <- unlist(gregexpr("updated_at", read)) + 13
  format(
    as.POSIXct(
      substr(read, start = pat, stop = pat + 18),
      format = "%Y-%m-%dT%H:%M:%S"
    ),
    format
  )

}
