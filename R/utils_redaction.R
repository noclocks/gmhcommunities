
list_redact <- function(x, names, case_sensitive = TRUE) {

  if (case_sensitive) {
    i <- match(names, names(x))
  } else {
    i <- match(tolower(names), tolower(names(x)))
  }

  x[i] <- cli::col_grey("<REDACTED>")
  x

}

headers_redact <- function(x, redact = TRUE, to_redact = NULL) {
  if (!redact) { return(x) }
  else {
    to_redact <- union(attr(x, "redact"), to_redact)
    attr(x, "redact") <- NULL
    list_redact(x, to_redact, case_sensitive = FALSE)
  }
}

headers_flatten <- function(x) {
  set_names(as.character(unlist(x, use.names = FALSE)), rep(names(x), lengths(x)))
}
