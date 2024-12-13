read_utf8 <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

render_template <- function(template_path, data = list()) {
  strsplit(whisker::whisker.render(read_utf8(template_path), data), "\n")[[1]]
}

use_template <- function(template, save_as, data = list(), ignore = FALSE, open = FALSE) {
  content <- render_template(template, data)
  new <- usethis::write_over(save_as, content)
  if (ignore) {
    usethis::use_build_ignore(save_as)
  }
  if (open && new) {
    file.edit(save_as)
  }
  invisible(new)
}

# use_template("dev/templates/mod_template.R", "dev/mod_test.R", data = list(name = "test"), open = TRUE)

use_module <- function(name) {
  use_template("dev/templates/module.template.R", paste0("R/mod_", name, ".R"), data = list(name = name), open = TRUE)
}
