init_venv <- function() {

  if (!reticulate::virtualenv_exists()) {
    fs::dir_create(".venv")
    reticulate::use_virtualenv(fs::path(getwd(), ".venv"))
    reticulate::py_install(c("mitmproxy", "mitmproxy2swagger"))
  }

}
