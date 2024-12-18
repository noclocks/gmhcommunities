init_venv <- function() {

  if (!reticulate::virtualenv_exists()) {
    fs::dir_create(".venv")
    processx::run(
      command = Sys.which("python"),
      args = c("-m", "venv", ".venv"),
      echo = TRUE
    )
    reticulate::use_virtualenv(fs::path(getwd(), ".venv"))
  }

}

install_mitmproxy <- function() {
  reticulate::py_install(c("mitmproxy", "mitmproxy2swagger"))
}

run_mitmweb <- function(stream_file, port = 8080, web_port = 8081) {

  args <- c(
    "--listen-port",
    port,
    "--web-port",
    web_port,
    "--save-stream-file",
    paste0("+", stream_file)
  )

  process <- processx::process$new(
    command = "mitmweb",
    args = args,
    stdout = "|",
    stderr = "|"
  )

  # Wait a bit to ensure the process has started
  Sys.sleep(2)

  if (process$is_alive()) {
    cli::cli_alert_success(
      c(
        "{.code mitmweb} started successfully on port: {.field {port}}\n",
        "Web Interface running on port: {.field {web_port}}\n",
        "Streaming Flows to File: {.path {stream_file}}\n"
      )
    )
  } else {
    cli::cli_abort("Failed to start mitmweb")
  }

  return(process)
}

stop_mitmweb <- function(process) {
  process$kill()
  if (!process$is_alive()) {
    cli::cli_alert_success("{.code mitmweb} stopped successfully")
  } else {
    cli::cli_abort("Failed to stop mitmweb")
  }
}
