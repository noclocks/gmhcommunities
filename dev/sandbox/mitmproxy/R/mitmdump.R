

start_mitmdump <- function(outfile) {
  options <- list(
    listen_host = "127.0.0.1",
    listen_port = 8080,
    save_stream_file = outfile
  )
  master <- mitmdump$DumpMaster(options)
  master$run()
}
