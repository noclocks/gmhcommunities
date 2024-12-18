
#  ------------------------------------------------------------------------
#
# Title : State Inspector
#    By : Jimmy Briggs
#  Date : 2024-12-13
#
#  ------------------------------------------------------------------------

# Conditionally set the state inspector if in a testing environment
if (Sys.getenv("R_TESTS") != "") {
  if (getRversion() >= "4.0.0") {
    testthat::set_state_inspector(function() {
      list(
        attached    = search(),
        connections = getAllConnections(),
        cwd         = getwd(),
        envvars     = Sys.getenv(),
        handlers    = globalCallingHandlers(),
        libpaths    = .libPaths(),
        locale      = Sys.getlocale(),
        options     = options(),
        par         = par(),
        packages    = .packages(all.available = TRUE),
        sink        = sink.number(),
        timezone    = Sys.timezone(),
        NULL
      )
    })
  }
}


# testthat::set_state_inspector(
#   function() {
#     list(
#       attached = search(),
#       num_conns = nrow(showConnections()),
#       conns = getAllConnections(),
#       handlers = if (getRversion() >= "4.0.0") {
#         globalCallingHandlers()
#       } else {
#         Sys.getenv("error")
#       },
#       cwd = getwd(),
#       envvars = Sys.getenv(),
#       libpaths = .libPaths(),
#       locale = Sys.getlocale(),
#       tz = Sys.timezone(),
#       par = par(),
#       sink = sink.number(),
#       options = .Options,
#       packages = .packages(all.available = TRUE),
#       tempfiles = list.files(tempdir(), full.names = TRUE),
#       NULL
#     )
#   }
# )
