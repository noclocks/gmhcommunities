.entrata_config_env <- new.env(parent = emptyenv())

initialize_logging <- function() {

  logger::log_threshold(logger::INFO, namespace = "gmhcommunities")

  logger::log_layout(
    logger::layout_glue_generator(
      "{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}"
    ),
    namespace = "gmhcommunities"
  )

  logger::log_formatter(
    logger::formatter_glue,
    namespace = "gmhcommunities"
  )

  logger::log_appender(
    appender = logger::appender_stdout,
    namespace = "gmhcommunities"
  )

}

initialize_pkg_options <- function() {

  options::define_options(
    # database
    "Stores a database pooled connection object.",
    db.conn = NULL,
    "Database user integer ID to use for query logging.",
    db.user_id = 1,
    # entrata
    "Entrata API Base URL",
    entrata.base_url = get_entrata_config("base_url"),
    "Entrata API Username",
    entrata.username = get_entrata_config("username"),
    "Entrata API Password",
    entrata.password = get_entrata_config("password"),
    "Entrata API User Agent",
    entrata.user_agent = NULL,
    "Entrata API Timeout",
    entrata.timeout = 30,
    "Entrata API Retry On Status Codes",
    entrata.retry_on = c(429, 500, 502, 503, 504),
    "Entrata API Retry Max Attempts",
    entrata.max_retries = 3,
    "Entrata Request ID",
    entrata.request_id = NULL,
    "Entrata Verbosity",
    entrata.verbosity = NULL,
    "Entrata Default Headers",
    entrata.default_headers = NULL,
    "Entrata API Proxy",
    entrata.proxy = NULL
  )

}


# entrata -----------------------------------------------------------------

#' Entrata Configuration Manager Class
#'
#' `r lifecycle::badge('experimental')`
#'
#' @description
#' `EntrataConfig` is an experimental [R6::R6Class()] that provides a high-level
#' abstraction interface for interacting with and managing the various components
#' associated with configuring the Entrata API client.
#'
#' @section Priority:
#'
#' 1. Directly supplied arguments to `entrata_config()` function.
#' 2. Configured values from a dedicated `config.yml` file.
#' 3. Environment variables (i.e. `Sys.getenv("ENTRATA_BASE_URL")`).
#' 4. R Options (i.e. `getOption("entrata.user_agent")`).
#' 5. Default values stored in an internal configuration R6 object or environment.
#'
#' @format
#' An R6 object of class `EntrataConfig`.
#'
#' @field base_url The base URL for the Entrata API.
#' @field username The username to use for authenticating with the Entrata API.
#' @field user_agent The user agent to use for the Entrata API client.
#' @field request_id The request ID to use for the Entrata API client.
#' @field request_headers A list of request headers to use for the Entrata API client.
#' @field proxy A list of proxy settings to use for the Entrata API client.
#' @field timeout The timeout value to use for the Entrata API client.
#' @field throttling A list of throttling settings to use for the Entrata API client.
#' @field logging A list of logging settings to use for the Entrata API client.
#' @field retry_policy A list of retry policy settings to use for the Entrata API client.
#' @field caching A list of caching settings to use for the Entrata API client.
#' @field mocking A list of mocking settings to use for the Entrata API client.
#' @field redaction A list of redaction settings to use for the Entrata API client.
#' @field parallel A list of parallel processing settings to use for the Entrata API client.
#' @field validation A list of validation settings to use for the Entrata API client.
#' @field pagination A list of pagination settings to use for the Entrata API client.
#'
#' @importFrom R6 R6Class
#' @importFrom cli cli_abort
#' @importFrom rlang caller_env
EntrataConfig <- R6::R6Class(
  "EntrataConfig",
  public = list(
    base_url = NULL,
    username = NULL,
    user_agent = NULL,
    request = NULL,
    logging = NULL,
    retry_policy = NULL,
    caching = NULL,
    mocking = NULL,
    redaction = NULL,
    parallel = NULL,
    validation = NULL,
    pagination = NULL,
    request_id = NULL,
    request_headers = NULL,
    proxy = NULL,
    timeout = NULL,
    throttling = NULL,

    initialize = function(config = list()) {

      defaults <- list(
        base_url = "https://gmhcommunities.entrata.com/api/v1",
        username = get_entrata_config("username"),
        user_agent = "gmhcommunities/0.0.0.9000 (https://github.com/noclocks/gmhcommunities)",
        request_id = as.integer(Sys.time()),
        property_id = NULL,
        report_name = "pre_lease",
        leasing_period = NULL,
        request_headers = list(
          "Accept" = "application/json",
          "Content-Type" = "application/json"
        ),
        proxy = list(
          url = NULL,
          port = NULL,
          username = NULL,
          password = NULL
        ),
        timeout = 60,
        throttling = list(
          rate = NULL,
          realm = NULL
        ),
        caching = list(
          cache_dir = "cache/",
          cache_policy = "lru",
          cache_ttl = 3600,
          cache_on_error = FALSE
        ),
        mocking = list(
          enabled = FALSE,
          mocks_dir = "tests/testthat/mocks",
          mocks_type = "json"
        ),
        redaction = list(
          enabled = FALSE,
          redact_headers = c("Authorization"),
          redact_cookies = TRUE
        ),
        parallel = list(
          enabled = FALSE,
          workers = 2,
          pool = NULL,
          on_error = "stop"
        ),
        validation = list(
          endpoint = TRUE,
          method = TRUE,
          params = TRUE,
          schema = TRUE
        ),
        pagination = list(
          page_size = 100,
          max_pages = 1000
        )
      )

      # Merge user config with defaults
      cfg <- modifyList(defaults, config)

      # Assign public fields
      self$base_url <- cfg$base_url
      self$username <- cfg$username
      # password is stored privately
      private$password <- cfg$password %||% get_entrata_config("password")
      self$user_agent <- cfg$user_agent
      self$request_id <- cfg$request_id
      self$request_headers <- cfg$request_headers
      self$proxy <- cfg$proxy
      self$timeout <- cfg$timeout
      self$throttling <- cfg$throttling
      self$logging <- LoggingConfig$new(cfg$logging %||% list())
      self$retry_policy <- RetryPolicyConfig$new(cfg$retry_policy %||% list())
      self$caching <- cfg$caching
      self$mocking <- cfg$mocking
      self$redaction <- cfg$redaction
      self$parallel <- cfg$parallel
      self$validation <- cfg$validation
      self$pagination <- cfg$pagination

      self$validate()

      return(invisible(self))

    },

    validate = function(call = rlang::caller_env()) {

      # validate base_url
      self_base_url <- self$base_url

      if (!is.character(self_base_url) || !grepl("^https?://", self_base_url)) {
        cli::cli_abort("Invalid {.field base_url}. Must be a valid URL", call = call)
      }

      # validate username
      self_username <- self$username

      if (!is.character(self_username) || nchar(self_username) == 0) {
        cli::cli_abort("Invalid {.field username}. Must be a non-empty string", call = call)
      }

      # validate password (private)
      self_password <- private$password

      if (!is.character(self_password) || nchar(self_password) == 0) {
        cli::cli_abort("Invalid {.field password}. Must be a non-empty string", call = call)
      }

      invisible(self)

    },

    get_password = function() {
      private$password
    },

    get_logging_config = function() {
      return(self$logging)
    },

    set_logging_config = function(config) {
      self$logging <- LoggingConfig$new(config)
      return(invisible(self))
    },

    get_retry_policy_config = function() {
      return(self$retry_policy)
    },

    set_retry_policy_config = function(config) {
      self$retry_policy <- RetryPolicyConfig$new(config)
      return(invisible(self))
    }
  ),

  private = list(
    password = NULL
  )

)

# logging -----------------------------------------------------------------

LoggingConfig <- R6::R6Class(
  "LoggingConfig",
  public = list(
    log_level = NULL,
    log_namespace = NULL,
    log_formatter = NULL,
    log_layout = NULL,
    log_appender = NULL,
    log_file = NULL,
    log_redaction = NULL,
    redact_headers = NULL,
    redact_cookies = NULL,

    initialize = function(config = list()) {

      defaults <- list(
        log_level = logger::INFO,
        log_namespace = "gmhcommunities",
        log_formatter = logger::formatter_glue,
        log_layout = logger::layout_glue_generator("{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}"),
        log_appender = logger::appender_stdout,
        log_file = "logs/entrata.log",
        log_redaction = TRUE,
        redact_headers = c("Authorization"),
        redact_cookies = TRUE
      )

      cfg <- modifyList(defaults, config)

      self$log_level <- cfg$log_level
      self$log_namespace <- cfg$log_namespace
      self$log_formatter <- cfg$log_formatter
      self$log_layout <- cfg$log_layout
      self$log_appender <- cfg$log_appender
      self$log_file <- cfg$log_file
      self$log_redaction <- cfg$log_redaction
      self$redact_headers <- cfg$redact_headers
      self$redact_cookies <- cfg$redact_cookies

      self$validate()

      logger::log_threshold(self$log_level, namespace = self$log_namespace)
      logger::log_layout(self$log_layout, namespace = self$log_namespace)
      logger::log_formatter(self$log_formatter, namespace = self$log_namespace)
      logger::log_appender(self$log_appender, namespace = self$log_namespace)

      return(invisible(self))

    },

    validate = function(call = rlang::caller_env()) {

      current_namespaces <- logger::log_namespaces()
      current_log_level <- logger::log_threshold()

      # validate log_level %in% logger thresholds
      self_log_level <- self$log_level

      log_levels_str <- c(
        "OFF",
        "FATAL",
        "ERROR",
        "WARN",
        "SUCCESS",
        "INFO",
        "DEBUG",
        "TRACE"
      )

      if (!inherits(self_log_level, "loglevel")) {
        if (is.character(self_log_level) && toupper(self_log_level) %in% log_levels_str) {
          self$log_level <- logger::log_threshold(self$log_level, namespace = self$log_namespace)
        } else {
          cli::cli_abort("Invalid {.field log_level}. Must be one of {.field {log_levels_str}}", call = call)
        }
      } else {
        # ensure namespace set
        self$log_level <- logger::log_threshold(self$log_level, namespace = self$log_namespace)
      }

      return(invisible(self))
    }
  ),
  private = list()
)


# retry -------------------------------------------------------------------


RetryPolicyConfig <- R6::R6Class(
  "RetryPolicyConfig",
  public = list(
    max_retries = NULL,
    max_seconds = NULL,
    retry_on_failure = NULL,
    retry_on_status_codes = NULL,
    retry_backoff = NULL,
    retry_after = NULL,

    initialize = function(config = list()) {

      defaults <- list(
        max_retries = 5,
        max_seconds = 60,
        retry_on_failure = TRUE,
        retry_on_status_codes = c(429, 500, 502, 503, 504),
        retry_backoff = self$exponential_backoff,
        retry_after = self$get_retry_after
      )

      cfg <- modifyList(defaults, config)

      self$max_retries <- cfg$max_retries
      self$max_seconds <- cfg$max_seconds
      self$retry_on_failure <- cfg$retry_on_failure
      self$retry_on_status_codes <- cfg$retry_on_status_codes
      self$retry_backoff <- cfg$backoff
      self$retry_after <- cfg$retry_after

      self$validate()

      return(invisible(self))

    },

    validate = function(call = rlang::caller_env()) {

      # validate max_retries > 0
      self_max_retries <- self$max_retries

      if (!is.numeric(self_max_retries) || self_max_retries < 0) {
        cli::cli_abort("Invalid {.field max_retries}. Must be a positive integer", call = call)
      }

      # validate max_seconds > 0
      self_max_seconds <- self$max_seconds

      if (!is.numeric(self_max_seconds) || self_max_seconds < 0) {
        cli::cli_abort("Invalid {.field max_seconds}. Must be a positive integer", call = call)
      }

      return(invisible(self))
    }
  ),
  private = list(

    exponential_backoff = function(num_requests) {
      backoff <- 2^num_requests
      jitter <- runif(1, 0, 1)
      backoff <- backoff + jitter
      return(min(backoff, self$max_seconds))
    },

    constant_backoff = function(num_requests) {
      backoff <- self$max_seconds / self$max_retries
      jitter <- runif(1, 0, 1)
      backoff <- backoff + jitter
      return(min(backoff, self$max_seconds))
    },

    check_is_transient = is_transient <- function(resp) {
      check_response(resp)
      transient_status_codes <- self$retry_on_status_codes
      status_code_transient <- httr2::resp_status(resp) %in% transient_status_codes
      rate_limit_info <- entrata_resp_parse_rate_limit_headers(resp)
      rate_limit_transient <- all(sapply(rate_limit_info$remaining, function(x) x == 0))

      return(
        any(status_code_transient, rate_limit_transient)
      )
    },

    get_retry_after = function(resp) {
      check_response(resp)
      entrata_resp_parse_retry_after_header(resp)
    }

  )
)


#' entrata_config <- function(
#'     base_url = NULL,
#'     username = NULL,
#'     password = NULL,
#'     user_agent = NULL,
#'     request_id = NULL,
#'     request_headers = NULL,
#'     retry_policy = NULL,
#'     timeout = NULL,
#'     throttle_rate = NULL,
#'     throttle_realm = NULL,
#'     verbosity = NULL,
#'     progress = NULL,
#'     proxy = NULL,
#'     logging = NULL,
#'     caching = NULL,
#'     parallel = NULL,
#'     property_id = NULL,
#'     report_name = NULL,
#'     leasing_period = NULL,
#'     json_schema_validation = NULL,
#'     pagination = list(page_size = 100, max_pages = 1000),
#'     file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
#'     config = Sys.getenv("R_CONFIG_ACTIVE", "default")
#' ) {
#'
#'   # config.yml
#'   yml_cfg <- tryCatch({
#'     get_entrata_config(file = file, config = config)
#'   }, error = function(e) {
#'     list()
#'   })
#'
#'   # envvars
#'   env_cfg <- list(
#'     # required
#'     base_url = Sys.getenv("ENTRATA_BASE_URL", NULL),
#'     username = Sys.getenv("ENTRATA_USERNAME", NULL),
#'     password = Sys.getenv("ENTRATA_PASSWORD", NULL),
#'     user_agent = Sys.getenv("ENTRATA_USER_AGENT", NULL),
#'     request_id = Sys.getenv("ENTRATA_REQUEST_ID", NULL),
#'     # assume no env vars for lists (headers, retry_policy, logging, caching, pagination)
#'     # retry_policy = Sys.getenv("ENTRATA_RETRY_POLICY", NULL),
#'     max_retries = Sys.getenv("ENTRATA_MAX_RETRIES", NULL),
#'     retry_delay = Sys.getenv("ENTRATA_RETRY_DELAY", NULL),
#'     timeout = Sys.getenv("ENTRATA_TIMEOUT", NULL),
#'     throttle_rate = Sys.getenv("ENTRATA_THROTTLE_RATE", NULL),
#'     throttle_realm = Sys.getenv("ENTRATA_THROTTLE_REALM", NULL),
#'     verbosity = Sys.getenv("ENTRATA_VERBOSITY", NULL),
#'     progress = Sys.getenv("ENTRATA_PROGRESS", NULL),
#'     proxy = Sys.getenv("ENTRATA_PROXY", NULL),
#'     proxy_url = Sys.getenv("ENTRATA_PROXY_URL", NULL),
#'     proxy_port = Sys.getenv("ENTRATA_PROXY_PORT", NULL),
#'     proxy_username = Sys.getenv("ENTRATA_PROXY_USERNAME", NULL),
#'     proxy_password = Sys.getenv("ENTRATA_PROXY_PASSWORD", NULL),
#'
#'     logging = Sys.getenv("ENTRATA_LOGGING", NULL),
#'     log_file = Sys.getenv("ENTRATA_LOG_FILE", NULL),
#'     log_level = Sys.getenv("ENTRATA_LOG_LEVEL", NULL),
#'     caching = Sys.getenv("ENTRATA_CACHING", NULL),
#'     cache_dir = Sys.getenv("ENTRATA_CACHE_DIR", NULL),
#'     parallel = Sys.getenv("ENTRATA_PARALLEL", NULL),
#'     default_report_name = Sys.getenv("ENTRATA_DEFAULT_REPORT_NAME", NULL),
#'     default_leasing_period = Sys.getenv("ENTRATA_DEFAULT_LEASING_PERIOD", NULL),
#'     default_endpoint_methods = Sys.getenv("ENTRATA_DEFAULT_ENDPOINT_METHODS", NULL),
#'     json_schema_validation = Sys.getenv("ENTRATA_JSON_SCHEMA_VALIDATION", NULL),
#'     pagination = Sys.getenv("ENTRATA_PAGINATION", NULL)
#'   )
#'
#'   opts_cfg <- list(
#'     base_url = getOption("entrata.base_url", NULL),
#'     username = getOption("entrata.username", NULL),
#'     password = getOption("entrata.password", NULL),
#'     user_agent = getOption("entrata.user_agent", NULL),
#'     request_id = getOption("entrata.request_id", NULL),
#'     request_headers = getOption("entrata.request_headers", NULL),
#'     retry_policy = getOption("entrata.retry_policy", NULL),
#'     timeout = getOption("entrata.timeout", NULL),
#'     throttle_rate = getOption("entrata.throttle_rate", NULL),
#'     throttle_realm = getOption("entrata.throttle_realm", NULL),
#'     verbosity = getOption("entrata.verbosity", NULL),
#'     progress = getOption("entrata.progress", NULL),
#'     proxy = getOption("entrata.proxy", NULL),
#'     logging = getOption("entrata.logging", NULL),
#'     caching = getOption("entrata.caching", NULL),
#'     parallel = getOption("entrata.parallel", NULL),
#'     default_report_name = getOption("entrata.default_report_name", NULL),
#'     default_leasing_period = getOption("entrata.default_leasing_period", NULL),
#'     default_endpoint_methods = getOption("entrata.default_endpoint_methods", NULL),
#'     json_schema_validation = getOption("entrata.json_schema_validation", NULL),
#'     pagination = getOption("entrata.pagination", NULL)
#'   )
#'
#'   defaults_cfg <- list(
#'     base_url = "https://gmhcommunities.entrata.com/api/v1/",
#'     username = NULL,
#'     password = NULL,
#'     user_agent = "gmhcommunities/0.0.0.9000 (https://github.com/noclocks/gmhcommunities)",
#'     request_id = NULL, # as.integer(Sys.time()) is the default when left NULL
#'     request_headers = list(
#'       "Content-Type" = "application/json",
#'       "Accept" = "application/json"
#'     ),
#'     retry_policy = list(
#'       max_retries = 3,
#'       max_seconds = 30,
#'       retry_on_failure = TRUE,
#'       transient_status_codes = c(429, 500, 502, 503, 504),
#'       backoff = "exponential"
#'     ),
#'     timeout = 60,
#'     throttle_rate = NULL,
#'     throttle_realm = NULL,
#'     verbosity = NULL,
#'     progress = NULL,
#'     proxy = NULL,
#'     logging = list(
#'       log_file = NULL,
#'       log_level = "INFO"
#'     ),
#'     caching = list(
#'       cache_dir = NULL,
#'       cache_expiry = 3600,
#'       cache_size = 1000,
#'       cache_policy = "lru",
#'       cache_on_error = TRUE
#'     ),
#'     parallel = FALSE,
#'     default_property_id = NULL,
#'     default_report_name = "pre_lease",
#'     default_leasing_period = NULL,
#'     default_endpoint_methods = NULL,
#'     json_schema_validation = FALSE,
#'     pagination = list(page_size = 100, max_pages = 1000)
#'   )
#'
#'   cfg <- list(
#'     base_url = base_url %||% yml_cfg$base_url %||% env_cfg$base_url %||% opts_cfg$base_url %||% defaults_cfg$base_url,
#'     username = username %||% yml_cfg$username %||% env_cfg$username %||% opts_cfg$username %||% defaults_cfg$username,
#'     password = password %||% yml_cfg$password %||% env_cfg$password %||% opts_cfg$password %||% defaults_cfg$password,
#'     user_agent = user_agent %||% yml_cfg$user_agent %||% env_cfg$user_agent %||% opts_cfg$user_agent %||% defaults_cfg$user_agent,
#'     request_id = request_id %||% yml_cfg$request_id %||% env_cfg$request_id %||% opts_cfg$request_id %||% defaults_cfg$request_id,
#'     request_headers = request_headers %||% yml_cfg$request_headers %||% env_cfg$request_headers %||% opts_cfg$request_headers %||% defaults_cfg$request_headers,
#'     retry_policy = retry_policy %||% yml_cfg$retry_policy %||% env_cfg$retry_policy %||% opts_cfg$retry_policy %||% defaults_cfg$retry_policy,
#'
#'
#'
#'     options("entrata.config" = cfg)
#'     cli::cli_alert_success("Entrata Configuration Set Successfully.")
#'     cli::cli_alert_info("Access Entrata configuration via {.code getOption('entrata.config')}.")
#'
#'     invisible(cfg)
#'
#'     }
#'
#' # get configuration -------------------------------------------------------
#'
#' #' @rdname entrata_config
#' #' @export
#' #' @importFrom cli cli_abort
#' #' @importFrom config get
#' #' @importFrom rlang arg_match
#' get_entrata_config <- function(
#'     key = NULL,
#'     file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
#'     config = Sys.getenv("R_CONFIG_ACTIVE", "default")
#' ) {
#'
#'   # normalize path to config file
#'   file <- normalizePath(file, mustWork = FALSE)
#'
#'   # ensure config file exists
#'   if (!file.exists(file)) {
#'     cli::cli_abort(
#'       c(
#'         "Provided configuration file: {.field {basename(file)}} not found ",
#'         "under path: {.path {dirname(file)}}. Please ensure the file exists."
#'       )
#'     )
#'   }
#'
#'   # attempt to read the configuration file
#'   cfg <- tryCatch({
#'     config::get(
#'       value = "entrata",
#'       file = file,
#'       config = config
#'     )
#'   }, error = function(e) {
#'     cli::cli_abort(
#'       c(
#'         "Error reading configuration file {.field {basename(file)}}.",
#'         "Ensure the file contains an {.code {entrata}} configuration block for ",
#'         " the {.field {config}} configuration.",
#'         "Error: {.error_message {e}}"
#'       )
#'     )
#'   })
#'
#'   keys <- names(cfg)
#'
#'   if (!is.null(key)) {
#'     key <- rlang::arg_match(key, keys)
#'     return(cfg[[key]])
#'   }
#'
#'   return(cfg)
#'
#' }
#'
#' # validate ----------------------------------------------------------------
#'
#' #' @rdname entrata_config
#' #' @export
#' #' @importFrom rlang caller_env caller_arg
#' #' @importFrom cli cli_abort
#' #' @importFrom yaml read_yaml
#' #' @importFrom purrr pluck
#' validate_entrata_config <- function(
#'     cfg,
#'     arg = rlang::caller_arg(cfg),
#'     call = rlang::caller_env()
#' ) {
#'
#'   # copy provided cfg
#'   cfg_orig <- cfg
#'
#'   # check if list or file path, and read if necessary
#'   if (!is.list(cfg) && file.exists(cfg)) {
#'
#'     # read whole yaml
#'     cfg <- yaml::read_yaml(cfg)
#'
#'     # ensure default key is present
#'     if (!("default" %in% names(cfg))) {
#'
#'       # throw error condition
#'       cli::cli_abort(
#'         c(
#'           "Invalid Configuration Provided: {.arg {arg}}",
#'           "Configuration is missing required {.field default} configuration."
#'         ),
#'         call = call
#'       )
#'
#'     }
#'
#'     # extract default config values
#'     default_cfg <- purrr::pluck(cfg, "default")
#'
#'     # ensure entrata key is present
#'     if (!("entrata" %in% names(default_cfg))) {
#'       cli::cli_abort(
#'         c(
#'           "Invalid Configuration Provided: {.arg {arg}}",
#'           "Configuration is missing required {.field entrata} field."
#'         ),
#'         call = call
#'       )
#'     }
#'
#'     # replace cfg with entrata values from the file used
#'     cfg <- default_cfg$entrata
#'   }
#'
#'   # if cfg has default, go one level deeper
#'   if ("default" %in% names(cfg)) {
#'     cfg <- purrr::pluck(cfg, "default")
#'   }
#'
#'   # if cfg has entrata, go one level deeper
#'   if ("entrata" %in% names(cfg)) {
#'     cfg <- purrr::pluck(cfg, "entrata")
#'   }
#'
#'   # validations -------------------------------------------------------------
#'
#'   # validate list
#'   if (!is.list(cfg)) {
#'     cli::cli_abort(
#'       c(
#'         "Invalid Configuration Provided: {.arg {arg}}. ",
#'         "Configuration must be a list."
#'       ),
#'       call = call
#'     )
#'   }
#'
#'   # validate required fields
#'   req_fields <- c("username", "password", "base_url")
#'   cfg_fields <- names(cfg)
#'   missing_fields <- req_fields[!req_fields %in% cfg_fields]
#'   num_missing_fields <- length(missing_fields)
#'
#'   if (num_missing_fields > 0) {
#'     # error message
#'     msg <- c(
#'       "Invalid Configuration Provided: {.arg {arg}}. ",
#'       "Provided configuration is missing required ",
#'       if (num_missing_fields == 1) {
#'         paste0("{.field ", missing_fields, "}")
#'       } else {
#'         paste0("fields: ", paste0("{.field ", missing_fields, "}", collapse = ", "))
#'       }
#'     )
#'
#'     # throw error
#'     cli::cli_abort(msg, call = call)
#'   }
#'
#'   # return original cfg invisibly
#'   invisible(cfg_orig)
#'
#' }
