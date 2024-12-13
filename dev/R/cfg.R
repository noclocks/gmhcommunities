cfg_structure <- list(
  base_url = "https://gmhcommunities.entrata.com/api/v1/",
  auth = list(username = NULL, password = NULL),
  user_agent = "gmhcommunities/0.0.0.9000 (https://github.com/noclocks/gmhcommunities)",
  request = list(
    defaults = list(
      request_id = "random",
      property_id = NULL,
      report_name = "pre_lease",
      leasing_period = "latest"
    ),
    headers = list(
      "Accept" = "application/json",
      "Content-Type" = "application/json"
    ),
    retry_policy = list(
      max_retries = 3,
      max_seconds = 30,
      retry_on_failure = TRUE,
      retry_on_status_codes = c(429, 500, 502, 503, 504),
      backoff = "exponential"
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
    )
  ),
  logging = list(
    log_file = NULL,
    log_level = logger::INFO,
    log_namespace = "gmhcommunities",
    log_formatter = logger::formatter_glue,
    log_layout = logger::layout_glue_generator("{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}"),
    log_appender = logger::appender_stdout
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
