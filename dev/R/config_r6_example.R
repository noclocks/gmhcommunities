cfg <- EntrataConfig$new(config = list(
  username = "myuser",
  password = "mypassword",
  logging = list(log_level = "DEBUG"),
  retry_policy = list(max_retries = 5)
))


cfg$logging$log_level
cfg$retry_policy$max_retries
