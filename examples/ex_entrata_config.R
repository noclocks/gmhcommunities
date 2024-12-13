#

# retrieve the full entrata configuration
cfg <- get_entrata_config()
names(cfg)
str(cfg)

# check that the usernames match
get_entrata_config("username") == cfg$username

# retrieve the base url from the configuration
get_entrata_config("base_url")

# set the cfg password to the production password
cfg$password <- get_entrata_config("password", config = "production")

# can alter config environments or files as needed
Sys.setenv("R_CONFIG_ACTIVE" = "production")
get_entrata_config()

Sys.unsetenv("R_CONFIG_ACTIVE")
get_entrata_config(config = "production")

# validate the configuration
validate_entrata_config(cfg)
