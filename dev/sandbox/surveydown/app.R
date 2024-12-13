# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
require(config)

db_config <- config::get("db")

# Database setup

# surveydown stores data on a database that you define at https://supabase.com/
# To connect to a database, update the sd_database() function with details
# from your supabase database. For this demo, we set ignore = TRUE, which will
# ignore the settings and won't attempt to connect to the database. This is
# helpful for local testing if you don't want to record testing data in the
# database table. See the documentation for details:
# https://surveydown.org/store-data

db <- surveydown::sd_database(
  host       = db_config$host,
  port       = db_config$port,
  dbname     = db_config$dbname,
  user       = db_config$user,
  password   = db_config$pass,
  table      = "market_survey",
  ignore = TRUE
)

# surveydown::sd_get_data(db)

# Server setup
server <- function(input, output, session) {

  # Define any conditional skip logic here (skip to page if a condition is true)
  surveydown::sd_skip_if()

  # Define any conditional display logic here (show a question if a condition is true)
  surveydown::sd_show_if()

  surveydown::sd_set_password("p")

  # Database designation and other settings
  surveydown::sd_server(
    db = db,
    # required_questions = c(
    #   "property_name",
    #   "property_website",
    #   "property_address",
    #   "property_phone_number",
    #   "property_developer",
    #   "property_manager",
    #   "property_owner",
    #   "property_type",
    #   "property_status",
    #   "product_type",
    #   "property_rating",
    #   "comp_status",
    #   "year_built",
    #   "year_last_renovated",
    #   "most_recent_sale_date",
    #   "distance_to_campus"
    # ),
    all_questions_required = TRUE,
    start_page = NULL,
    admin_page = TRUE,
    auto_scroll = TRUE,
    rate_survey = TRUE,
    language = "en",
    use_cookies = TRUE
  )

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
