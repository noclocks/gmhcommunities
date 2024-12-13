market_survey_input <- function(
  id,
  ns = function(x) { x },
  ...
) {

  # filter for row for id
  input_df <- property_summary_inputs |>
    dplyr::filter(
      .data$id == .env$id
    )

  if (nrow(input_df) != 1) {
    cli::cli_abort("No input data found for id: {.arg id}.")
  }

  # get input & icon functions
  input_fn <- eval(parse(text = input_df$input_function[[1]]))
  input_icon_fn <- eval(parse(text = input_df$icon_function[[1]]))

  stopifnot(
    rlang::is_function(input_fn),
    rlang::is_function(input_icon_fn)
  )

  # get input icon
  input_icon_str <- input_df$icon[[1]]
  input_icon <- input_icon_fn(input_icon_str)

  input_fn_formals <- formalArgs(input_fn)
  input_fn_arg_names <- names(input_df)[names(input_df) %in% input_fn_formals]
  input_fn_arg_names <- input_fn_arg_names[input_fn_arg_names != "id"]
  input_fn_arg_names <- input_fn_arg_names[input_fn_arg_names != "icon"]

  # get input values
  input_args <- input_df[input_fn_arg_names] |> as.list()

  do.call(input_fn, c(list(inputId = ns(id), icon = input_icon), input_args))

}


x <- shinyWidgets::textInputIcon(
  ns("property_name"),
  label = "Property Name",
  icon = shiny::icon("building"),
  placeholder = "Enter Property Name"
)

y <- market_survey_input("property_name")

all.equal(y, x) # TRUE
waldo::compare(x, y) # TRUE
identical(y, x) # FALSE?

htmltools::tagList(
  shinyWidgets::textInputIcon(
    ns("property_name"),
    label = "Property Name",
    icon = shiny::icon("building"),
    value = NULL,
    placeholder = "Enter Property Name"
  ),
  shinyWidgets::textInputIcon(
    ns("website"),
    label = "Website",
    icon = shiny::icon("globe"),
    value = NULL,
    placeholder = "Enter Property Website"
  ),
  shinyWidgets::textInputIcon(
    ns("address"),
    label = "Address",
    icon = shiny::icon("house"),
    value = NULL,
    placeholder = "1077 Commonwealth Ave, Boston, MA 02215"
  ),
  shinyWidgets::textInputIcon(
    ns("phone"),
    label = "Phone",
    icon = shiny::icon("phone"),
    value = NULL,
    placeholder = "617-500-6481"
  ),
  shinyWidgets::textInputIcon(
    ns("property_image"),
    label = "Property Image",
    icon = shiny::icon("image"),
    value = NULL, # https://images1.forrent.com/i2/0ksd7Uwm-74efUhB-UiU1cEyVv3NVOiCCVuF4oQ7cZI/117/image.jpg
    placeholder = "www.example.com/image.jpg"
  )

)
