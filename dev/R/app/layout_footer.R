
#  ------------------------------------------------------------------------
#
# Title : Shiny App Layout Components - Footer
#    By : Jimmy Briggs
#  Date : 2024-12-14
#
#  ------------------------------------------------------------------------

layout_footer <- function() {

  bslib::card_footer(
    class = class,
    htmltools::tags$footer(
      style = paste0("text-align: ", align, ";"),
      htmltools::tags$hr(),
      footer_top_section(app_info, developer_info),
      htmltools::tags$br(),
      footer_client_section(client_info),
      htmltools::tags$hr(),
      footer_copyright_section(year, copyright_holder, developer_info),
      htmltools::tags$hr(),
      footer_logo_section(developer_info, client_info, entrata_info)
    )
  )

}

footer_top_section <- function(app_info, developer_info) {
  htmltools::tags$div(
    htmltools::tags$span(
      htmltools::tags$strong(app_info$name),
      " v",
      app_info$version,
      htmltools::HTML("&middot;"),
      "Developed by ",
      htmltools::tags$a(
        href = developer_info$url,
        paste0(developer_info$name, " ")
      ),
      htmltools::tags$img(
        src = developer_info$symbol,
        alt = paste0(developer_info$name, " logo symbol"),
        height = 20,
        class = "footer-img"
      ),
      " using ",
      htmltools::tags$a(
        href = "https://shiny.rstudio.com/",
        htmltools::tags$span(
          "R Shiny ",
          fontawesome::fa(name = "r-project", fill = "steelblue")
        )
      ),
      htmltools::HTML("&middot;"),
      htmltools::tags$a(
        href = app_info$docs_url,
        htmltools::tags$span(
          "View Documentation ",
          shiny::icon("book")
        )
      ),
      htmltools::HTML("&middot;"),
      htmltools::tags$a(
        href = app_info$repo_url,
        htmltools::tags$span(
          "View on GitHub ",
          shiny::icon("github")
        )
      )
    )
  )
}

#' @rdname mod_footer
#' @export
#' @importFrom htmltools tags HTML
footer_client_section <- function(client_info) {
  htmltools::tags$div(
    htmltools::tags$span(
      "Client: ",
      htmltools::tags$a(
        href = client_info$url,
        client_info$name
      )
    ),
    if (!is.null(client_info$symbol)) {
      htmltools::tags$img(
        src = client_info$symbol,
        alt = paste0(client_info$name, " logo symbol"),
        height = 20,
        class = "footer-img"
      )
    }
  )
}

#' @rdname mod_footer
#' @export
#' @importFrom bslib layout_columns
#' @importFrom htmltools tags
footer_logo_section <- function(app_info = NULL, developer_info = NULL, client_info = NULL) {

  # Initialize empty lists for columns and column widths
  columns <- list()
  col_widths <- c()

  # Define the total units (e.g., 12 for a Bootstrap grid system)
  total_width_units <- 12

  # Define the units for side margins (ensure it's an integer)
  side_margin_units <- 1

  # Collect non-NULL content sections
  content_sections <- list()

  if (!is.null(app_info)) {
    content_sections[[length(content_sections) + 1]] <- htmltools::tags$div(
      htmltools::tags$a(
        href = app_info$url,
        htmltools::tags$img(
          src = app_info$logo,
          alt = paste0(app_info$name, " logo"),
          height = "50px"
        )
      )
    )
  }

  if (!is.null(developer_info)) {
    content_sections[[length(content_sections) + 1]] <- htmltools::tags$div(
      htmltools::tags$a(
        href = developer_info$url,
        htmltools::tags$img(
          src = developer_info$logo,
          alt = paste0(developer_info$name, " logo"),
          height = "50px"
        )
      )
    )
  }

  if (!is.null(client_info)) {
    content_sections[[length(content_sections) + 1]] <- htmltools::tags$div(
      htmltools::tags$a(
        href = client_info$url,
        htmltools::tags$img(
          src = client_info$logo,
          alt = paste0(client_info$name, " logo"),
          height = "50px"
        )
      )
    )
  }

  num_content_sections <- length(content_sections)

  # If no content sections are provided, return an empty div
  if (num_content_sections == 0) {
    return(htmltools::tags$div())
  }

  # Calculate the content units available after subtracting side margins
  content_units <- total_width_units - 2 * side_margin_units

  # Calculate the base width for each content section (ensure it's an integer)
  content_column_width <- floor(content_units / num_content_sections)

  # Calculate total assigned units and remaining units to distribute
  assigned_units <- 2 * side_margin_units + num_content_sections * content_column_width
  remaining_units <- total_width_units - assigned_units

  # Distribute remaining units among content columns
  content_col_widths <- rep(content_column_width, num_content_sections)
  if (remaining_units > 0) {
    # Distribute the extra units to the first few columns
    for (i in seq_len(remaining_units)) {
      content_col_widths[i] <- content_col_widths[i] + 1
    }
  }

  # Start building the columns list with the side margin at the beginning
  columns[[1]] <- htmltools::tags$div()  # Left margin
  col_widths[1] <- side_margin_units

  # Add each content section to the columns list and update col_widths
  for (i in seq_along(content_sections)) {
    columns[[length(columns) + 1]] <- content_sections[[i]]
    col_widths <- c(col_widths, content_col_widths[i])
  }

  # Add the side margin at the end
  columns[[length(columns) + 1]] <- htmltools::tags$div()  # Right margin
  col_widths <- c(col_widths, side_margin_units)

  # Ensure that col_widths sums to total_width_units
  if (sum(col_widths) != total_width_units) {
    stop("Column widths do not sum up to total width units.")
  }

  # Prepare arguments for layout_columns
  args <- c(list(col_widths = col_widths), columns)

  # Create the layout using do.call
  do.call(bslib::layout_columns, args)
}

#' @rdname mod_footer
#' @export
#' @importFrom htmltools tags HTML
footer_copyright_section <- function(year, copyright_holder, developer_info) {
  htmltools::tags$div(
    htmltools::tags$span(
      htmltools::HTML("&copy;"),
      year,
      " ",
      htmltools::tags$a(
        href = developer_info$url,
        copyright_holder
      ),
      " | All rights reserved."
    )
  )
}


# validate_image ----------------------------------------------------------

#' @keywords internal
#' @noRd
validate_image <- function(img_path) {
  full_path <- system.file(img_path, package = "gmhcommunities")
  if (file.exists(full_path)) {
    return(img_path)
  } else {
    warning(paste("Image not found:", img_path, "- using placeholder image."))
    return("www/images/shared/placeholders/default-image.png")
  }
}

