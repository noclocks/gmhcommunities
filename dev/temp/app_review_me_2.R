# Chart explanations
explanations <- list(
  occupancy = "This chart shows the current occupancy rate for each property. Higher percentages indicate better utilization of available beds.",
  leases = "Compares the number of current leases with prior period leases for each property, helping identify trending patterns in lease volumes.",
  distribution = "Breaks down the composition of leases between new tenants and renewals, showing the balance of tenant retention vs. new acquisition.",
  variance = "Displays the year-over-year change in lease percentage. Green bars indicate improvement, while red bars show decline from the previous year."
)

ui <- page_navbar(
  title = "Property Leasing Dashboard",
  sidebar = sidebar(
    checkboxGroupInput("properties", "Filter Properties",
                       choices = unique(data$property_name),
                       selected = unique(data$property_name))
  ),

  nav_panel(
    "Overview",
    layout_columns(
      value_box(
        title = "Total Properties",
        value = nrow(data),
        showcase = bsicons::bs_icon("building")
      ),
      value_box(
        title = "Total Beds",
        value = sum(data$total_beds),
        showcase = bsicons::bs_icon("houses")
      ),
      value_box(
        title = "Average Occupancy",
        value = scales::percent(mean(data$current_occupancy), accuracy = 0.1),
        showcase = bsicons::bs_icon("percent")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Property Details"),
      reactableOutput("property_table")
    )
  ),

  nav_panel(
    "Occupancy",
    card(
      height = "500px",
      card_header("Current Occupancy Rates"),
      apexchartOutput("occupancy_plot"),
      textOutput("occupancy_explanation")
    )
  ),

  nav_panel(
    "Leases Comparison",
    card(
      height = "500px",
      card_header("Current vs Prior Leases"),
      apexchartOutput("leases_plot"),
      textOutput("leases_explanation")
    )
  ),

  nav_panel(
    "Lease Distribution",
    card(
      height = "500px",
      card_header("New vs Renewal Distribution"),
      apexchartOutput("distribution_plot"),
      textOutput("distribution_explanation")
    )
  ),

  nav_panel(
    "YOY Variance",
    card(
      height = "500px",
      card_header("Year-over-Year Variance"),
      apexchartOutput("variance_plot"),
      textOutput("variance_explanation")
    )
  )
)

server <- function(input, output) {

  filtered_data <- reactive({
    data %>%
      filter(property_name %in% input$properties)
  })

  # Occupancy Plot
  output$occupancy_plot <- renderApexchart({
    df <- filtered_data() %>%
      arrange(desc(current_occupancy)) %>%
      mutate(occupancy_pct = round(current_occupancy * 100, 1))

    apex(data = df,
         type = "bar",
         mapping = aes(x = property_name, y = occupancy_pct)) %>%
      ax_yaxis(title = list(text = "Occupancy Rate (%)"),
               labels = list(formatter = JS("function(val) { return val + '%' }"))) %>%
      ax_xaxis(title = list(text = "Property")) %>%
      ax_tooltip(y = list(formatter = JS("function(val) { return val + '%' }")))
  })

  # Leases Plot
  output$leases_plot <- renderApexchart({
    df <- filtered_data() %>%
      select(property_name, total_leases, prior_total_leases) %>%
      rename("Current" = total_leases,
             "Prior" = prior_total_leases) %>%
      pivot_longer(-property_name, names_to = "Period", values_to = "Leases")

    apex(data = df,
         type = "bar",
         mapping = aes(x = property_name, y = Leases, fill = Period)) %>%
      ax_yaxis(title = list(text = "Number of Leases")) %>%
      ax_xaxis(title = list(text = "Property")) %>%
      ax_tooltip(shared = TRUE, intersect = TRUE)
  })

  # Distribution Plot
  output$distribution_plot <- renderApexchart({
    df <- filtered_data() %>%
      select(property_name, total_new, total_renewals) %>%
      rename("New" = total_new,
             "Renewals" = total_renewals) %>%
      pivot_longer(-property_name, names_to = "Type", values_to = "Count")

    apex(data = df,
         type = "bar",
         mapping = aes(x = property_name, y = Count, fill = Type)) %>%
      ax_plotOptions(bar = list(stacked = TRUE)) %>%
      ax_yaxis(title = list(text = "Number of Leases")) %>%
      ax_xaxis(title = list(text = "Property")) %>%
      ax_tooltip(shared = TRUE, followCursor = TRUE)
  })

  # Variance Plot
  output$variance_plot <- renderApexchart({
    df <- filtered_data() %>%
      arrange(yoy_variance_pct) %>%
      mutate(variance_pct = round(yoy_variance_pct * 100, 1),
             color = ifelse(yoy_variance_pct >= 0, "#00E396", "#FF4560"))

    apex(data = df,
         type = "bar",
         mapping = aes(x = property_name, y = variance_pct)) %>%
      ax_colors(unique(df$color)) %>%
      ax_yaxis(title = list(text = "YOY Variance (%)"),
               labels = list(formatter = JS("function(val) { return val + '%' }"))) %>%
      ax_xaxis(title = list(text = "Property")) %>%
      ax_tooltip(y = list(formatter = JS("function(val) { return val + '%' }")))
  })

  # Chart Explanations
  output$occupancy_explanation <- renderText(explanations[["occupancy"]])
  output$leases_explanation <- renderText(explanations[["leases"]])
  output$distribution_explanation <- renderText(explanations[["distribution"]])
  output$variance_explanation <- renderText(explanations[["variance"]])

  # Table Output
  output$property_table <- renderReactable({
    filtered_data() %>%
      select(property_name, total_beds, current_occupancy, total_leases,
             prelease_pct, yoy_variance_pct) %>%
      reactable(
        columns = list(
          property_name = colDef(name = "Property"),
          total_beds = colDef(name = "Total Beds"),
          current_occupancy = colDef(
            name = "Current Occupancy",
            format = colFormat(percent = TRUE, digits = 1)
          ),
          total_leases = colDef(name = "Total Leases"),
          prelease_pct = colDef(
            name = "Pre-lease %",
            format = colFormat(percent = TRUE, digits = 1)
          ),
          yoy_variance_pct = colDef(
            name = "YOY Variance",
            format = colFormat(percent = TRUE, digits = 1),
            style = function(value) {
              color <- if (
