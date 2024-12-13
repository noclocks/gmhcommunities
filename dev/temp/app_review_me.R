library(shiny)
library(bslib)
library(tidyverse)
library(apexcharter)
library(reactable)

# Data preparation
data <- read.csv(text = "property_name,total_beds,model_beds,current_occupancy,total_new,total_renewals,total_leases,prelease_pct,prior_total_new,prior_total_renewals,prior_total_leases,prior_prelease_pct,yoy_variance_num,yoy_variance_pct,weekly_new_leases,weekly_new_renewals,weekly_total_leases,weekly_pct_gained,weekly_velocity_beds_left,weekly_velocity_leased_this_week,weekly_velocity_90_pct,weekly_velocity_95_pct,weekly_velocity_100_pct
The Academy Chorro,96,0,0.9895833333,72,23,95,0.9895833333,53,38,91,0.9479166667,4,0.04166666667,0,0,0,0,1,0,0.06923076923,0.07307692308,0.07692307692
The Academy Palomar,129,0,1,86,38,124,0.9612403101,90,35,125,0.9689922481,-1,-0.007751937984,0,0,0,0,5,0,0.3461538462,0.3653846154,0.3846153846
Academy 65,309,0,0.8899676375,47,102,149,0.4822006472,48,106,154,0.498381877,-5,-0.01618122977,0,0,0,0,160,0,11.07692308,11.69230769,12.30769231
1047 Commonwealth Avenue,183,0,0.9945355191,51,65,116,0.6338797814,63,61,124,0.6775956284,-8,-0.04371584699,6,2,8,0.04371584699,67,8,4.638461538,4.896153846,5.153846154
Academy Lincoln,632,0,0.960443038,346,226,572,0.9050632911,332,134,466,0.7373417722,106,0.167721519,0,0,0,0,60,0,4.153846154,4.384615385,4.615384615
307 E. Daniel,40,4,1,23,16,39,0.975,34,5,39,0.975,0,0,0,0,0,0,1,0,0.06923076923,0.07307692308,0.07692307692
501 S. 6th,104,0,0.9903846154,48,54,102,0.9807692308,73,24,97,0.9326923077,5,0.04807692308,0,0,0,0,2,0,0.1384615385,0.1461538462,0.1538461538
908 S. 1st,96,0,0.96875,61,34,95,0.9895833333,71,16,87,0.90625,8,0.08333333333,0,0,0,0,1,0,0.06923076923,0.07307692308,0.07692307692
1008 S. 4th,158,4,0.9620253165,103,55,158,1,130,21,151,0.9556962025,7,0.04430379747,0,0,0,0,0,0,0,0,0
The Dean Campustown,672,4,0.9866071429,317,272,589,0.8764880952,408,205,613,0.912202381,-24,-0.03571428571,5,0,5,0.00744047619,83,5,5.746153846,6.065384615,6.384615385")

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
    "Summary",
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

    layout_columns(
      fill = FALSE,
      col_widths = c(12),
      navset_card_tab(
        title = "Property Tables",
        nav_panel(
          "Details",
          reactableOutput("property_table")
        )
      )
    ),

    layout_columns(
      fill = FALSE,
      col_widths = c(12),
      navset_card_tab(
        title = "Visualizations",
        nav_panel(
          "Occupancy",
          apexchartOutput("occupancy_plot"),
          p(class = "mt-3", textOutput("occupancy_explanation"))
        ),
        nav_panel(
          "Lease Comparison",
          apexchartOutput("leases_plot"),
          p(class = "mt-3", textOutput("leases_explanation"))
        ),
        nav_panel(
          "Lease Distribution",
          apexchartOutput("distribution_plot"),
          p(class = "mt-3", textOutput("distribution_explanation"))
        ),
        nav_panel(
          "YOY Variance",
          apexchartOutput("variance_plot"),
          p(class = "mt-3", textOutput("variance_explanation"))
        )
      )
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
              color <- if (value >= 0) "#00E396" else "#FF4560"
              list(color = color, fontWeight = "bold")
            }
          )
        ),
        defaultPageSize = 5,
        filterable = TRUE,
        sortable = TRUE,
        striped = TRUE,
        highlight = TRUE
      )

  })

}

shinyApp(ui, server)
