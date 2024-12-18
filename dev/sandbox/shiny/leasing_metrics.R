library(shiny)
library(bslib)
library(tidyverse)
library(DT)

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

ui <- page_sidebar(
  title = "Property Leasing Dashboard",
  sidebar = sidebar(
    selectInput("plot_type", "Select Visualization",
                choices = c("Occupancy Rates" = "occupancy",
                            "Current vs Prior Leases" = "leases",
                            "New vs Renewal Distribution" = "distribution",
                            "YOY Variance" = "variance")),
    checkboxGroupInput("properties", "Filter Properties",
                       choices = unique(data$property_name),
                       selected = unique(data$property_name))
  ),

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
    plotOutput("main_plot", height = "400px")
  ),

  card(
    full_screen = TRUE,
    DTOutput("property_table")
  )
)

server <- function(input, output) {

  filtered_data <- reactive({
    data %>%
      filter(property_name %in% input$properties)
  })

  output$main_plot <- renderPlot({
    req(input$plot_type)

    switch(input$plot_type,
           "occupancy" = {
             ggplot(filtered_data(), aes(x = reorder(property_name, current_occupancy), y = current_occupancy)) +
               geom_col(fill = "steelblue") +
               geom_text(aes(label = scales::percent(current_occupancy, accuracy = 0.1)),
                         vjust = -0.5) +
               theme_minimal() +
               labs(title = "Current Occupancy Rates by Property",
                    x = "Property", y = "Occupancy Rate") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           "leases" = {
             ggplot(filtered_data(), aes(x = reorder(property_name, total_leases))) +
               geom_col(aes(y = total_leases, fill = "Current"), position = "dodge") +
               geom_col(aes(y = prior_total_leases, fill = "Prior"), position = "dodge") +
               theme_minimal() +
               labs(title = "Current vs Prior Total Leases",
                    x = "Property", y = "Number of Leases",
                    fill = "Period") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           "distribution" = {
             filtered_data() %>%
               select(property_name, total_new, total_renewals) %>%
               pivot_longer(cols = c(total_new, total_renewals),
                            names_to = "type", values_to = "count") %>%
               ggplot(aes(x = reorder(property_name, count), y = count, fill = type)) +
               geom_col(position = "stack") +
               theme_minimal() +
               labs(title = "New vs Renewal Lease Distribution",
                    x = "Property", y = "Number of Leases",
                    fill = "Lease Type") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           "variance" = {
             ggplot(filtered_data(), aes(x = reorder(property_name, yoy_variance_pct), y = yoy_variance_pct)) +
               geom_col(aes(fill = yoy_variance_pct >= 0)) +
               geom_text(aes(label = scales::percent(yoy_variance_pct, accuracy = 0.1)),
                         vjust = ifelse(filtered_data()$yoy_variance_pct >= 0, -0.5, 1.5)) +
               theme_minimal() +
               labs(title = "Year-over-Year Variance in Lease Percentage",
                    x = "Property", y = "YOY Variance") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               scale_fill_manual(values = c("red", "green"), guide = "none")
           })
  })

  output$property_table <- renderDT({
    filtered_data() %>%
      select(property_name, total_beds, current_occupancy, total_leases,
             prelease_pct, yoy_variance_pct) %>%
      datatable(options = list(pageLength = 5),
                rownames = FALSE) %>%
      formatPercentage(columns = c("current_occupancy", "prelease_pct", "yoy_variance_pct"),
                       digits = 1)
  })
}

shinyApp(ui, server)
