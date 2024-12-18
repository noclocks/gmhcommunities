star_rating_input <- function(id, label = "Rating", stars = 5) {
  htmltools::tagList(
    htmltools::tags$head(
      fontawesome::fa_html_dependency()
    ),
    htmltools::tags$label(label),
    htmltools::tags$div(
      id = id,
      class = "star-rating",
      lapply(1:stars, function(i) {
        tags$i(class = "fa fa-star", `data-value` = i)
      })
    ),
    tags$script(HTML("
      const initStarRating = function(el) {
        const stars = el.querySelectorAll('.fa-star');
        stars.forEach(star => {
          star.addEventListener('click', function() {
            const value = this.dataset.value;
            stars.forEach(s =>
              s.style.color = s.dataset.value <= value ? 'gold' : 'gray'
            );
            Shiny.setInputValue(el.id, value);
          });
        });
      };
      document.querySelectorAll('.star-rating').forEach(initStarRating);
    "))
  )
}

ui <- fluidPage(
  star_rating_input("rating"),
  verbatimTextOutput("rating")
)

server <- function(input, output, session) {
  output$rating <- renderPrint({
    input$rating
  })
}

shinyApp(ui, server)
