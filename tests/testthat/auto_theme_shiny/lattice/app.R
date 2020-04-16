library(shiny)
library(thematic)

thematic_on(font = "auto")
onStop(thematic_off)

ui <- fluidPage(
  titlePanel("Hello"),
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css?family=Pacifico", rel="stylesheet"),
    tags$style(HTML("body{background-color:#444; color:#e4e4e4; font-family: Pacifico}")),
    tags$style(HTML("a{color:#e39777}"))
  ),
  plotOutput("p")
)

server <- function(input, output, session) {
  output$p <- renderPlot({
    lattice::show.settings()
  })
}

shinyApp(ui, server)
