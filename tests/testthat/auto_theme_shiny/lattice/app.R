library(shiny)
library(thematic)

thematic_on(font = "auto")
onStop(thematic_off)

ui <- fluidPage(
  titlePanel("Hello"),
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css?family=Pacifico", rel="stylesheet"),
    tags$style(HTML("body{background-color:#444444; color:#e4e4e4; font-family: Pacifico}")),
    tags$style(HTML("a{color:#e39777}"))
  ),
  plotOutput("p")
)

render_plot <- function(expr) {
  snapshotPreprocessOutput(renderPlot(expr), function(value) {})
}

server <- function(input, output, session) {
  output$p <- render_plot({
    lattice::show.settings()
  })
}

shinyApp(ui, server)
