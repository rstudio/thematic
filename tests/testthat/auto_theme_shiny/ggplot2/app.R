library(shiny)
library(thematic)

thematic_begin(font = "auto")
onStop(thematic_end)

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
    ggplot2::qplot(1:10, 1:10, color = 1:10)
  })
}

shinyApp(ui, server)
