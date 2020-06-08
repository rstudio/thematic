library(shiny)
library(ggplot2)
library(thematic)

thematic_on("black", "white", font = font_spec("Pacifico", 1.25, update = TRUE))
onStop(function() {
  thematic_off()
})

ui <- fluidPage(
  imageOutput("svg")
)

render_image <- function(expr) {
  snapshotPreprocessOutput(renderImage(expr), function(value) {})
}

server <- function(input, output, session) {
  output$svg <- render_image({
    file <- thematic_save_plot(
      qplot(1:10) + ggtitle("grDevices::svg"),
      device = grDevices::svg, filename = tempfile(fileext = ".svg"),
      antialias = "none",
      width = 8, height = 4
    )
    list(src = file, width = 800, height = 400)
  })
}

shinyApp(ui, server)
