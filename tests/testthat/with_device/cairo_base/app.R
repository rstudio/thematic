library(shiny)
library(ggplot2)
library(thematic)

thematic_begin("black", "white", font = font_spec("Pacifico", 1.25, update = TRUE))
onStop(function() {
  thematic_end()
})

ui <- fluidPage(
  imageOutput("png"),
  imageOutput("svg")
)

render_image <- function(expr) {
  snapshotPreprocessOutput(renderImage(expr), function(value) {})
}

server <- function(input, output, session) {
  output$png <- render_image({
    file <- thematic_with_device(
      qplot(1:10) + ggtitle("grDevices::png"),
      device = grDevices::png, type = "cairo",
      width = 800, height = 400
    )
    list(src = file, width = 800, height = 400)
  })
  output$svg <- render_image({
    file <- thematic_with_device(
      qplot(1:10) + ggtitle("grDevices::svg"),
      device = grDevices::svg, filename = tempfile(fileext = ".svg"),
      width = 8, height = 4
    )
    list(src = file, width = 800, height = 400)
  })

}

shinyApp(ui, server)
