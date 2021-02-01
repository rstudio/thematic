library(shiny)
library(ggplot2)
library(thematic)

thematic_shiny("black", "white", font = font_spec("Pacifico", 1.25, update = TRUE))

ui <- fluidPage(
  imageOutput("png")
)

render_image <- function(expr) {
  snapshotPreprocessOutput(renderImage(expr), function(value) {})
}

server <- function(input, output, session) {
  output$png <- render_image({
    file <- thematic_save_plot(
      qplot(1:10) + ggtitle("grDevices::png"),
      device = grDevices::png, type = "cairo",
      antialias = "none",
      width = 800, height = 400
    )
    list(src = file, width = 800, height = 400)
  })
}

shinyApp(ui, server)
