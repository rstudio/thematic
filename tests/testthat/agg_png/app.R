library(shiny)
library(ggplot2)
library(thematic)

thematic_on("black", "white", font = font_spec("Pacifico", 1.25, update = TRUE))
onStop(function() {
  thematic_off()
})

ui <- fluidPage(
  imageOutput("ragg_png")
)

render_image <- function(expr) {
  snapshotPreprocessOutput(renderImage(expr), function(value) {})
}

server <- function(input, output, session) {
  output$ragg_png <- render_image({
    file <- thematic_save_plot(
      qplot(1:10) + ggtitle("ragg::agg_png"),
      device = ragg::agg_png,
      width = 800, height = 400
    )
    list(src = file, width = 800, height = 400)
  })
}

shinyApp(ui, server)
