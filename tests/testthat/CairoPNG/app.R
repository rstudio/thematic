library(shiny)
library(ggplot2)
library(thematic)

thematic_on("black", "white", font = font_spec("Pacifico", 1.25, update = TRUE))
onStop(function() {
  thematic_off()
})

ui <- fluidPage(
  imageOutput("CairoPNG")
)

render_image <- function(expr) {
  snapshotPreprocessOutput(renderImage(expr), function(value) {})
}

server <- function(input, output, session) {
  output$CairoPNG <- render_image({
    file <- thematic_save_plot(
      qplot(1:10) + ggtitle("Cairo::CairoPNG"),
      device = Cairo::CairoPNG,
      width = 800, height = 400
    )
    list(src = file, width = 800, height = 400)
  })
}

shinyApp(ui, server)
