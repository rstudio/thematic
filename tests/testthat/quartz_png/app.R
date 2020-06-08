library(shiny)
library(ggplot2)
library(thematic)

thematic_on("black", "white", font = font_spec("Pacifico", 1.25, update = TRUE))
onStop(function() {
  thematic_off()
})

ui <- fluidPage(
  imageOutput("quartz")
)

server <- function(input, output, session) {
  output$quartz <- snapshotPreprocessOutput(renderImage({
    file <- thematic_save_plot(
      qplot(1:10) + ggtitle("quartz png"),
      device = grDevices::png
    )
    list(src = file, width = 480, height = 480)
  }), function(value) {})
}

shinyApp(ui, server)
