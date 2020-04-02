library(shiny)
library(ggplot2)
library(thematic)

thematic_begin("black", "white", font = font_spec("Pacifico", 1.25, update = TRUE))
onStop(function() {
  thematic_end()
})

ui <- fluidPage(
  imageOutput("quartz")
)

server <- function(input, output, session) {
  output$quartz <- renderImage({
    file <- thematic_with_device(
      qplot(1:10) + ggtitle("quartz png"),
      device = grDevices::png, type = "quartz",
      width = 800, height = 400
    )
    list(src = file, width = 800, height = 400)
  })
}

shinyApp(ui, server)
