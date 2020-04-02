library(shiny)
library(ggplot2)
library(thematic)

thematic_begin("black", "white", font = font_spec("Pacifico", 1.25, update = TRUE))
onStop(function() {
  thematic_end()
})

ui <- fluidPage(
  imageOutput("png"),
  imageOutput("ragg_png"),
  imageOutput("jpeg"),
  imageOutput("svg")
)

width_px <- 800
height_px <- 400

doWithDevice <- function(expr, device, width = width_px, height = height_px, ...) {
  file <- thematic_with_device(
    expr, device = device,
    width = width, height = height,
    ...
  )
  list(src = file, width = width, height = height)
}

server <- function(input, output, session) {
  output$png <- renderImage({
    doWithDevice(
      qplot(1:10) + ggtitle("Cairo::CairoPNG"),
      Cairo::CairoPNG
    )
  })
  output$ragg_png <- renderImage({
    doWithDevice(
      qplot(1:10) + ggtitle("ragg::agg_png"),
      ragg::agg_png
    )
  })
  output$jpeg <- renderImage({
    doWithDevice(
      qplot(1:10) + ggtitle("grDevices::jpeg"),
      grDevices::jpeg, filename = tempfile(fileext = ".jpeg")
    )
  })
  output$svg <- renderImage({
    x <- doWithDevice(
      qplot(1:10) + ggtitle("grDevices::svg"),
      grDevices::svg, filename = tempfile(fileext = ".svg"),
      width = 8, height = 4
    )
    list(src = x$src)
  })
}

shinyApp(ui, server)
