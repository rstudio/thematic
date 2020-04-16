library(shiny)
library(ggplot2)
library(thematic)

# The device we're testing
device <- "CairoPNG"
ext <- "png"

ui <- fluidPage(
  imageOutput("ggplot"),
  imageOutput("lattice"),
  imageOutput("base")
)

render_image <- function(expr) {
  snapshotPreprocessOutput(renderImage(expr, deleteFile = FALSE), function(value) {})
}

image_info <- function(type, ext) {
  list(
    src = paste0(type, "-1.", ext),
    width = 600, height = 400
  )
}

server <- function(input, output, session) {
  # Knit all the Rmds
  # Note that this is done inside the server function
  # to avoid a timeout issue with shinytest::testApp()
  rmd_txt <- knitr::knit_expand("../template_device.Rmd", device = sprintf("'%s'", device))
  res <- callr::r(function(...) { knitr::knit2html(...) }, args = list(text = rmd_txt))

  output$ggplot <- render_image(image_info("ggplot", ext))
  output$lattice <- render_image(image_info("lattice", ext))
  output$base <- render_image(image_info("base", ext))

  onFlush(function() {
    unlink(dir(pattern = paste0("\\.", ext)))
  })
}

shinyApp(ui, server)
