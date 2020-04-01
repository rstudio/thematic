library(shiny)

# All the devices we're testing
devices <- c("png", "svg", "jpeg")

onStop(function() {
  lapply(devices, function(device) {
    unlink(dir(pattern = paste0("\\.", device)))
  })
})


# Modularized image output
imageOutputs <- function(device) {
  ns <- NS(device)
  tagList(
    imageOutput(ns("ggplot")),
    imageOutput(ns("lattice")),
    imageOutput(ns("base"))
  )
}

renderImages <- function(device) {
  moduleServer(device, function(input, output, session) {
    image_info <- function(type, device) {
      list(
        src = paste0(type, "-1.", device),
        width = 600, height = 400
      )
    }
    output$ggplot <- renderImage({
      image_info("ggplot", device)
    }, deleteFile = FALSE)
    output$lattice <- renderImage({
      image_info("lattice", device)
    }, deleteFile = FALSE)
    output$base <- renderImage({
      image_info("base", device)
    }, deleteFile = FALSE)
  })
}


tabs <- lapply(devices, function(x) tabPanel(x, imageOutputs(x)))
ui <- fluidPage(
  do.call(tabsetPanel, c(list(id = "foo"), tabs))
)

server <- function(input, output, session) {
  # Knit all the Rmds
  # Note that this is done inside the server function
  # to avoid a timeout issue with shinytest::testApp()
  invisible(lapply(devices, function(device) {
    rmd_txt <- knitr::knit_expand("template.Rmd", device = sprintf("'%s'", device))
    callr::r(function(...) { knitr::knit2html(...) }, args = list(text = rmd_txt))
  }))

  lapply(devices, renderImages)
}

shinyApp(ui, server)
