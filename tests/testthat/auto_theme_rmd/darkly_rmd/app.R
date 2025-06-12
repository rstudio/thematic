library(shiny)
library(ggplot2)
library(thematic)

# The device we're testing
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

# Knit all the Rmds
# Note that this is done inside the server function
# to avoid a timeout issue with shinytest::testApp()
infile <- "darkly.Rmd"
rmd_txt <- knitr::knit_expand("../../template_theme.Rmd", theme = "darkly")
writeLines(rmd_txt, infile)
outfile <- rmarkdown::render(infile)

onStop(function() {
  unlink(infile)
  unlink(dir(pattern = paste0("\\.", ext)))
  unlink(
    c(infile, outfile, paste0(tools::file_path_sans_ext(infile), "_files")),
    recursive = TRUE
  )
})

server <- function(input, output, session) {

  output$ggplot <- render_image(image_info("ggplot", ext))
  output$lattice <- render_image(image_info("lattice", ext))
  output$base <- render_image(image_info("base", ext))
}

shinyApp(ui, server)
