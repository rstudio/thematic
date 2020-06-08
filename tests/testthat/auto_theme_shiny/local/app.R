library(shiny)
library(thematic)

theme_base <- thematic_theme("black", "white", accent = "purple")
theme_ggplot <- thematic_theme("black", "white", accent = "red")
theme_lattice <- thematic_theme("black", "white", accent = "orange")

ui <- fluidPage(
  plotOutput("base"),
  plotOutput("ggplot"),
  plotOutput("lattice")
)

server <- function(input, output, session) {

  output$base <- renderPlot({
    thematic_with_theme(theme_base, plot(1:10, 1:10, col = thematic_get_option("accent")))
  })

  output$ggplot <- renderPlot({
    thematic_with_theme(theme_ggplot, ggplot2::qplot(1:10, 1:10, color = 1:10))
  })

  output$lattice <- renderPlot({
    thematic_with_theme(theme_lattice, lattice::show.settings())
  })

}

shinyApp(ui, server)


