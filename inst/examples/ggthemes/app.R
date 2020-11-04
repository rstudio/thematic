library(shiny)
library(ggplot2)
library(ggthemes)
library(thematic)
library(dplyr)
library(showtext)
library(Cairo)
library(bslib)


ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  selectInput(
    "theme", "Choose a theme",
    choices = grep("^theme_", getNamespaceExports("ggthemes"), value = TRUE)
  ),
  fluidRow(
    column(6, tags$h4("Before thematic"), plotOutput("theme_off")),
    column(6, tags$h4("After thematic"), plotOutput("theme_on"))
  ),
  verbatimTextOutput("cache")
)

p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color =  Species)) +
  geom_point()

server <- function(input, output, session) {

  observe({
    theme_fun <- match.fun(input$theme)
    theme_set(theme_fun())
  }, priority = 2)

  output$theme_on <- renderPlot({
    input$theme
    thematic_with_theme(thematic_theme(font = "Pacifico"), print(p))
  })

  output$theme_off <- renderPlot({input$theme; print(p)})

}

shinyApp(ui, server)
