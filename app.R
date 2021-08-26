library(shinydashboard)
library(tidyverse)
library(shiny)
library(plotly)
library(shinythemes)

mydf <- data.frame(x = 1:5, y = c("a", "b", "c", "d", "e"))

mycolors <- c("blue", "#FFC125", "darkgreen", "darkorange")

shinyApp(
  ui = shinyUI(navbarPage(" Dashboard",
                          theme = shinytheme("cerulean"), windowTitle = "ABC",
                          tabPanel(
                            "Dashboard",
                            (fluidRow(column(width = 5, (plotlyOutput("plot4", height = 250)))))
                          )
  )),
  # Server.R
  server = function(input, output) {
    output$plot4 <- renderPlotly({
      plot_ly(
        data = mydf, labels = ~ y,
        values = ~ x,
        marker = list(colors = mycolors)
      ) %>%
        add_pie(hole = 0.0) %>%
        layout(
          title = "CT",
          xaxis = list(
            zeroline = F,
            showline = F,
            showticklabels = F,
            showgrid = F
          ),
          yaxis = list(
            zeroline = F,
            showline = F,
            showticklabels = F,
            showgrid = F
          )
        )
    })
  }
)