#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Factor estimate using GLD and 3 points"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30),
       numericInput(inputId = "estimated_mode_value",
                    label = "Typical value (mode)",
                    value = 500000,
                    min = 0,
                    max = 1000000,
                    width = "25%")
    ),

    # Show a plot of the generated distribution
    mainPanel(
       #plotOutput("distPlot")
       #plotOutput("plot_all")
      fluidRow(
        column(10, plotOutput("plot_density")),
        column(10, plotOutput("plot_quantile"))
      )
    )
  )
))
