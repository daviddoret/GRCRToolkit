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
      numericInput(inputId = "estimated_range_min_value",
                   label = "Est. range min value",
                   value = 100000,
                   min = 0,
                   max = 1000000000,
                   step = 100000,
                   width = "100%"),
      numericInput(inputId = "estimated_mode_value",
                   label = "Estimated mode",
                   value = 200000,
                   min = 0,
                   max = 1000000000,
                   step = 100000,
                   width = "100%"),
       numericInput(inputId = "estimated_range_max_value",
                    label = "Est. range max value",
                    value = 300000,
                    min = 0,
                    max = 1000000000,
                    step = 100000,
                    width = "100%"),
      numericInput(inputId = "estimated_range_min_proba",
                   label = "Est. range max value",
                   value = .05,
                   min = 0,
                   max = 1,
                   step = .01,
                   width = "100%"),
      numericInput(inputId = "estimated_range_max_proba",
                   label = "Est. range max value",
                   value = .95,
                   min = 0,
                   max = 1,
                   step = .95,
                   width = "100%")

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
