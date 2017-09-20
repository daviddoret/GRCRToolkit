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
  titlePanel("Factor estimate using GLD and 3 points"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
          numericInput(inputId = "estimated_range_min_value", label = "Est. range min value", value = 100000, min = 0, max = 1000000000, step = 100000, width = "100%"),
          numericInput(inputId = "estimated_mode_value", label = "Estimated mode", value = 200000, min = 0, max = 1000000000, step = 100000, width = "100%"),
          numericInput(inputId = "estimated_range_max_value", label = "Est. range max value", value = 300000, min = 0, max = 1000000000, step = 100000, width = "100%"),
          numericInput(inputId = "estimated_range_min_proba", label = "Est. range max value", value = .05, min = 0, max = 1, step = .01, width = "100%"),
          numericInput(inputId = "estimated_range_max_proba", label = "Est. range max value", value = .95, min = 0, max = 1, step = .95, width = "100%")
          ),
    mainPanel(
      tabsetPanel(
      tabPanel("Fitted Distribution",
               verbatimTextOutput("fit_dist_summary"),
               plotOutput("plot_density"),
               plotOutput("plot_probability"),
               plotOutput("plot_quantile")),
      tabPanel("Simulation Sample",
               plotOutput("plot_simulation_sample"),
               numericInput(inputId = "simulation_sample_sample_size", label = "Sample size", value = 20, min = 1, max = 1000, step = 1, width = "100%"),
               fluidRow(
                column(width = 3,
                       withTags(
                         div(
                             h3("Head"),
                             p("The first nth items")
                         )
                       ),
                       tableOutput("table_simulation_sample_head")),
                column(width = 3,
                       withTags(
                         div(
                           h3("Tail"),
                           p("The last nth items")
                         )
                       ),
                       tableOutput("table_simulation_sample_tail")),
                column(width = 3,
                       withTags(
                         div(
                           h3("Random"),
                           p("n random items")
                         )
                       ),
                       tableOutput("table_simulation_sample_random")))
      )
    )))))
