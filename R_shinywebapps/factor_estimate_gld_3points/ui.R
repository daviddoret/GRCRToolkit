#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(grctoolkit)
require(ggplot2)
require(colorspace)
require(labeling)
require(cowplot)
require(ggExtra)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Factor estimate using GLD and 3 points"),
  # Sidebar with a slider input for number of bins

  fluidRow(
  column(width=4,
    wellPanel(
      tags$h3("PERT-like 3 points estimate"),
      tags$p("Type in your min/max range estimate for the given confidence interval and the typical value (mode)"),
      fluidRow(
        column(width=4, numericInput(inputId = "estimated_range_min_value", label = "Min.", value = 200, min = 0, max = 1000000000, step = 1, width = "100%")),
        column(width=4, numericInput(inputId = "estimated_mode_value", label = "Mode", value = 300, min = 0, max = 1000000000, step = 1, width = "100%")),
        column(width=4, numericInput(inputId = "estimated_range_max_value", label = "Max.", value = 400, min = 0, max = 1000000000, step = 1, width = "100%"))
          ),
      tags$p("Type in the confidence interval for the above estimate"),
      fluidRow(
        column(width=4, numericInput(inputId = "estimated_range_min_proba", label = "Min.", value = .05, min = 0, max = 1, step = .01, width = "100%")),
        column(width=4, numericInput(inputId = "estimated_range_max_proba", label = "Max.", value = .95, min = 0, max = 1, step = .95, width = "100%"))
          ),
      tags$p("You may type in limits. For loss events, the minimum should be set to 0 (excluding opportunities or positive risks) or the minimum financial loss being considered a loss as per the organization's risk management policy. The maximum should be set to the highest possible risk, usually equaly to the company's equity value"),
      fluidRow(
        column(width = 4, numericInput(inputId = "limit_min_value", label = "Min.", value = 100, min = 0, max = 1000000000, step = 1, width = "100%")),
        column(width = 4, numericInput(inputId = "limit_max_value", label = "Max.", value = 500, min = 0, max = 1000000000, step = 1, width = "100%"))
      ),
      fluidRow(
        column(width = 4, selectInput(inputId = "limit_min_behavior", label = "Min. behavior", selected = "Limit", choices = c("Limit", "Replace", "Discard"))),
        column(width = 4, selectInput(inputId = "limit_max_behavior", label = "Max. behavior", selected = "Limit", choices = c("Limit", "Replace", "Discard")))
        ),
      tags$p("Graph navigation"),
      fluidRow(
        column(width=4, numericInput(inputId = "x_start", label = "X start", value = 0, min = 0, max = 1, step = 1, width = "100%")),
        column(width=4, numericInput(inputId = "x_end", label = "X end", value = 600, min = 0, max = 1000000000, step = 1, width = "100%"))
          )
      )
    ),
    column(width=8,
    mainPanel(
      tabsetPanel(
      tabPanel("Fitted Distribution",
               verbatimTextOutput("fit_distribution_summary"),
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
    ))))))
