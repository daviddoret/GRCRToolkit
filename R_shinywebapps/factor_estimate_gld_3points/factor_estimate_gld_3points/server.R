#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Declare user session variables
  factor_estimate_1 <- factor_estimate_gld_3points$new(
    estimated_range_min_value = -100,
    estimated_mode_value = 0,
    estimated_range_max_value = 100
  )

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

  output$plot_density <- renderPlot({

    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    print(input$estimated_mode_value)
    suppressWarnings(factor_estimate_1$fit_dist())
    factor_estimate_1$graph_density()

  })

  output$plot_quantile <- renderPlot({

    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    print(input$estimated_mode_value)
    factor_estimate_1$fit_dist()
    factor_estimate_1$graph_quantile()

  })


})
