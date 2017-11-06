#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Declare user session variables
  factor_estimate_1 <- factor_estimate_gld_3points$new(
    estimated_range_min_value = -100,
    estimated_mode_value = 0,
    estimated_range_max_value = 100
  )

  update_factor_estimate <- reactive({

    factor_estimate_1$estimated_range_min_value <- input$estimated_range_min_value
    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    factor_estimate_1$estimated_range_max_value <- input$estimated_range_max_value
    factor_estimate_1$estimated_range_min_proba <- input$estimated_range_min_proba
    factor_estimate_1$estimated_range_max_proba <- input$estimated_range_max_proba

    factor_estimate_1$limit_min_value <- input$limit_min_value
    factor_estimate_1$limit_min_behavior <- input$limit_min_behavior
    factor_estimate_1$limit_max_value <- input$limit_max_value
    factor_estimate_1$limit_max_behavior <- input$limit_max_behavior

    factor_estimate_1$fit_distribution()
    factor_estimate_1$simulate()

    })

  output$fit_distribution_summary <- renderText({
    update_factor_estimate()
  })

  output$plot_density <- renderPlot({
    update_factor_estimate()
    factor_estimate_1$plot_density(x_start = input$x_start, x_end = input$x_end)
  })

  output$plot_quantile <- renderPlot({
    update_factor_estimate()
    factor_estimate_1$plot_quantile()
  })

  output$plot_probability <- renderPlot({
    update_factor_estimate()
    factor_estimate_1$plot_probability(x_start = input$x_start, x_end = input$x_end)
  })

  output$plot_simulation_sample <- renderPlot({
    update_factor_estimate()
    factor_estimate_1$plot_simulation_sample()
  })

  output$text_summary <- renderPrint({
    update_factor_estimate()
    print(factor_estimate_1)
  })

  output$table_simulation_sample_head <- renderTable({
    update_factor_estimate()
    factor_estimate_1$get_simulation_sample_head(n = input$simulation_sample_sample_size)
  })

  output$table_simulation_sample_tail <- renderTable({
    update_factor_estimate()
    factor_estimate_1$get_simulation_sample_tail(n = input$simulation_sample_sample_size)
  })

  output$table_simulation_sample_random <- renderTable({
    update_factor_estimate()
    factor_estimate_1$get_simulation_sample_random(n = input$simulation_sample_sample_size)
  })

})
