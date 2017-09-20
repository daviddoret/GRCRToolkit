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

  output$fit_dist_summary <- renderText({ factor_estimate_1$get_print_lines() })

  output$plot_density <- renderPlot({

    factor_estimate_1$estimated_range_min_value <- input$estimated_range_min_value
    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    factor_estimate_1$estimated_range_max_value <- input$estimated_range_max_value
    factor_estimate_1$estimated_range_min_proba <- input$estimated_range_min_proba
    factor_estimate_1$estimated_range_max_proba <- input$estimated_range_max_proba

    factor_estimate_1$plot_density()

  })

  output$plot_quantile <- renderPlot({

    factor_estimate_1$estimated_range_min_value <- input$estimated_range_min_value
    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    factor_estimate_1$estimated_range_max_value <- input$estimated_range_max_value
    factor_estimate_1$estimated_range_min_proba <- input$estimated_range_min_proba
    factor_estimate_1$estimated_range_max_proba <- input$estimated_range_max_proba

    factor_estimate_1$plot_quantile()

  })

  output$plot_probability <- renderPlot({

    factor_estimate_1$estimated_range_min_value <- input$estimated_range_min_value
    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    factor_estimate_1$estimated_range_max_value <- input$estimated_range_max_value
    factor_estimate_1$estimated_range_min_proba <- input$estimated_range_min_proba
    factor_estimate_1$estimated_range_max_proba <- input$estimated_range_max_proba

    factor_estimate_1$plot_probability()

  })

  output$plot_simulation_sample <- renderPlot({

    factor_estimate_1$estimated_range_min_value <- input$estimated_range_min_value
    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    factor_estimate_1$estimated_range_max_value <- input$estimated_range_max_value
    factor_estimate_1$estimated_range_min_proba <- input$estimated_range_min_proba
    factor_estimate_1$estimated_range_max_proba <- input$estimated_range_max_proba

    factor_estimate_1$plot_simulation_sample()

  })

  output$text_summary <- renderPrint({

    factor_estimate_1$estimated_range_min_value <- input$estimated_range_min_value
    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    factor_estimate_1$estimated_range_max_value <- input$estimated_range_max_value
    factor_estimate_1$estimated_range_min_proba <- input$estimated_range_min_proba
    factor_estimate_1$estimated_range_max_proba <- input$estimated_range_max_proba

    print(factor_estimate_1)

  })

  output$table_simulation_sample_head <- renderTable({
    factor_estimate_1$estimated_range_min_value <- input$estimated_range_min_value
    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    factor_estimate_1$estimated_range_max_value <- input$estimated_range_max_value
    factor_estimate_1$estimated_range_min_proba <- input$estimated_range_min_proba
    factor_estimate_1$estimated_range_max_proba <- input$estimated_range_max_proba
    factor_estimate_1$fit_dist()
    factor_estimate_1$simulate()
    factor_estimate_1$get_simulation_sample_head(n = input$simulation_sample_sample_size)
  })

  output$table_simulation_sample_tail <- renderTable({
    factor_estimate_1$estimated_range_min_value <- input$estimated_range_min_value
    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    factor_estimate_1$estimated_range_max_value <- input$estimated_range_max_value
    factor_estimate_1$estimated_range_min_proba <- input$estimated_range_min_proba
    factor_estimate_1$estimated_range_max_proba <- input$estimated_range_max_proba
    factor_estimate_1$fit_dist()
    factor_estimate_1$simulate()
    factor_estimate_1$get_simulation_sample_tail(n = input$simulation_sample_sample_size)
  })

  output$table_simulation_sample_random <- renderTable({
    factor_estimate_1$estimated_range_min_value <- input$estimated_range_min_value
    factor_estimate_1$estimated_mode_value <- input$estimated_mode_value
    factor_estimate_1$estimated_range_max_value <- input$estimated_range_max_value
    factor_estimate_1$estimated_range_min_proba <- input$estimated_range_min_proba
    factor_estimate_1$estimated_range_max_proba <- input$estimated_range_max_proba
    factor_estimate_1$fit_dist()
    factor_estimate_1$simulate()
    factor_estimate_1$get_simulation_sample_random(n = input$simulation_sample_sample_size)
  })

})
