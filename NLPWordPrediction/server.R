source('global.R')

shinyServer(function(input, output) {
        output$text <- renderText({
                paste(input$txt, 'foo', sep = ' ')
        })
        
#         output$main_plot <- renderPlot({
#                 
#                 hist(faithful$eruptions,
#                      probability = TRUE,
#                      breaks = as.numeric(input$n_breaks),
#                      xlab = "Duration (minutes)",
#                      main = "Geyser eruption duration")
#                 
#                 if (input$individual_obs) {
#                         rug(faithful$eruptions)
#                 }
#                 
#                 if (input$density) {
#                         dens <- density(faithful$eruptions,
#                                         adjust = input$bw_adjust)
#                         lines(dens, col = "blue")
#                 }
#                 
#         })
})