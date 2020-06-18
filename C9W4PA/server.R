#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(jsonlite)
library(ggplot2)    

shinyServer(function(input, output) {
    # Load data
    jsonData = readRDS(file = "data/jsonData.Rds")
    
    output$distPlot <- renderPlot({
        
        reg <- switch(input$reg,
                      "ger" = ".",
                      "ber" = "Berlin",
                      "mun" = "SK.M.nchen",
                      "ham" = "SK.Hamburg",
                      "col" = "SK.K.ln",
                      "tr" = "SK Trier",
                      "ks" = "SK Kassel",
                      ".")
        # generate bins based on input$bins from ui.R
        data <- jsonData[grepl(reg, jsonData$features.properties$Landkreis),]
        
        data$features.properties$Meldedatum <- data$features.properties$Meldedatum %>%
            as.Date()
        
        ggplot(data = data, aes(x = features.properties$Meldedatum,
                   col = features.properties$Geschlecht)) +
            geom_histogram(bins = 100) +
            labs(x = "Date",
                 y = "New infections",
                 title = reg,
                 col = "Sex")
        
    })
    
})
