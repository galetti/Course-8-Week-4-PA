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
    titlePanel("COVID19 in Germany - A Gender Study"),
    h4("This App will load some live data from the RKI Germany.
       Please wait a moment till the dataset is loaded"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("reg", "Region:",
                         c("Germany" = "ger",
                           "Berlin" = "ber",
                           "Munich" = "mun",
                           "Hamburg" = "ham",
                           "Cologne" = "col",
                           "Trier" = "tr",
                           "Kassel" = "ks")
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
