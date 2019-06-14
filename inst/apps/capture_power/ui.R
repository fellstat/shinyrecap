

library(shiny)
library(shinyhelper)
library(magrittr)

srhelp <- function(x, ...){
  helper(x, ..., colour="lightgrey")
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Multiple Capture Re-Capture Power Analysis") %>% srhelp(content="main"),

  sidebarLayout(
    sidebarPanel(
       numericInput("N","Population Size",NA,min=2,max=1000000, step=1) %>% srhelp(content="N"),
       numericInput("nsim","# of Simulations",200, min=10, max=10000, step=1) %>% srhelp(content="nsim"),
       hr(),
       h4("Expected Capture Sample Sizes") %>% srhelp(content="capSizes"),
       uiOutput("capSizes"),
       actionButton("addList","Add"),
       actionButton("removeList","Remove"),
       hr(),
       h4("Heterogeneity"),
       selectInput("htype","Type",list("None","Normal")) %>% srhelp(content="htype"),
       conditionalPanel("input.htype == 'Normal'",
         numericInput("hetero",
                      "Odds ratio of capture of 90th percentile most likely to be captured compared to average",
                      NA,
                      1,
                      100) %>% srhelp(content="hetero")
       ),
       hr(),
       br(),
       actionButton("run","Run"),
       actionButton("cancel","Cancel")
    ),

    # Show a plot of the generated distribution
    mainPanel(
       tableOutput("quants") %>% srhelp(content="quants"),
       plotOutput("estHist") %>% srhelp(content="estHist"),
      plotOutput("heteroPlot") %>% srhelp(content="heteroPlot")
    )
  )
))
