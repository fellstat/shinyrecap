

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Multiple Capture Re-Capture Power Analysis"),

  sidebarLayout(
    sidebarPanel(
       numericInput("N","Population Size",NA,min=2,max=1000000, step=1),
       numericInput("nsim","# of Simulations",200, min=10, max=10000, step=1),
       hr(),
       h4("Expected Capture Sample Sizes"),
       uiOutput("capSizes"),
       actionButton("addList","Add"),
       actionButton("removeList","Remove"),
       hr(),
       h4("Heterogeneity"),
       selectInput("htype","Type",list("None","Normal", "Gamma")),
       conditionalPanel("input.htype == 'Normal'",
         numericInput("hetero",
                      "Odds ratio of capture of 90th percentile most likely to be captured compared to average",
                      NA,
                      1,
                      100)
       ),
       hr(),
       br(),
       actionButton("run","Run"),
       actionButton("cancel","Cancel")
    ),

    # Show a plot of the generated distribution
    mainPanel(
       tableOutput("quants"),
       plotOutput("estHist"),
       plotOutput("heteroPlot")
    )
  )
))
