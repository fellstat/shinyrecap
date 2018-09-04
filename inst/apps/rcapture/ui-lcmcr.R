


renderLcmcr <- function(){
  tabPanel(
    "Bayesian Latent Class",
    sidebarLayout(
      sidebarPanel(
        h4("Prior on # of Groups (Stick Breaking Process)"),
        numericInput(
          "lcmcrK",
          "Maximum Number of Groups",
          10,
          min = 1,
          max = 50
        ),
        numericInput(
          "lcmcrShape",
          "Prior Shape",
          .25,
          min = .00001,
          max = 1000000
        ),
        numericInput(
          "lcmcrScale",
          "Prior Scale",
          .25,
          min = .00001,
          max = 1000000
        ),
        hr(),
        h4("MCMC Sampling Parameters"),
        numericInput(
          "lcmcrSamples",
          "# of Samples",
          10000,
          min = 10,
          max = 1000000
        ),
        numericInput(
          "lcmcrThinning",
          "Thinning",
          10,
          min = 1,
          max = 1000000
        ),
        numericInput(
          "lcmcrBurnin",
          "Burn in",
          10000,
          min = 1,
          max = 10000000
        ),
        hr(),
        actionButton("lcmcrCancel","Cancel"),
        actionButton("lcmcrRun","Run")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Posterior",
            br(),
            textOutput("lcmcrEssTextWarning"),
            tags$head(
              tags$style("#lcmcrEssTextWarning{color: red;
                         font-size: 20px;
                         font-style: italic;}"
              )
              ),
            h3("Posterior Summaries"),
            tableOutput("lcmcrTable"),
            br(),
            h3("Posterior Distribution"),
            plotOutput("lcmcrPlot")
          ),
          tabPanel(
            "Diagnostics",
            plotOutput("lcmcrTracePlot"),
            textOutput("lcmcrEssText"),
            tags$head(
              tags$style("#lcmcrEssText{font-size: 20px;}"
              )
            )
          )
        )
      )
    )
  )
}
