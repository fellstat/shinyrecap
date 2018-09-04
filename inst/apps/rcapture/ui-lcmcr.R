


renderLcmcr <- function(){
  tabPanel(
    "Bayesian Latent Class",
    sidebarLayout(
      sidebarPanel(

      ),
      mainPanel(
        br(),
        h3("Posterior Summaries"),
        withSpinner(tableOutput("lcmcrTable")),
        br(),
        h3("Posterior Distribution"),
        withSpinner(plotOutput("lcmcrPlot"))
      )
    )
  )
}
