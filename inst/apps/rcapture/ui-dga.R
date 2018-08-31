


renderDGA <- function(){
  tabPanel(
    "Bayesian Model Averaging",
    tabsetPanel(
      tabPanel(
        "Prior",
        sidebarLayout(
          sidebarPanel(
            h3("Prior On Model Complexity"),
            numericInput("dgaPriorDelta",
                         "Delta",
                         1,
                         min=0,
                         max = 1000000),
            br(),
            br(),
            h3("Prior On Population Size"),
            numericInput("dgaNMax",
                         "Maximum Population Size",
                         100000,
                         min=0,
                         max = 1000000),
            radioButtons("dgaPriorType","
                         Prior Distribution",
                         choices = c("Non-informative"="noninf",
                                   "Log-normal"="lnorm"),
                         selected="noninf"),
            conditionalPanel("input.dgaPriorType == \"lnorm\"",
              numericInput("dgaPriorMedian",
                           "Prior: Median",
                           7000,
                           min=0,
                           max = 1000000),
              numericInput("dgaPrior90",
                           "Prior: 90% Upper Bound",
                           10000,
                           min=0,
                           max = 1000000)
            )
          ),
          mainPanel(
            h3("Distribution"),
            plotOutput("dgaPrior"),
            h3("Cumulative Distribution"),
            plotOutput("dgaCumPrior")
          )
        )
      )
    )
  )
}
