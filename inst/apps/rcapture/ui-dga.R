



renderDGA <- function() {
  tabPanel(
    "Bayesian Model Averaging",
    sidebarLayout(
      sidebarPanel(
        h4("Prior Model Complexity"),
        numericInput("dgaPriorDelta",
                     "Delta",
                     1,
                     min = 0,
                     max = 1000000),
        br(),
        br(),
        h4("Prior Population Size"),
        numericInput(
          "dgaNMax",
          "Maximum Population Size",
          100000,
          min = 0,
          max = 1000000
        ),
        radioButtons(
          "dgaPriorType",
          "
                 Prior Distribution",
          choices = c("Non-informative" = "noninf",
                      "Log-normal" = "lnorm"),
          selected = "noninf"
        ),
        conditionalPanel(
          "input.dgaPriorType == \"lnorm\"",
          numericInput(
            "dgaPriorMedian",
            "Prior: Median",
            7000,
            min = 0,
            max = 1000000
          ),
          numericInput(
            "dgaPrior90",
            "Prior: 90% Upper Bound",
            10000,
            min = 0,
            max = 1000000
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Prior",
            h3("Distribution"),
            withSpinner(plotOutput("dgaPrior")),
            h3("Cumulative Distribution"),
            withSpinner(plotOutput("dgaCumPrior"))
          ),
          tabPanel(
            "Posterior",
            br(),
            h3("Posterior Summaries"),
            withSpinner(tableOutput("dgaTable")),
            br(),
            h3("Posterior Distribution"),
            withSpinner(plotOutput("dgaPlot"))
          )
        )
      )
    )
  )
}
