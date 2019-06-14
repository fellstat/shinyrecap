



renderDga <- function() {
  tabPanel(
    "Bayesian Model Averaging",
    sidebarLayout(
      sidebarPanel(
        h4("Prior Model Complexity"),
        numericInput("dgaPriorDelta",
                     "Delta",
                     1,
                     min = 0,
                     max = 1000000) %>% srhelp("dgaPriorDelta"),
        hr(),
        h4("Prior Population Size") %>% srhelp("dgaPrior"),
        numericInput(
          "dgaNMax",
          "Maximum Population Size",
          100000,
          min = 0,
          max = 1000000
        ) %>% srhelp("dgaNMax"),
        radioButtons(
          "dgaPriorType",
          "
                 Prior Distribution",
          choices = c("Non-informative" = "noninf",
                      "Log-normal" = "lnorm"),
          selected = "noninf"
        ) %>% srhelp("dgaPriorType"),
        conditionalPanel(
          "input.dgaPriorType == \"lnorm\"",
          numericInput(
            "dgaPriorMedian",
            "Prior: Median",
            7000,
            min = 0,
            max = 1000000
          ) %>% srhelp("dgaPriorMedian"),
          numericInput(
            "dgaPrior90",
            "Prior: 90% Upper Bound",
            10000,
            min = 0,
            max = 1000000
          ) %>% srhelp("dgaPrior90")
        ),
        hr(),
        checkboxInput(
          "dgaSaturated",
          "Include Saturated Model",
          FALSE
        ) %>% srhelp("dgaSaturated")
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
            "Posterior Population Size",
            textOutput("dgaSaturatedWarning"),
            tags$head(
              tags$style("#dgaSaturatedWarning{color: red;
                font-size: 20px;
                font-style: italic;}"
              )
            ),
            br(),
            h3("Posterior Summaries"),
            withSpinner(tableOutput("dgaTable")) %>% srhelp("dgaTable"),
            br(),
            h3("Posterior Distribution"),
            withSpinner(plotOutput("dgaPlot")) %>% srhelp("dgaPlot")
          ),
          tabPanel(
            "Posterior Model Probabilities",
            withSpinner(tableOutput("dgaModelPost")) %>% srhelp("dgaModelPost")
          )
        )
      )
    )
  )
}
