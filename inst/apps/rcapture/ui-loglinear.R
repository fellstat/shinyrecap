renderLogLinearSidebar <- function() {
  sidebarPanel(
    #br(),
    #uiOutput("model_select"),
    br(),
    br(),
    radioButtons(
      "OutputSelect",
      "Select Output:",
      choices = c(
        "M0 (No variation in capture probabilities)" = "M0",
        "Mt (Capture probability vary by time)" = "Mt",
        "Mh (Individuals have a heterogeneous prob. of capture)" = "Mh",
        "Mth (Both Mt and Mh)" = "Mth"
      ),
      selected = 'M0'
    ) %>% srhelp("OutputSelect"),
    conditionalPanel(
      "input.OutputSelect == 'Mh' || input.OutputSelect == 'Mth'",
      radioButtons(
        "Hetero",
        "Heterogeneity Type:",
        choices = c(
          "Lower Bound" = "Chao",
          "Poisson" = "Poisson",
          "Darroch" = "Darroch",
          "Gamma" = "Gamma",
          "Normal" = "Normal"
        ),
        selected = 'Chao'
      ) %>% srhelp("Hetero")
    ),
    div(style = "height:150px;")
  )
}

renderLogLinear <- function() {
  tabPanel("Log Linear Models",
    br(),
    h3("Log-linear Abundance Analysis"),
    tabsetPanel(
      tabPanel(
        "Model Comparison",
        h4("Population Size Estimates by Model:"),
        tableOutput("Abund") %>% srhelp("Abund"),
        p("M0 : All captures have the same probability and individuals are uniform."),
        p("Mt : Captures may have different probabilities and individuals are uniform."),
        p("Mh : All captures have the same probability and individuals may be heterogeneous."),
        p("Mth : Captures may have different probabilities and individuals may be heterogeneous.")
      ),
      tabPanel("Model Selection",
        sidebarLayout(
          renderLogLinearSidebar(),
          mainPanel(
            verbatimTextOutput("CI95"),
            verbatimTextOutput("tot1"),
            h4("95% Confidence Interval plots"),
            br(),
            plotOutput("CIPlot")
          )
        )
      ),
      tabPanel(
        "Descriptives",
        verbatimTextOutput("FreqStat"),
        plotOutput("HetPlot") %>% srhelp("HetPlot"),
        tags$hr()
      ),
      tabPanel(
        "Report",
        radioButtons(
          "llReportFormat",
          "Report Format:",
          choices = c(
            "HTML" = "html_document",
            "Word" = "word_document",
            "PDF" = "pdf_document"
          ),
          selected = 'html_document'
        ),
        checkboxInput("llReportCode","Include R Code",TRUE),
        checkboxGroupInput("llReportCheckBox",
                           "Report Contents",
                           c("Model Comparison","Model Selection","Descriptives"),
                           c("Model Comparison","Model Selection","Descriptives")
        ),
        downloadButton("llDownloadReport", "Download")
      )
    )
  )
}
