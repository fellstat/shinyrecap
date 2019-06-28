

library(shiny)
library(shinyrecap)
library(shinycssloaders)
library(shinyhelper)
library(magrittr)

source("ui-intro.R")
source("ui-import.R")
source("ui-loglinear.R")
source("ui-dga.R")
source("ui-lcmcr.R")
source("ui-pairwise.R")

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    "Multiple Source Capture Recapture Analysis",
    ## Define conditional panels for selected tab
    renderIntroPanel(),
    renderImportDataPanel(),
    tabPanel(
      p("Analysis"),
      shinyjs::useShinyjs(),
      tabsetPanel(
        renderLogLinear(),
        renderDga(),
        renderLcmcr(),
        renderPairwise()
      )
    ),
    tabPanel(
      "Report",
      radioButtons(
        "mainReportFormat",
        "Report Format:",
        choices = c(
          "HTML" = "html_document",
          "Word" = "word_document",
          "PDF" = "pdf_document"
        ),
        selected = 'html_document'
      ),
      checkboxInput("mainReportCode","Include R Code",TRUE),
      checkboxGroupInput("mainReportCheckBox",
                         "Report Contents",
                         c("Log Linear","Bayesian Model Averaging", "Bayesian Latent Class", "Pairwise"),
                         c("Log Linear","Bayesian Model Averaging", "Bayesian Latent Class", "Pairwise")
      ),
      downloadButton("mainDownloadReport", "Download")
    )
  )

)
