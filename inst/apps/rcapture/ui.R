

library(shiny)
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
      tabsetPanel(
        renderLogLinear(),
        renderDga(),
        renderLcmcr(),
        renderPairwise()
      )
    )
  )

)
