

library(shiny)

source("ui-intro.R")
source("ui-import.R")
source("ui-loglinear.R")
source("ui-dga.R")
source("ui-pairwise.R")





# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    "Multiple Source Capture Recapture Analysis",
    ## Define conditional panels for selected tab
    renderIntroPanel(),
    renderImportDataPanel(),
    tabPanel(
      "Analysis",
      tabsetPanel(
        renderLogLinear(),
        renderDGA(),
        renderPairwise()
      )
    )
  )

)
