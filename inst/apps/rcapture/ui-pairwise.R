

renderPairwise <- function(){
  tabPanel(
    "Pairwise",
    br(),
    h3("Pairwise Abundance Analysis"),
    tableOutput("pairwise")
  )
}
