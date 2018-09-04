

renderPairwise <- function(){
  tabPanel(
    "Pairwise",
    br(),
    h3("Pairwise Analysis"),
    tableOutput("pairwise")
  )
}
