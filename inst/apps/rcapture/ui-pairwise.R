

renderPairwise <- function(){
  tabPanel(
    "Pairwise",
    br(),
    h3("Pairwise Analysis (Chapman's)"),
    tableOutput("pairwise") %>% srhelp("pairwise")
  )
}
