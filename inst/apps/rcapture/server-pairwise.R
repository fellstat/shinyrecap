serverPairwise <- function(input, output, session, getData){

  output$pairwise <- renderTable({
    if (is.null(getData())) {
      return(NULL)
    }
    dat <- getData()
    if (input$DataType == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    result <- estN.pair(as.record(dat))
    result <- result[,-2]
    colnames(result)<- c("Population Size", "se", "95% CI Lower","95% CI Upper")
    result
  },rownames = TRUE)

}
