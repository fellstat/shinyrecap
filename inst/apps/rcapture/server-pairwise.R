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
    round(result)
  },rownames = TRUE, digits=0)

  getMarkdownReport <- function(...){
    objToString <- function(expr){
      paste(capture.output(dput(eval(expr))), collapse = "\n")
    }
    paste0('

## Pairwise Analysis

```{r}
    library(shinyrecap)
    library(CARE1)
    dat <- getData()
    if (',objToString(input$DataType) ,' == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    result3 <- estN.pair(as.record(dat))
    result3 <- result3[,-2]
    colnames(result3)<- c("Population Size", "se", "95% CI Lower","95% CI Upper")
    result3 %>% knitr::kable(digits=0)
```
')
  }
  list(getMarkdownReport=getMarkdownReport)
}
