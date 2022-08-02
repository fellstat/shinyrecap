pair_n <- function(n1, n2, m2){
  alpha <- .05
  n_chap <- (n1 + 1) * (n2 + 1)/(m2 + 1) - 1
  var_n_chap <- (n1 + 1) * (n2 + 1) * (n1 - m2) * (n2 - m2)/((m2 + 1)^2 * (m2 + 2))
  m12 <- n1 + n2 - m2
  const <- exp(qnorm(1 - alpha/2) * sqrt(log(1 + var_n_chap/(n_chap -
                                                     m12)^2)))
  lower <- m12 + (n_chap - m12) / const
  upper <- m12 + (n_chap - m12) * const
  data.frame(est=n_chap, lower=lower, upper=upper)
}

serverPairwise <- function(input, output, session, getData){

  output$pairwise <- renderTable({
    if (is.null(getData())) {
      return(NULL)
    }
    dat <- getData()
    if (input$DataType == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    result <- data.frame(c(),c(),c(),c())
    for(i in 1:(ncol(dat)-1)){
      for(j in (i+1):ncol(dat)){
        n1 <- sum(dat[[i]])
        n2 <- sum(dat[[j]])
        m2 <- sum(dat[[i]] + dat[[j]] > 1.5)
        result <- rbind(
          result,
           cbind(
             paste0("events_",i,"_",j),
             round(pair_n(n1,n2,m2))
             )
          )
      }
    }
    colnames(result) <- c("Comparison","Population Size", "95% CI Lower","95% CI Upper")
    result
    #result <- CARE1::estN.pair(CARE1::as.record(dat))
    #result <- result[,-2]
    #colnames(result)<- c("Population Size", "se", "95% CI Lower","95% CI Upper")
    #round(result)
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
