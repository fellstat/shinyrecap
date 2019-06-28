serverLogLinear <- function(input, output, session, getData){
  output$FreqStat <- renderPrint({
    if (is.null(getData())) {
      return(NULL)
    }
    freqstat <- descriptive(getData(TRUE), dfreq = FALSE)
    print(freqstat)                               # Print descriptive stats
  })


  # Output: abundance-using loglinear model(Rcapture)
  #--------------------
  output$Abund <- renderTable({
    if (is.null(getData())) {
      return(NULL)
    }
    logli <- closedp(getData(TRUE), dfreq = FALSE)
    normTFit <- try(closedpCI.t(getData(TRUE), m="Mth",h="Normal"))
    normFit <- try(closedpCI.t(getData(TRUE), m="Mh",h="Normal"))
    results <- as.data.frame(logli$results[1:10,-c(3,4,7)])
    colnames(results)[1] <- "Population Size"
    if(!inherits(normTFit, "try-error")){
      results <- rbind(results, normFit$results[,c(1,2,7,8)])
      row.names(results)[11] <- "Mth Normal"
    }
    if(!inherits(normFit, "try-error")){
      results <- rbind(results[1:6,], normFit$results[,c(1,2,7,8)],results[7:nrow(results),])
      row.names(results)[7] <- "Mh Normal"
    }

    ind <- which.min(results[,3])
    modnm <- row.names(results)[ind]
    modnm <- strsplit(modnm, " ")[[1]]
    m <- modnm[1]
    updateRadioButtons(session, "OutputSelect", selected = m)
    if(length(modnm) > 1){
      h <- switch(modnm[2],
                  Chao = "Chao",
                  Poisson2 = "Poisson",
                  Darrosh = "Darroch",
                  Gamma3.5 = "Gamma")
      updateRadioButtons(session, "Hetero", selected = h)
    }
    results[,1] <- as.integer(round(results[,1]))
    results[,2] <- as.integer(round(results[,2]))
    results
  }, rownames = TRUE)


  output$tot1 <- renderPrint({
    if (is.null(getData())) {
      return(NULL)
    }
    logli <- closedp(getData(TRUE), dfreq = FALSE)
    cat(noquote(paste (
      "Total number of captured units =", logli$n
    )), "\n") # just  remove the squared bracket from the output
    cat(noquote(paste (
      "Number of capture occasions =", logli$t
    )), "\n")

  })

  output$CI95 <- renderPrint({
    if (is.null(getData())) {
      return(NULL)
    }
    if(is.null(input$OutputSelect))
      return(NULL)
    #browser()
    agg <- input$DataType == "Aggregate"
    ci <- closedpCI.t(getData(),
                            dfreq = agg,
                            m = input$OutputSelect,
                            h = input$Hetero)
    if(input$Hetero == "Normal")
      ci <- ci$results[c(1,3,4)]
    else
      ci <- ci$CI[1:3]
    names(ci)<-
      c("Population Size", "Lower 95%", "Upper 95%")
    print(round(ci))
  })


  output$CIPlot <- renderPlot({
    if (is.null(getData())) {
      return(NULL)
    }
    if(is.null(input$OutputSelect))
      return(NULL)
    Conf.Intp <-
      closedpCI.t(getData(TRUE),
                  dfreq = FALSE,
                  m = input$OutputSelect,
                  h = input$Hetero)

    plotCI(
      Conf.Intp,
      main = paste(
        '95% Confidence Interval for the',
        input$OutputSelect,
        'model'
      ),
      col = "red"
    )
  })


  # Exploratory heterogeneity graph
  #--------------------
  output$HetPlot <- renderPlot({
    if (is.null(getData())) {
      return(NULL)
    }
    freqstat1 <- descriptive(getData(TRUE), dfreq = FALSE)
    plot(freqstat1)

  })

  getMarkdownReport <- function(includeDf=TRUE){
    objToString <- function(expr){
      paste(capture.output(dput(eval(expr))), collapse = "\n")
    }
    rmd <- ""
    if(includeDf){
    rmd <- paste0(rmd,
                  "
## Log-linear Analysis Report
### Input Data
```{r}
library(Rcapture)
library(shinyrecap)
df <- ",
                  objToString(getData()),
                  "
getData <- function(disag=FALSE){
    if(disag && ",objToString(input$DataType) ,"== \"Aggregate\")
      df <- disaggregate(df[-length(df)],df[[length(df)]])
    df
}
knitr::kable(df)
```
")
    }else{
      rmd <- paste0(rmd,
                    "
## Log-linear Analysis Report
### Input Data
```{r}
library(Rcapture)
library(shinyrecap)
```
")
    }
    if("Model Comparison" %in% input$llReportCheckBox){
      rmd <- paste0(rmd,
                    '
### Model Comparison
```{r}
    logli <- closedp(getData(TRUE), dfreq = FALSE)
    normTFit <- try(closedpCI.t(getData(TRUE), m="Mth",h="Normal"))
    normFit <- try(closedpCI.t(getData(TRUE), m="Mh",h="Normal"))
    results <- as.data.frame(logli$results[1:10,-c(3,4,7)])
    colnames(results)[1] <- "Population Size"
    if(!inherits(normTFit, "try-error")){
      results <- rbind(results, normFit$results[,c(1,2,7,8)])
      row.names(results)[11] <- "Mth Normal"
    }
    if(!inherits(normFit, "try-error")){
      results <- rbind(results[1:6,], normFit$results[,c(1,2,7,8)],results[7:nrow(results),])
      row.names(results)[7] <- "Mh Normal"
    }
    knitr::kable(results)
```\n
')
    }

    if("Model Selection" %in% input$llReportCheckBox){
      rmd <- paste(rmd,'
### Model Results for ',input$OutputSelect,' ', if(input$OutputSelect %in% c("Mt","Mth")) input$Hetero else "",'
```{r}
    agg <- ',objToString(input$DataType),' == "Aggregate"
    ci <- closedpCI.t(getData(),
                            dfreq = agg,
                            m = ',objToString(input$OutputSelect),',
                            h = ',objToString(input$Hetero),')
    if(',objToString(input$Hetero),' == "Normal"){
      ci <- ci$results[c(1,3,4)]
    }else{
      ci <- ci$CI[1:3]
    }
    names(ci)<-
      c("Population Size", "Lower 95%", "Upper 95%")
    print(round(ci))
```
')
    }

    if("Descriptives" %in% input$llReportCheckBox){
      rmd <- paste0(rmd,
                    '
### Log-linear Descriptives
```{r}
    freqstat <- descriptive(getData(TRUE), dfreq = FALSE)
    print(freqstat)
    plot(freqstat)
```
')
    }
    rmd
  }

  output$llDownloadReport <- downloadHandler(
    filename = function(){
      ext <- if(input$llReportFormat == "html_document"){
        "html"
      }else if(input$llReportFormat == "word_document"){
        "doc"
      }else{
        "pdf"
      }

      paste0("log_linear_report.", ext)
    },
    content = function(file){
      fm <- paste0('
---
title: "Log-Linear Model Report"
author: "shinyrecap"
output: ',input$llReportFormat,'
---
')
      setup <- paste0(
'
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = ',input$llReportCode,')
```
'
      )
      rmd <- paste0(fm, setup, getMarkdownReport())
      tempReport <- file.path(tempdir(), "report.Rmd")
      cat(rmd, file=tempReport)
      shinyjs::disable("llDownloadReport")
      note <- showNotification("Generating Report...", duration=NULL)
      rr <- try(rmarkdown::render(tempReport, output_file = file,
                                  envir = new.env(parent = globalenv())
      ))
      removeNotification(note)
      shinyjs::enable("llDownloadReport")
      rr
    }
  )
  list(
    getMarkdownReport=getMarkdownReport
  )
}
