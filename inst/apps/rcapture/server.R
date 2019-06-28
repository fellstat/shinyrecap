
source("import.R")
source("server-import.R")
source("server-loglinear.R")
source("server-pairwise.R")
source("server-dga.R")
source("server-lcmcr.R")

plan(multiprocess)
#plan(sequential)

data(graphs3)
data(graphs4)
data(graphs5)



shinyServer(function(input, output, session) {

  observe_helpers()

  getData <- serverImport(input, output, session)

  llmark <- serverLogLinear(input, output, session, getData)

  pairmark <- serverPairwise(input, output, session, getData)

  dgamark <- serverDga(input, output, session, getData)

  lcmcrmark <- serverLcmcr(input, output, session, getData)


  getMarkdownReport <- function(){
    objToString <- function(expr){
      paste(capture.output(dput(eval(expr))), collapse = "\n")
    }
    rmd <- paste0(
                  "
## Input Data
```{r}
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
    if("Log Linear" %in% input$mainReportCheckBox){
      rmd <- paste(rmd,llmark$getMarkdownReport(FALSE),"\n\n")
    }
    if("Bayesian Model Averaging" %in% input$mainReportCheckBox){
      rmd <- paste(rmd,dgamark$getMarkdownReport(FALSE),"\n\n")
    }
    if("Bayesian Latent Class" %in% input$mainReportCheckBox){
      rmd <- paste(rmd,lcmcrmark$getMarkdownReport(FALSE),"\n\n")
    }

    if("Pairwise" %in% input$mainReportCheckBox){
      rmd <- paste(rmd, pairmark$getMarkdownReport(FALSE),"\n\n")
    }

    rmd
  }

  output$mainDownloadReport <- downloadHandler(
    filename = function(){
      ext <- if(input$mainReportFormat == "html_document"){
        "html"
      }else if(input$mainReportFormat == "word_document"){
        "doc"
      }else{
        "pdf"
      }

      paste0("crc_report.", ext)
    },
    content = function(file){
      fm <- paste0('
---
title: "Multiple Source Capture Recapture Report"
author: "shinyrecap"
output: ',input$mainReportFormat,'
---
')
      setup <- paste0(
        '
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = ',input$mainReportCode,')
```
'
      )
      rmd <- paste0(fm, setup, getMarkdownReport())
      tempReport <- file.path(tempdir(), "report.Rmd")
      cat(rmd, file=tempReport)
      shinyjs::disable("mainDownloadReport")
      note <- showNotification("Generating Report...", duration=NULL)
      rr <- try(rmarkdown::render(tempReport, output_file = file,
                        envir = new.env(parent = globalenv())
      ))
      removeNotification(note)
      shinyjs::enable("mainDownloadReport")
      rr
    }
  )

})
