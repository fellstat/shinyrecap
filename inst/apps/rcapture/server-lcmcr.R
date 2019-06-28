


serverLcmcr <- function(input, output, session, getData){

  interruptor <- AsyncInterruptor$new()    # To signal STOP to the future

  resultVal <- reactiveVal()

  runParams <- reactiveValues()

  running <- reactiveVal(FALSE)
  observeEvent(input$lcmcrRun,{
    #Don't do anything if in the middle of a run
    if(running())
      return(NULL)
    running(TRUE)

    if (is.null(getData())) {
      return(NULL)
    }
    dat <- getData()
    if (input$DataType == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    K <- input$lcmcrK
    shape <- input$lcmcrShape
    invScale <- input$lcmcrScale
    thinning <- input$lcmcrThinning
    samples <- input$lcmcrSamples
    burnin <- input$lcmcrBurnin
    d2 <- as.data.frame(lapply(dat, as.factor))

    progress <- AsyncProgress$new(message="Running LCMCR Analysis")

    fut <- future({
      func <- function(subsamp, tot){
        # throw errors that were signal (if Cancel was clicked)
        interruptor$execInterrupts()
        # increment progress bar
        progress$inc(subsamp / tot)
      }

      sampler <- lcmCR(d2, tabular = FALSE, K = K, a_alpha = shape,
                          b_alpha = invScale, seed = "auto", buffer_size = samples*thinning + burnin + 1,
                          thinning = thinning)
      post <- lcmcrSample(sampler, burnin = burnin,
                                   samples = samples, thinning = thinning,
                                   output = FALSE, nMonitorBreaks=100, monitorFunc = func)
      result <- list(N=post)
      result
    }) %...>% (function(result){
      runParams$K <- K
      runParams$lcmcrShape <- shape
      runParams$lcmcrScale <- invScale
      runParams$lcmcrThinning <- thinning
      runParams$lcmcrSamples <- samples
      runParams$lcmcrBurnin <- burnin
      runParams$dat <- dat
      resultVal(result)
    })
    # Show notification on error or user interrupt
    fut <- catch(fut,
                 function(e){
                   resultVal(NULL)
                   print(e$message)
                   showNotification(e$message)
                 })

    # When done with analysis, remove progress bar
    fut <- finally(fut, function(){
      progress$sequentialClose()
      running(FALSE) # Declare done with run
    })

    # Return something other than the future so we don't block the UI
    NULL
  })

  # Send interrupt signal to future
  observeEvent(input$lcmcrCancel,{
    if(running())
      interruptor$interrupt("User Interrupt")
  })


  output$lcmcrTable <- renderTable({
    if(is.null(resultVal()))
      return(NULL)
    post <- resultVal()$N
    quant <- quantile(post, c(0.50, .025, 0.975))
    hdint <- HDInterval::hdi(post)
    result <- data.frame(mean(post), quant[1], hdint[1], hdint[2])
    names(result) <- c("Mean","Median","95% Lower","95% Upper")
    round(result)
  }, digits=0)

  output$lcmcrPlot <- renderPlot({
    if(is.null(resultVal()))
      return(NULL)
    Posterior <- resultVal()$N
    hist(Posterior, breaks=50)
  })

  output$lcmcrTracePlot <- renderPlot({
    if(is.null(resultVal()))
      return(NULL)
    Posterior <- resultVal()$N
    plot(Posterior,
         xlab="Sample #",
         ylab="Population Size",
         main="Trace Plot")
  })

  output$lcmcrEssText <- renderText({
    if(is.null(resultVal()))
      return(NULL)
    post <- resultVal()$N
    ess <- effectiveSize(post)
    paste0("Effective sample size = ", floor(ess))
  })

  output$lcmcrEssTextWarning <- renderText({
    if(is.null(resultVal()))
      return(NULL)
    post <- resultVal()$N
    ess <- effectiveSize(post)
    if(ess > 100)
      return(NULL)
    paste0("Warning: Low effective sample size. Increase number of samples or thinning. Effective sample size = ", floor(ess))
  })


  getMarkdownReport <- function(includeDf=TRUE){
    objToString <- function(expr){
      paste(capture.output(dput(eval(expr))), collapse = "\n")
    }
    if(is.null(resultVal())){
      showNotification("Latent Class Model has not been run")
      return('\n## No Bayesian Latent Class Results to Report\n')
    }
    rmd <- '\n\n## Bayesian Latent Class'
    if(includeDf){
      rmd <- paste0(rmd,
                    "
### Input Data
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
    }
    if(input$lcmcrReportCode){
      ll <- as.list(runParams)
      dat <- ll$dat
      ll$dat <- NULL
      rmd <- paste0(rmd,
'
```{r, eval=FALSE}
    library(LCMCR)
    library(shinyrecap)
    dat <- getData()
    if (', objToString(input$DataType),' == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    input <- ', objToString(ll), '
    K <- input$lcmcrK
    shape <- input$lcmcrShape
    invScale <- input$lcmcrScale
    thinning <- input$lcmcrThinning
    samples <- input$lcmcrSamples
    burnin <- input$lcmcrBurnin
    d2 <- as.data.frame(lapply(dat, as.factor))
    sampler <- lcmCR(d2, tabular = FALSE, K = K, a_alpha = shape,
                        b_alpha = invScale, seed = "auto", buffer_size = samples*thinning + burnin + 1,
                        thinning = thinning)
    post <- lcmcrSample(sampler, burnin = burnin,
                                 samples = samples, thinning = thinning,
                                 output = FALSE, nMonitorBreaks=100, monitorFunc = func)
    result <- list(N=post)
    resultVal <- function() result
```
')
    }

    rmd <- paste0(rmd,
'
```{r, include=FALSE}
library(LCMCR)
library(shinyrecap)
result <- ', objToString(resultVal()), '
resultVal <- function() result
```
')
    if("Posterior" %in% input$lcmcrReportCheckBox){
      rmd <- paste0(rmd,
'
### Posterior Distribution
Summaries:
```{r}
    post <- resultVal()$N
    quant <- quantile(post, c(0.50, .025, 0.975))
    hdint <- HDInterval::hdi(post)
    result1 <- data.frame(mean(post), quant[1], hdint[1], hdint[2])
    names(result1) <- c("Mean","Median","95% Lower","95% Upper")
    result1 %>% knitr::kable(digits=0)
```

Distribution:

```{r}
    hist(post, breaks=50)
```
')
    }
    if("Diagnostics" %in% input$lcmcrReportCheckBox){
      rmd <- paste0(rmd,
'
### Trace Plot
```{r}
    ess <- effectiveSize(post)

    plot(post,
         xlab="Sample #",
         ylab="Population Size",
         main="Trace Plot")
```

### `r paste0("Effective sample size = ", floor(ess))`

')
    }
    rmd
  }


  output$lcmcrDownloadReport <- downloadHandler(
    filename = function(){
      ext <- if(input$lcmcrReportFormat == "html_document"){
        "html"
      }else if(input$lcmcrReportFormat == "word_document"){
        "doc"
      }else{
        "pdf"
      }

      paste0("lcmcr_report.", ext)
    },
    content = function(file){
      if(is.null(resultVal())){
        showNotification("Latent class model has not been run yet")
      }
      fm <- paste0('
---
title: "Latent Class CRC Report"
author: "shinyrecap"
output: ',input$lcmcrReportFormat,'
---
')
      setup <- paste0(
        '
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = ',input$lcmcrReportCode,')
```
'
      )
      rmd <- paste0(fm, setup, getMarkdownReport())
      tempReport <- file.path(tempdir(), "report.Rmd")
      cat(rmd, file=tempReport)
      shinyjs::disable("lcmcrDownloadReport")
      note <- showNotification("Generating Report...", duration=NULL)
      rr <- try(rmarkdown::render(tempReport, output_file = file,
                                  envir = new.env(parent = globalenv())
      ))
      removeNotification(note)
      shinyjs::enable("lcmcrDownloadReport")
      rr
    }
  )

  list(
    getMarkdownReport=getMarkdownReport
  )
}
