serverDga <- function(input, output, session, getData){

  plot_dga_post <-function (weights, N)
  {
    #wts <- apply(weights, 1, sum)
    #df <- do.call("rbind",lapply(1:nrow(weights), function(i){
    #  data.frame(NN=N,dens = weights[i, ], lwd = wts[i] * 3,i=i)
    #}))
    df2 <- data.frame(N=N,d=apply(weights, 2, sum))
    ggplot() +
      #geom_line(aes(x=NN,y=dens,group=i, linewidth=lwd,linetype="By Model"), data=df) +
      #scale_linewidth_continuous(range=c(min(wts),max(wts)) * 2, guide=NULL) +
      geom_line(aes(x=N,y=d, linetype="Averaged"), linewidth=2,data=df2) +
      scale_linetype(name="Posterior:", guide="legend") +
      theme_bw() +
      theme(legend.position = "bottom") +
      ylab("Density") +
      xlab("N") +
      guides(linetype=guide_legend(override.aes = list(linewidth=1)))
  }


  priorDist <- reactive({
    if( !is.null(getData())){
      dat <- getData()
      if (input$DataType == "Aggregate"){
        nobs <- sum(dat[[length(dat)]])
        ncap <- ncol(dat) - 1
      }else{
        nobs <- nrow(dat)
        ncap <- ncol(dat)
      }
      updateNumericInput(session, "dgaPriorDelta",value = 1 / 2^ncap)
    }else
      return(NULL)
    if(input$dgaPriorType == "lnorm"){

      mu <- log(input$dgaPriorMedian)
      if(input$dgaPrior90 <= input$dgaPriorMedian){
        showNotification("Prior 90th percentile must be larger than the median")
        return(NULL)
      }
      ssd <- (log(input$dgaPrior90) - mu) / qnorm(.9)
      x <- 0:(input$dgaNMax - nobs) + nobs
      values <- dlnorm(x,mu,ssd)
    }else{
      x <- 1:(input$dgaNMax - nobs) + nobs
      values <- 1 / (1:(input$dgaNMax - nobs))
    }
    values <- values / sum(values)
    out <- list(x=x, values=values)
    out
  })

  output$dgaPrior <- renderPlot({
    if (is.null(getData())) {
      return(NULL)
    }
    prior <- priorDist()
    dgaPriorType <- input$dgaPriorType
    future({
      x <- prior$x
      values <- prior$values
      if(dgaPriorType == "lnorm")
        titl <- "Log-normal Prior"
      else
        titl <- "Non-informative Prior (p(x) ~ 1/ (Population Size - Sample Size))"
      lower90 <- x[min(which(cumsum(values) >= .1))]
      upper90 <- x[min(which(cumsum(values) >= .9))]
      p <- ggplot() +
        geom_line(aes(x=x,y=values)) +
        geom_vline(xintercept = lower90, color="red") +
        geom_vline(xintercept = upper90, color="red") +
        xlab("Population Size (red lines = 10th and 90th percentiles)") +
        ylab("Prior Probability") +
        ggtitle(titl) +
        theme_bw() +
        xlim(c(0,max(x)))
    }) %...>% (function(p){
      print(p)
    })
  })

  output$dgaCumPrior <- renderPlot({
    if (is.null(getData())) {
      return(NULL)
    }
    prior <- priorDist()
    dgaPriorType <- input$dgaPriorType
    future({

      x <- prior$x
      values <- prior$values
      if(dgaPriorType == "lnorm")
        titl <- "Log-normal Prior"
      else
        titl <- "Non-informative Prior (p(x) ~ 1/ (Population Size - Sample Size))"
      lower90 <- x[min(which(cumsum(values) >= .1))]
      upper90 <- x[min(which(cumsum(values) >= .9))]
      p <- ggplot() +
        geom_line(aes(x=x,y=cumsum(values))) +
        xlab("Population Size") +
        ylab("Prior Cumulative Probability") +
        ggtitle(titl) +
        theme_bw() +
        xlim(c(0,max(x)))
    }) %...>% (function(p){
      print(p)
    })
  })

  dgaPriorValid <- reactive({
    if(input$dgaPriorType == "lnorm"){
      if(input$dgaPrior90 <= input$dgaPriorMedian){
        return("Prior 90th percentile must be larger than the median")
      }
      if(input$dgaNMax <= input$dgaPrior90){
        return("Maximum population size percentile must be larger than the 90th percentile")
      }
    }
    ""
  })


  dga <- reactive({
    if (is.null(getData())) {
      return(NULL)
    }
    if(dgaPriorValid() != ""){
      showNotification(dgaPriorValid())
      return(NULL)
    }
    dat <- getData()
    if (input$DataType == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    if(ncol(dat) > 5){
      showNotification("Bayesian model averaging can only be performed on <= 5 sources")
      return(NULL)
    }
    if(ncol(dat) == 3)
      graphs <- graphs3
    else if(ncol(dat) == 4)
      graphs <- graphs4
    else
      graphs <- graphs5
    nobs <- nrow(dat)
    rec <- make.strata(dat, locations=rep("a",nrow(dat)))$overlap.counts
    rec <- array(rec, dim=rep(2, ncol(dat)))

    mu <- log(input$dgaPriorMedian)
    ssd <- (log(input$dgaPrior90) - mu) / qnorm(.9)
    nmax <- input$dgaNMax - nobs
    delta <- input$dgaPriorDelta
    prior <- priorDist()
    #future(
    #  {
        x <- prior$x
        post <- bma.cr(rec,
                       delta=delta,
                       Nmissing=x - nobs,
                       logprior = log(prior$values),
                       graphs = graphs)
        list(prior=prior, post=post)
    #  },
    #  seed = TRUE
    #)
  })

  output$dgaSaturatedWarning <- renderText({
    dga <- dga()
    if(is.null(dga))
      return(NULL)
    dga <- value(dga)
    post <- dga$post
    psat <- sum(post[nrow(post), ])
    if(psat >= .15){
      inc <- ""
      if(!input$dgaSaturated)
        inc <- " (not included in estimates) "
      return(paste0("Warning: The posterior probability of the saturated model",
                    inc,
                    " is ",
                    round(psat*100),
                    "%. Estimates may be unreliable."))
    }
    return(NULL)
  })

  output$dgaTable <- renderTable({
    dga <- dga()
    if(is.null(dga))
      return(NULL)
    dga <- value(dga)
    post <- dga$post
    if(!input$dgaSaturated){
      post <- post[-nrow(post), , drop=FALSE]
    }
    postN <- colSums(post)
    postN <- postN / sum(postN)
    x <- dga$prior$x
    mn <- sum(x * postN)
    med <- x[which(cumsum(postN) > .5)[1]]

    # HDI
    opt <- optimize(
      function(cut){
        abs(.05 - sum(postN*(postN <= cut)))
      },
      interval = c(0,max(postN))
    )
    inInterval <- which(postN > opt$minimum)
    lower <- x[inInterval[1]]
    upper <- x[inInterval[length(inInterval)]]

    #lower <- x[which(cumsum(postN) > .025)[1]]
    #upper <- x[which(cumsum(postN) > .975)[1]]
    result <- data.frame(mn, med, lower, upper)
    names(result) <- c("Mean","Median","95% Lower","95% Upper")
    round(result)
  }, digits=0)

  output$dgaPlot <- renderPlot({
    dga <- dga()
    if(is.null(dga))
      return(NULL)
    dga <- value(dga)
    x <- dga$prior$x
    post <- dga$post
    if(!input$dgaSaturated){
      post <- post[-nrow(post), , drop=FALSE]
    }
    postN <- colSums(post)
    postN <- postN / sum(postN)
    ind <- cumsum(postN)  < .995
    #browser()
    #plot(1:10)
    plot_dga_post(post[,ind], x[ind])
    #plotPosteriorN(post[,ind], x[ind])
  })

  output$dgaModelPost <- renderTable({
    dga <- dga()
    if(is.null(dga))
      return(NULL)
    dga <- value(dga)
    x <- dga$prior$x
    post <- dga$post

    dat <- getData()
    if (input$DataType == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    if(ncol(dat) > 5){
      showNotification("Bayesian model averaging can only be performed on <= 5 sources")
      return(NULL)
    }
    if(ncol(dat) == 3)
      graphs <- graphs3
    else if(ncol(dat) == 4)
      graphs <- graphs4
    else
      graphs <- graphs5
    if(!input$dgaSaturated){
      post <- post[-nrow(post), , drop=FALSE]
      graphs <- graphs[-length(graphs)]
    }
    mp <- rowSums(post)
    means <- apply(post, 1, function(p){
      p <- p / sum(p)
      sum(p * x)
    })
    means <- as.integer(round(means))
    mp <- mp / sum(mp)
    mp <- round(mp * 100, 3)

    data.frame(Interaction=formatGraphs(graphs),
               `Posterior Probability (%)` = mp,
               `Expected Pop. Size` = means,
               check.names=FALSE)
  })

  getMarkdownReport <- function(includeDf=TRUE){
    objToString <- function(expr){
      paste(capture.output(dput(eval(expr))), collapse = "\n")
    }

    rmd <- paste0('
## Bayesian Model Averaging
```{r}
if(!exists("input")) input <- list()
input$DataType <- ',objToString(input$DataType),'
input$dgaPriorType <- ',objToString(input$dgaPriorType),'
input$dgaPriorMedian <- ',objToString(input$dgaPriorMedian),'
input$dgaPriorDelta <- ',objToString(input$dgaPriorDelta),'
input$dgaNMax <- ',objToString(input$dgaNMax),'
input$dgaPrior90 <- ',objToString(input$dgaPrior90),'
input$dgaSaturated <- ',objToString(input$dgaSaturated),'
library(dga)
library(ggplot2)
library(shinyrecap)
```
')
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

  if("Prior" %in% input$dgaReportCheckBox){
    rmd <- paste0(rmd,'
### Prior Distribution
```{r}
    if( !is.null(getData())){
      dat <- getData()
      if (input$DataType == "Aggregate"){
        nobs <- sum(dat[[length(dat)]])
        ncap <- ncol(dat) - 1
      }else{
        nobs <- nrow(dat)
        ncap <- ncol(dat)
      }
    }else
      return(NULL)
    if(input$dgaPriorType == "lnorm"){

      mu <- log(input$dgaPriorMedian)
      ssd <- (log(input$dgaPrior90) - mu) / qnorm(.9)
      x <- 0:(input$dgaNMax - nobs) + nobs
      values <- dlnorm(x,mu,ssd)
    }else{
      x <- 1:(input$dgaNMax - nobs) + nobs
      values <- 1 / (1:(input$dgaNMax - nobs))
    }
    values <- values / sum(values)
    prior <- list(x=x, values=values)
    priorDist <- function() prior
    dgaPriorType <- input$dgaPriorType
    x <- prior$x
    values <- prior$values
    if(dgaPriorType == "lnorm"){
      titl <- "Log-normal Prior"
    }else{
      titl <- "Non-informative Prior (p(x) ~ 1/ (Population Size - Sample Size))"
    }
    lower90 <- x[min(which(cumsum(values) >= .1))]
    upper90 <- x[min(which(cumsum(values) >= .9))]
    p <- ggplot() +
      geom_line(aes(x=x,y=values)) +
      geom_vline(xintercept = lower90, color="red") +
      geom_vline(xintercept = upper90, color="red") +
      xlab("Population Size (red lines = 10th and 90th percentiles)") +
      ylab("Prior Probability") +
      ggtitle(titl) +
      theme_bw() +
      xlim(c(0,max(x)))
    print(p)
    p <- ggplot() +
        geom_line(aes(x=x,y=cumsum(values))) +
        xlab("Population Size") +
        ylab("Prior Cumulative Probability") +
        ggtitle(titl) +
        theme_bw() +
        xlim(c(0,max(x)))
    print(p)
```
')
  }

  if("Posterior" %in% input$dgaReportCheckBox){
    rmd <- paste0(rmd,'
### Posterior Distribution
```{r}
    dat <- getData()
    if (input$DataType == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    if(ncol(dat) == 3){
      data(graphs3)
      graphs <- graphs3
    }else if(ncol(dat) == 4){
      data(graphs4)
      graphs <- graphs4
    }else{
      data(graphs5)
      graphs <- graphs5
    }
    nobs <- nrow(dat)
    rec <- make.strata(dat, locations=rep("a",nrow(dat)))$overlap.counts
    rec <- array(rec, dim=rep(2, ncol(dat)))

    mu <- log(input$dgaPriorMedian)
    ssd <- (log(input$dgaPrior90) - mu) / qnorm(.9)
    nmax <- input$dgaNMax - nobs
    delta <- input$dgaPriorDelta
    prior <- priorDist()
      x <- prior$x
      post <- bma.cr(rec,
                     delta=delta,
                     Nmissing=x - nobs,
                     logprior = log(prior$values),
                     graphs = graphs)
    dga <- list(prior=prior, post=post)
        post <- dga$post
    if(!input$dgaSaturated){
      post <- post[-nrow(post), , drop=FALSE]
    }
    postN <- colSums(post)
    postN <- postN / sum(postN)
    x <- dga$prior$x
    mn <- sum(x * postN)
    med <- x[which(cumsum(postN) > .5)[1]]

    # HDI
    opt <- optimize(
      function(cut){
        abs(.05 - sum(postN*(postN <= cut)))
      },
      interval = c(0,max(postN))
    )
    inInterval <- which(postN > opt$minimum)
    lower <- x[inInterval[1]]
    upper <- x[inInterval[length(inInterval)]]

    #lower <- x[which(cumsum(postN) > .025)[1]]
    #upper <- x[which(cumsum(postN) > .975)[1]]
    result <- data.frame(mn, med, lower, upper)
    names(result) <- c("Mean","Median","95% Lower","95% Upper")
    result %>% knitr::kable(digits=0)
    postN <- colSums(post)
    postN <- postN / sum(postN)
    ind <- cumsum(postN)  < .995
    plotPosteriorN(post[,ind], x[ind])
```
')
  }
  if("Model Summaries" %in% input$dgaReportCheckBox){
    rmd <- paste0(rmd,'
### BMA Individual Model Summaries
```{r}
    if(!input$dgaSaturated){
      graphs <- graphs[-length(graphs)]
    }
    mp <- rowSums(post)
    means <- apply(post, 1, function(p){
      p <- p / sum(p)
      sum(p * x)
    })
    means <- as.integer(round(means))
    mp <- mp / sum(mp)
    mp <- round(mp * 100, 3)

    data.frame(Interaction=formatGraphs(graphs),
               `Posterior Probability (%)` = mp,
               `Expected Pop. Size` = means,
               check.names=FALSE) %>% knitr::kable()
```
')
  }
    rmd
  }


  output$dgaDownloadReport <- downloadHandler(
    filename = function(){
      ext <- if(input$dgaReportFormat == "html_document"){
        "html"
      }else if(input$dgaReportFormat == "word_document"){
        "doc"
      }else{
        "pdf"
      }

      paste0("bma_report.", ext)
    },
    content = function(file){
      fm <- paste0('
---
title: "Bayesian Model Averaging Report"
author: "shinyrecap"
output: ',input$dgaReportFormat,'
---
')
      setup <- paste0(
        '
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = ',input$dgaReportCode,')
```
'
      )
      rmd <- paste0(fm, setup, getMarkdownReport())
      tempReport <- file.path(tempdir(), "report.Rmd")
      cat(rmd, file=tempReport)
      shinyjs::disable("dgaDownloadReport")
      note <- showNotification("Generating Report...", duration=NULL)
      rr <- try(rmarkdown::render(tempReport, output_file = file,
                                  envir = new.env(parent = globalenv())
      ))
      removeNotification(note)
      shinyjs::enable("dgaDownloadReport")
      rr
    }
  )
  list(
    getMarkdownReport=getMarkdownReport
  )
}
