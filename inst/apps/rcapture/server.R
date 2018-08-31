

library(shiny)
library(shinyrecap)
library(DT)
library(caret)
library(Rcapture)                      # model capture recapture data (loglinear model)
library(conting)                       # model capture recapture data-Bayesian
library(ggplot2)                       # graphics
library(reshape)                       # reshaping dataset
library(MASS)
library(VGAM)
library(data.table)
library(CARE1)                         # model capture recapture data (sample coverage)
library(dga)
library(future)
library(promises)
plan(multiprocess)

data(graphs3)
data(graphs4)
data(graphs5)

shinyServer(function(input, output, session) {
  CRdata <- reactive({
    file1 <- input$file
    if (is.null(file1)) {
      return(NULL)
    }
    dat <- read.csv(
      file = file1$datapath,
      sep = input$sep,
      header = input$header,
      stringsAsFactors = input$stringAsFactors
    )
    nc <- ncol(dat)
    for(i in 1:nc){
      dat[[i]] <- as.integer(dat[[i]])
    }

    if(length(na.omit(dat[[nc]])) > 0){
      if(any(dat[[nc]] > 1)){
        updateRadioButtons(session,"DataType",selected="Aggregate")
      }else{
        updateRadioButtons(session,"DataType",selected="Individual")
      }
    }
    dat
  })

  # Output table
  #--------------------
  output$table <- renderTable({
    if (is.null(CRdata())) {
      return(NULL)
    }
    CRdata()
  })

  output$DataTable <- renderUI({
    if (is.null(CRdata())) {
      return(NULL)
    }
    tableOutput("table")
  })


  # Select model
  #--------------------
  #output$model_select <- renderUI({
  #  selectInput("modelselect",
  #              "Select Model:",
  #              choices = c("Loglinear" = "logli"))#,  options = list(create = TRUE) ,  "Bayesian"="bsn"
  #})


  # Output: frequency statistics
  #--------------------
  output$FreqStat <- renderPrint({
    if (is.null(CRdata())) {
      return(NULL)
    }
    if (input$DataType == "Aggregate") {
      # If the user uploads an aggregate data, use teh model below
      freqstat <- descriptive(CRdata(), dfreq = TRUE)
    } else if (input$DataType == "Individual") {
      # If the user uploads individual level data, use the model below
      freqstat <- descriptive(CRdata(), dfreq = FALSE)
    }
    print(freqstat)                               # Print descriptive stats
  })


  # Output: abundance-using loglinear model(Rcapture)
  #--------------------
  output$Abund <- renderPrint({
    if (is.null(CRdata())) {
      return(NULL)
    }
    if (input$DataType == "Aggregate") {
      logli <- closedp(CRdata(), dfreq = TRUE) # All possible models
    } else if (input$DataType == "Individual") {
      logli <- closedp(CRdata(), dfreq = FALSE)
    }
    ind <- which.min(logli$results[1:10,5])
    modnm <- row.names(logli$results)[ind]
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
    MOutPut <-
      setDT(as.data.frame(logli$results), keep.rownames = TRUE)[] # Extract model outputs
    colnames(MOutPut)[1] <- "Model"
    # MOutPut$CI95<-paste((round((MOutPut$abundance-MOutPut$stderr*1.96),2)), # Compute CI using stand error
    #                     round((MOutPut$abundance+ MOutPut$stderr*1.96),2), sep=",")
    print(MOutPut[,-c(4, 5, 8)], row.names = F, digits = 4)
  })


  output$tot1 <- renderPrint({
    if (is.null(CRdata())) {
      return(NULL)
    }
    if (input$DataType == "Aggregate") {
      logli <- closedp(CRdata(), dfreq = TRUE) # All possible models
    } else if (input$DataType == "Individual") {
      logli <- closedp(CRdata(), dfreq = FALSE)
    }
    cat(noquote(paste (
      "Total number of captured units =", logli$n
    )), "\n") # just  remove the squared bracket from the output
    cat(noquote(paste (
      "Number of capture occasions =", logli$t
    )), "\n")

  })

  output$CI95 <- renderPrint({
    if (is.null(CRdata())) {
      return(NULL)
    }
    if(is.null(input$OutputSelect))
      return(NULL)
    agg <- input$DataType == "Aggregate"
    Conf.Int <- closedpCI.t(CRdata(),
                             dfreq = agg,
                             m = input$OutputSelect,
                             h = input$Hetero)$CI[1:3]
    names(Conf.Int)[1:3] <-
      c("Abundance", "Lower 95%", "Upper 95%")
    print(Conf.Int)
  })


  output$CIPlot <- renderPlot({
    if (is.null(CRdata())) {
      return(NULL)
    }
    if(is.null(input$OutputSelect))
      return(NULL)
    if (input$DataType == "Aggregate") {
      Conf.Intp <-
        closedpCI.t(CRdata(), dfreq = TRUE, m = input$OutputSelect, h = input$Hetero)
    } else if (input$DataType == "Individual") {
      Conf.Intp <-
        closedpCI.t(CRdata(), dfreq = FALSE, m = input$OutputSelect, h = input$Hetero)

    }

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
    if (is.null(CRdata())) {
      return(NULL)
    }
    if (input$DataType == "Aggregate") {
      freqstat1 <-
        descriptive(CRdata(), dfreq = TRUE)   # explore heterogeneity (aggregate data)

    } else if (input$DataType == "Individual") {
      freqstat1 <-
        descriptive(CRdata(), dfreq = FALSE)   # explore heterogeneity (individual data)

    }
    plot(freqstat1)

  })

  output$pairwise <- renderTable({
    if (is.null(CRdata())) {
      return(NULL)
    }
    dat <- CRdata()
    if (input$DataType == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    result <- estN.pair(as.record(dat))
    result <- result[,-2]
    colnames(result)<- c("Abundance Estimate", "se", "95% CI Lower","95% CI Upper")
    result
  },rownames = TRUE)


  dgaModel <- reactive({
    if (is.null(CRdata())) {
      return(NULL)
    }
    dat <- CRdata()
    if (input$DataType == "Aggregate") {
      dat <- disaggregate(dat[,-ncol(dat)], dat[[ncol(dat)]])
    }
    rec <- make.strata(dat, locations=rep("a", nrow(dat1)))$overlap.counts
    rec <- array(rec, dim= rep(2,ncol(dat1)))
    nmax <- input$dgaNMax
    nmiss <- 0:nmax
    data(graphs3)
    ma <- bma.cr(rec, delta = 1/2^ncol(dat1), Nmissing = nmiss,graphs=graphs3)
    NULL
  })

  priorDist <- reactive({
    if( !is.null(CRdata())){
      dat <- CRdata()
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
    list(x=x, values=values)
  })

  output$dgaPrior <- renderPlot({
    if (is.null(CRdata())) {
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
        xlim(c(0,max(x)))
    }) %...>% print
  })

  output$dgaCumPrior <- renderPlot({
    if (is.null(CRdata())) {
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
        xlim(c(0,max(x)))
    }) %...>% print
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
    if (is.null(CRdata())) {
      return(NULL)
    }
    if(dgaPriorValid() != ""){
      showNotification(dgaPriorValid())
      return(NULL)
    }
    dat <- CRdata()
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
    future({
      x <- prior$x
      post <- bma.cr(rec,
                   delta=delta,
                   Nmissing=x - nobs,
                   logprior = log(prior$values),
                   graphs = graphs)
      list(prior=prior, post=post)
    })
  })

  output$dgaTable <- renderTable({
    dga <- dga()
    if(is.null(dga))
      return(NULL)
    dga <- value(dga)
    postN <- colSums(dga$post)
    x <- dga$prior$x
    mn <- sum(x * postN)
    med <- x[which(cumsum(postN) > .5)[1]]
    lower <- x[which(cumsum(postN) > .025)[1]]
    upper <- x[which(cumsum(postN) > .975)[1]]
    result <- data.frame(mn, med, lower, upper)
    names(result) <- c("Mean","Median","95% Lower","95% Upper")
    result
  })

  output$dgaPlot <- renderPlot({
    dga <- dga()
    if(is.null(dga))
      return(NULL)
    dga <- value(dga)
    x <- dga$prior$x
    post <- dga$post
    postN <- colSums(dga$post)
    ind <- cumsum(postN)  < .995
    plotPosteriorN(post[,ind], x[ind])
  })

})
