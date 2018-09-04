serverDga <- function(input, output, session, getData){
  dgaModel <- reactive({
    if (is.null(getData())) {
      return(NULL)
    }
    dat <- getData()
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
    list(x=x, values=values)
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
        xlim(c(0,max(x)))
    }) %...>% print
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
    if(!input$dgaSaturated){
      post <- post[-nrow(post), , drop=FALSE]
    }
    postN <- colSums(post)
    postN <- postN / sum(postN)
    ind <- cumsum(postN)  < .995
    plotPosteriorN(post[,ind], x[ind])
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
    mp <- mp / sum(mp)
    mp <- round(mp * 100, 3)

    data.frame(Interaction=formatGraphs(graphs),
               `Posterior Probability (%)` = mp,
               `Expected Pop. Size` = means,
               check.names=FALSE)
  })
}
