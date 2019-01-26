
library(shiny)
library(ggplot2)
library(ipc)
library(future)
library(promises)
library(shinyrecap)
plan(multiprocess)

shinyServer(function(input, output, session) {

  nLists <- reactiveVal(3)
  interruptor <- AsyncInterruptor$new()    # To signal STOP to the future
  sims <- reactiveVal(NULL)
  running <- reactiveVal(FALSE)

  output$capSizes <- renderUI({
    ipts <- lapply(1:nLists(), function(i) {
      numericInput(paste0("n",i),NA,NA, min=1, max=input$N - 1, step=1)
    })
    do.call(tagList, ipts)
  })

  observeEvent(input$addList,{
    nLists(nLists() + 1)
  })

  observeEvent(input$removeList,{
    if(nLists() == 3){
      showNotification("Must have at least three captures")
      return()
    }
    nLists(nLists() - 1)
  })

  observeEvent(input$run,{
    if(running())
      return()
    running(TRUE)

    N <- input$N
    if(is.null(N)){
      showNotification("Population size must be set")
      return()
    }
    n <- sapply(1:nLists(), function(i) input[[paste0("n",i)]])
    if(is.null(n) || any(is.na(n))){
      showNotification("Capture sizes must be set")
      return()
    }
    het <- input$hetero
    if(is.null(het) & input$htype == "Normal"){
      showNotification("Amount of heterogeneity must bet specified")
      return()
    }
    nsim <- input$nsim
    if(is.null(nsim)){
      showNotification("Number of simulations must bet specified")
      return()
    }

    #Run simulations
    progress <- AsyncProgress$new(session, message="Simulating...")
    fut <- future({
      p <- n / N
      func <- function(i) {
        interruptor$execInterrupts()
        progress$inc(1/nsim)
      }
      ee <- simulateEstimates(nsim, N, p, input$htype,  heteroPerc=het, monitorFunc = func)
      attr(ee,"N") <- N
      ee
    }) %...>% sims

    # Show notification on error or user interrupt
    fut <- catch(fut,
                 function(e){
                   sims(NULL)
                   showNotification(e$message)
                 })

    # When done with analysis, remove progress bar
    fut <- finally(fut, function(){
      progress$close()
      running(FALSE) # Declare done with run
    })
    #sims(ee)
    NULL
  })

  # Send interrupt signal to future
  observeEvent(input$cancel,{
    if(running())
      interruptor$interrupt("User Interrupt")
  })

  output$quants <- renderTable({
    if(is.null(sims())) return(NULL)
    ee <- sims()
    N <- attr(ee, "N")
    diffs <- ee[[1]] - N
    p <- c(.5,.6,.7,.8,.9,.95)
    qq <- quantile(na.omit(abs(diffs)), p=p)
    percErr <- abs(ee[[1]] - N) / N
    qqPerc <- 100 * quantile(na.omit(percErr), p=p)
    data.frame(`% of estimates within`=round(100 * p),
               `% Accuracy`=qqPerc,
               `Absolute Accuracy`=qq,
               check.names = FALSE)
  })


  output$estHist <- renderPlot({
    if(is.null(sims())) return(NULL)
    ee <- sims()
    N <- attr(ee, "N")
    p <- qplot(na.omit(ee[[1]]), bins=100) +
      geom_vline(xintercept = N, color="red") +
      xlab("Simulated Estimates")
    print(p)
  })

  output$heteroPlot <- renderPlot({
    if(is.null(sims())) return(NULL)
    if(is.null(input$hetero) || input$htype == "None") return(NULL)
    N <- input$N
    n <- sapply(1:nLists(), function(i) input[[paste0("n",i)]])
    p <- n / N
    t <- length(p)
    samps <- data.frame(probCap=rep(NA, N * t), capture=rep(NA, N * t))
    for(i in 1:t){
      het <- simulateHeteroNormal(N,input$hetero)
      cap <- shinyrecap:::.simulateSingleCapture(het, p[i])
      pc <- attr(cap,"pc")
      samps$probCap[(N * (i-1) + 1):(N*i)] <- pc
      samps$capture[(N * (i-1) + 1):(N*i)] <- i
    }
    samps$capture <- as.factor(samps$capture)
    pl <- qplot(x=probCap + rnorm(length(probCap), sd=.000001),
                color=capture,geom="density",
                data=samps) +
      xlab("Individual probability of capture") +
      xlim(c(0,max(samps$probCap)*1.1))
    plot(pl)
  })

})
