


serverLcmcr <- function(input, output, session, getData){

  interruptor <- AsyncInterruptor$new()    # To signal STOP to the future

  resultVal <- reactiveVal()

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
    }) %...>% resultVal
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
    result <- data.frame(mean(post), quant[1], quant[2], quant[3])
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


}
