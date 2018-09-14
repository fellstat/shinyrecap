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
    print(ci)
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

}
