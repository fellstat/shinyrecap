serverLogLinear <- function(input, output, session, getData){
  output$FreqStat <- renderPrint({
    if (is.null(getData())) {
      return(NULL)
    }
    if (input$DataType == "Aggregate") {
      # If the user uploads an aggregate data, use teh model below
      freqstat <- descriptive(getData(), dfreq = TRUE)
    } else if (input$DataType == "Individual") {
      # If the user uploads individual level data, use the model below
      freqstat <- descriptive(getData(), dfreq = FALSE)
    }
    print(freqstat)                               # Print descriptive stats
  })


  # Output: abundance-using loglinear model(Rcapture)
  #--------------------
  output$Abund <- renderPrint({
    if (is.null(getData())) {
      return(NULL)
    }
    if (input$DataType == "Aggregate") {
      logli <- closedp(getData(), dfreq = TRUE) # All possible models
    } else if (input$DataType == "Individual") {
      logli <- closedp(getData(), dfreq = FALSE)
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
    if (is.null(getData())) {
      return(NULL)
    }
    if (input$DataType == "Aggregate") {
      logli <- closedp(getData(), dfreq = TRUE) # All possible models
    } else if (input$DataType == "Individual") {
      logli <- closedp(getData(), dfreq = FALSE)
    }
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
    agg <- input$DataType == "Aggregate"
    Conf.Int <- closedpCI.t(getData(),
                            dfreq = agg,
                            m = input$OutputSelect,
                            h = input$Hetero)$CI[1:3]
    names(Conf.Int)[1:3] <-
      c("Abundance", "Lower 95%", "Upper 95%")
    print(Conf.Int)
  })


  output$CIPlot <- renderPlot({
    if (is.null(getData())) {
      return(NULL)
    }
    if(is.null(input$OutputSelect))
      return(NULL)
    if (input$DataType == "Aggregate") {
      Conf.Intp <-
        closedpCI.t(getData(), dfreq = TRUE, m = input$OutputSelect, h = input$Hetero)
    } else if (input$DataType == "Individual") {
      Conf.Intp <-
        closedpCI.t(getData(), dfreq = FALSE, m = input$OutputSelect, h = input$Hetero)

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
    if (is.null(getData())) {
      return(NULL)
    }
    if (input$DataType == "Aggregate") {
      freqstat1 <-
        descriptive(getData(), dfreq = TRUE)   # explore heterogeneity (aggregate data)

    } else if (input$DataType == "Individual") {
      freqstat1 <-
        descriptive(getData(), dfreq = FALSE)   # explore heterogeneity (individual data)

    }
    plot(freqstat1)

  })

}
