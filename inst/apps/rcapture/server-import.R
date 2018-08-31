

serverImport <- function(input, output, session){
  getData <- reactive({
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
    if (is.null(getData())) {
      return(NULL)
    }
    getData()
  })

  output$DataTable <- renderUI({
    if (is.null(getData())) {
      return(NULL)
    }
    tableOutput("table")
  })

  getData
}
