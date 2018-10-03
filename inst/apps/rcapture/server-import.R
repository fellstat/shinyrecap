

serverImport <- function(input, output, session){

  data <- reactive({
    file1 <- input$file
    if (is.null(file1)) {
      return(NULL)
    }
    dat <- read.csv(
      file = file1$datapath,
      sep = input$sep,
      header = input$header,
      stringsAsFactors = FALSE
    )
    nc <- ncol(dat)
    for(i in 1:nc){
      if(!is.numeric(dat[[i]]))
        dat[[i]] <- dat[[i]] == max(dat[[i]])
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

  getData <- function(disag=FALSE){
    df <- data()
    if(disag && input$DataType == "Aggregate")
      df <- disaggregate(df[-length(df)],df[[length(df)]])
    df
  }

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
