
library(shinyrecap)
library(shiny)
library(rhandsontable)

make_hot_table <- function(n){
    if(n==1){
        histories <- data.frame(as.numeric(NA))
        names(histories) <- "# Observed at Event 1"
    }else{
        opts <- as.list(as.data.frame(replicate(n-1,c("Yes","No"))))
        histories <- expand.grid(opts)
        #histories <- as.data.frame(histories[1:(nrow(histories)-1),])
        names(histories) <- paste("Captured at Event",1:(n-1))
        histories[[paste0("# Observed at Event ", n)]] <- as.numeric(NA)
    }
    tab <- rhandsontable(histories,rowHeaders=NULL)
    if(n > 1){
        tab <- tab %>% hot_col(1:(n-1), readOnly=TRUE)
    }
    tab <- tab %>% hot_validate_numeric(n, min=0) %>% hot_col(n, format = list(mantissa = 0))
    tab
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    observe_helpers()
    output[[paste0("hot", 1)]] <- renderRHandsontable(make_hot_table(1))
    output[[paste0("hot", 2)]] <- renderRHandsontable(make_hot_table(2))
    output[[paste0("hot", 3)]] <- renderRHandsontable(make_hot_table(3))
    output[[paste0("hot", 4)]] <- renderRHandsontable(make_hot_table(4))
    output[[paste0("hot", 5)]] <- renderRHandsontable(make_hot_table(5))
    output[[paste0("hot", 6)]] <- renderRHandsontable(make_hot_table(6))
    output[[paste0("hot", 7)]] <- renderRHandsontable(make_hot_table(7))
    output[[paste0("hot", 8)]] <- renderRHandsontable(make_hot_table(8))
    output[[paste0("hot", 9)]] <- renderRHandsontable(make_hot_table(9))
    output[[paste0("hot", 10)]] <- renderRHandsontable(make_hot_table(10))
    tab <- reactiveVal(NULL)
    output$hist_table <- renderTable({

        datasets <- list()
        for(i in 1:input$ncap){
            browser()
            df <- hot_to_r(input[[paste0("hot", i)]])
            if(anyNA(df[,ncol(df)])){
                tab(NULL)
                return(data.frame(Error=paste0("Please enter counts for all '# Observed at Event ", i,"'")))
            }
            for(j in seq_len(i-1)){
                df[[j]] <- df[[j]] == "Yes"
            }
            names(df)[i] <- "count"
            df[[i]] <- round(df[[i]])
            datasets[[i]] <- df
        }
        tab(extract_histories(datasets, count_name = "count"))
        if(any(tab()[["__count__"]] < -0.5)){
            output$warn <- renderText("Invalid Data: Capture event data implies some capture histories have less than 0 cases.")
        }else{
            output$warn <- renderText("")
        }
        tab()
    })

    output$download <- downloadHandler(
        filename = function() "histories.csv",
        content = function(file){
            write.csv(tab(), file, row.names=FALSE)
        }
    )

})
