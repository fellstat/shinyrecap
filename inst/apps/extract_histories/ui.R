
# make_hot_table <- function(n){
#     if(n==1){
#         histories <- data.frame(as.numeric(NA))
#         names(histories) <- "# Observed at Event 1"
#     }else{
#         opts <- as.list(as.data.frame(replicate(n-1,c("Yes","No"))))
#         histories <- expand.grid(opts)
#         #histories <- as.data.frame(histories[1:(nrow(histories)-1),])
#         names(histories) <- paste("Captured at Event",1:(n-1))
#         histories[[paste0("# Observed at Event ", n)]] <- as.numeric(NA)
#     }
#     tab <- rhandsontable(histories,rowHeaders=NULL)
#     if(n > 1){
#         tab <- tab %>% hot_col(1:(n-1), readOnly=TRUE)
#     }
#     tab <- tab %>% hot_validate_numeric(n, min=0) %>% hot_col(n, format = list(mantissa = 0))
#     tab
# }

make_entry <- function(n){
    conditionalPanel(paste("input.ncap >=", n),
        fluidRow(
            column(8, align="center", offset = 2,
                h2(paste("Data From Capture Event",n))
            )
        ),
        br(),
        fluidRow(
            rHandsontableOutput(paste0("hot",n))#make_hot_table(n)
        ),
        br(),
        hr()
    )
}


srhelp <- function(x, ...){
    shinyhelper::helper(x, ..., colour="lightgrey")
}

library(shiny)
library(rhandsontable)
library(shinyhelper)
library(magrittr)

shinyUI(fluidPage(

    # Application title
    shiny::wellPanel(
    titlePanel("Convert Unique Event Indentifier Format Data Into Capture History Format") %>% srhelp(content="main"),
    br(),
    shiny::fluidRow(
        column(4, align="center", offset = 4,
            shiny::numericInput("ncap","Number of Captures:",value=3, min=2, max=10)
        )
    )
    ),
    hr(),
    make_entry(1),
    make_entry(2),
    make_entry(3),
    make_entry(4),
    make_entry(5),
    make_entry(6),
    make_entry(7),
    make_entry(8),
    make_entry(9),
    make_entry(10),
    hr(),
    hr(),
    shiny::tableOutput("hist_table"),
    p(textOutput("warn"), color="red"),
    downloadButton('download', 'Download')
))
