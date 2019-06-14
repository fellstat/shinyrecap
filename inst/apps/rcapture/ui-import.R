
renderImportDataPanel <- function() {
  tabPanel("Import Data",
           sidebarLayout(
             sidebarPanel(
               br(),
               fileInput("file", "Upload Your Data", multiple = F) %>% srhelp("file"),
               helpText("Select the data parameters below"),
               checkboxInput(
                 inputId = 'header',
                 label = 'Header',
                 value = TRUE
               ) %>% srhelp("header"),
               #checkboxInput(inputId = "stringAsFactors", "StringAsFactors", FALSE),
               radioButtons(
                 inputId = 'DataType',
                 label = 'Data Type',
                 choices = c("Aggregate", "Individual"),
                 selected = ''
               ) %>% srhelp("DataType"),
               radioButtons(
                 inputId = 'sep',
                 label = 'Separator',
                 choices = c(
                   Comma = ',',
                   Semicolon = ';',
                   Tab = '\t',
                   Space = ''
                 ),
                 selected = ','
               ) %>% srhelp("sep")
             ),
             mainPanel(uiOutput("DataTable"))
           ))
}
