renderIntroPanel <- function() {
  tabPanel(
    "Introduction",
    mainPanel(
      width = 8,
      br(),
      h3("Overview"),
      tags$p(
        p(
          'Capture recapture analysis was originally developed to estimate the size of wild animal populations using observed counts.
          However, the method has been  generalized for use in estimating the size of hard-to-reach populations such as
          Female Sex Workers (FSWs), Men having Sex with Men (MSM) and People Who Inject Drugs (PWID). It starts with "capturing and tagging"
          of indivuduals in a series of sampling occasions. This could be offering unique and easily identifyable gifts in each capture ocassion
          and collecting as much information as the study protocol allows to be able to identify recaptures without  identifying individuals.',
          style = "text-align:justify;"
        )
      ),


      tags$p(
        p(
          'This tool is designed to estimate key population size using multiple  source capture recapture
          data implementing loglinear models. It uses the',
          tags$b('Rcapture'),
          ' package in R. Please view',
          a(' Rcapture:Loglinear Models for Capture-Recapture in
            R' , href = 'https://www.jstatsoft.org/article/view/v019i05') ,
          'for full documentation.',
          style = "text-align:justify;"
          )
      ),

      tags$hr(),
      h3("Data Preparation"),
      tags$p(
        p(
          'Please carefully read the instruction below on how to prepare your dataset before importing it in to this tool'
        ),
        p(
          'In the current version of the app, either aggregate or indivudual capture history of two or more capture occasions can be used for estimating the population size. For an aggregate format, prepare your dataset in such a way that the first n-1 columns
          (where n is the number of capture occasions) represent capture histories (0=not captured, 1=captured) wheras the last column (n',
          tags$sup('th'),
          'column) represent capture frequency for the row. The individual capture history data are simply records of three columns each representing
          the capture occasion and rows representing individual capture history records. Columns can have any names but a two or three character
          name is sufficient. E.g for capture 1: CH1 or OC1. Look at the examples below.',
          style = "text-align:justify;"
          ),

        p('Aggregate data:'),
        h5(
          'Sixtyfive individuals were captured only in the first occasion and 15 unique individuals were captured in all the three
          ocassions and so on.',
          style = "text-align:justify;"
        ),

        tags$table(
          style = "border: none; padding: 10%; width: 60%;",
          tags$tr(
            tags$th("CH1"),
            tags$th("CH2"),
            tags$th("CH3"),
            tags$th("Frequency")
          ),
          tags$tr(tags$td("1"), tags$td("0"), tags$td("0"), tags$td("65")),
          tags$tr(tags$td("1"), tags$td("1"), tags$td("1"), tags$td("15")),
          tags$tr(tags$td("."), tags$td("."), tags$td("."), tags$td("."))
        ),
        br(),
        p('Individual level data'),
        tags$table(
          style = "border: none; padding: 10%; width: 40%;",
          tags$tr(tags$th("CH1"), tags$th("CH2"), tags$th("CH3")),
          tags$tr(tags$td("1"), tags$td("0"), tags$td("0")),
          tags$tr(tags$td("1"), tags$td("0"), tags$td("0")),
          tags$tr(tags$td("1"), tags$td("0"), tags$td("0")),
          tags$tr(tags$td("."), tags$td("."), tags$td("."))
        )
        ),

      tags$hr(),
      tags$p(
        p(
          'If you have prepared your dataset accordingly, please proceed to the',
          tags$span(style = "color:DodgerBlue", "Import Data"),
          'tab to import your data. Once you imported your dataset, please make sure to
          select the appropriate data type from the sidebar menu before proceding to the next step.'
        )
        )
      )
      )
}
