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
          'This tool is designed to estimate key population size using multiple source capture recapture
          data implementing Log-Linear, Bayesian Model Averaging and Bayesian Latent Class models.',
          style = "text-align:justify;"
          )
      ),
      tags$h4("Log-Linear Models"),
      tags$p(
        p(
          'Log-linear models are a classic methodology for the analysis of multiple source capture recapture data. Variants are implemented that allow for varying capture probabilities across events, and heterogeneous capture probabilities among members of the population.',
          style = "text-align:justify;"
        )
      ),
      tags$h4("Bayesian Model Averaging"),
      tags$p(
        p(
          'This method allows the analyst to flexibly account for list dependency by creating models for all possible dependencies, and averaging over them in a way that is proportional to the probability that the dependence is correct.',
          style = "text-align:justify;"
        )
      ),
      tags$h4("Bayesian Latent Class"),
      tags$p(
        p(
          'The Bayesian latent class model deals with heterogeneity in a novel way. It posits that there are unobserved subgroups in the data with different capture probabilities for each capture event. The number of these groups and their probabilities are unknown. The algorithm uses a Bayesian framework to estimates these, along with the population size.',
          style = "text-align:justify;"
        )
      ),
      tags$hr(),
      h3(
        tags$a("Online Manual",
               href="https://fellstat.github.io/shinyrecap/"
        )
      )
    )
  )
}
