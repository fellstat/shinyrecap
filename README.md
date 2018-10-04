# Desciption
Shiny User Interface for Multiple Source Capture Recapture Models.

Check out the manual at https://fellstat.github.io/shinyrecap/

## Installation

To install the latest development version from the github repo run:
```
# If devtools is not installed:
# install.packages("devtools")

devtools::install_github("fellstat/shinyrecap")

```

## Usage

```
library(shinyrecap)

# Launch the Shiny application for multiple source capture re-capture
launchShinyPopSize()

# Launch the power analysis application
launchShinyPopSize("power")
```
