# Desciption
Shiny User Interface for Multiple Source Capture Recapture Models


## Installation

To install the latest development version from the github repo run:
```
# If devtools is not installed:
# install.packages("devtools")

devtools::install_github("fellstat/shinyrecap", depends="Suggests")

```


## Usage

```
library(shinyrecap)

# Launch the Shiny application for multiple source capture re-capture
launchShinyPopSize()

# Launch the power analysis application
launchShinyPopSize("power")
```
