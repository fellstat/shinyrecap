

library(shiny)
library(shinyrecap)
library(DT)
library(caret)
library(Rcapture)                      # model capture recapture data (loglinear model)
library(conting)                       # model capture recapture data-Bayesian
library(ggplot2)                       # graphics
library(reshape)                       # reshaping dataset
library(MASS)
library(VGAM)
library(data.table)
library(CARE1)                         # model capture recapture data (sample coverage)
library(dga)
library(future)
library(promises)
plan(multiprocess)

data(graphs3)
data(graphs4)
data(graphs5)

source("server-import.R")
source("server-loglinear.R")
source("server-pairwise.R")
source("server-dga.R")

shinyServer(function(input, output, session) {

  getData <- serverImport(input, output, session)

  serverLogLinear(input, output, session, getData)

  serverPairwise(input, output, session, getData)

  serverDga(input, output, session, getData)

})
