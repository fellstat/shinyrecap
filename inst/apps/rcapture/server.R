

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
library(LCMCR)
library(dga)
library(ipc)
library(future)
library(promises)
library(coda)
plan(multiprocess)
#plan(sequential)

data(graphs3)
data(graphs4)
data(graphs5)

source("server-import.R")
source("server-loglinear.R")
source("server-pairwise.R")
source("server-dga.R")
source("server-lcmcr.R")

shinyServer(function(input, output, session) {

  getData <- serverImport(input, output, session)

  serverLogLinear(input, output, session, getData)

  serverPairwise(input, output, session, getData)

  serverDga(input, output, session, getData)

  serverLcmcr(input, output, session, getData)

})
