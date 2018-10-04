
source("import.R")
source("server-import.R")
source("server-loglinear.R")
source("server-pairwise.R")
source("server-dga.R")
source("server-lcmcr.R")

plan(multiprocess)
#plan(sequential)

data(graphs3)
data(graphs4)
data(graphs5)



shinyServer(function(input, output, session) {

  getData <- serverImport(input, output, session)

  serverLogLinear(input, output, session, getData)

  serverPairwise(input, output, session, getData)

  serverDga(input, output, session, getData)

  serverLcmcr(input, output, session, getData)

})
