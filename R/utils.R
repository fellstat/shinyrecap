
#' disaggregate data
#' @param dat a data.frame
#' @param counts frequency counts for each row
#' @export
disaggregate <- function(dat, counts){
  dat <- cbind(dat,counts)
  result <- do.call(rbind, apply(dat,1 ,function(r){
    r1 <- r[-length(r)]
    n <- r[length(r)]
    d <- as.data.frame(matrix(NA, nrow=n, ncol=length(r1)))
    if(n > 0){
      for(i in seq_along(r1)){
        d[,i] <- r1[i]
      }
    }
    d
  }))
  as.data.frame(result)
}


#' Format graphs
#' @param graphs the graphs
#' @export
formatGraphs <- function(graphs){
  sapply(graphs, function(x){
    v <- sapply(x$C, function(y){
      paste0(y, collapse=" - ")
    })
    paste0(v, collapse=" | ")
  })
}


#' Perform LCMCR sampling with a monitor function
#' @param object the samples
#' @param burnin MCMC burn in
#' @param samples number of samples
#' @param thinning MCMC thinning
#' @param clear_buffer buffer clear buffer of object
#' @param output output progress
#' @param nMonitorBreaks number of times to call the monitor function
#' @param monitorFunc A function called nMonitorBreaks times taking the number of samples to be taken, and the total samples
#' @details
#' An edited version of \code{lcmCR_PostSampl}
#' @export
lcmcrSample <- function(object, burnin = 10000, samples = 1000, thinning = 10,
                        clear_buffer = FALSE, output=TRUE,
                        nMonitorBreaks=100,
                        monitorFunc = function(subs, tot){}){
  object$Update(burnin, output)
  object$Change_SubSamp(thinning)
  object$Change_Trace_Length(samples)
  if (!("n0" %in% object$Get_Trace_List())) {
    object$Set_Trace("n0")
  }
  if (clear_buffer) {
    object$Reset_Traces()
  }
  object$Activate_Tracing()
  tot <- samples * thinning
  breaks <- ceiling(seq(from=1, to = tot, length.out=nMonitorBreaks))
  partN <- diff(c(0,breaks))
  for(i in 1:length(partN)){
    if(partN[i] > 0){
      object$Update(partN[i], output)
      monitorFunc(partN[i], tot)
    }
  }
  N <- object$Get_Trace("n0") + object$n
  return(as.numeric(N))
}



#' A simple wrapper for shinyhelper::helper
#' @param x The shiny object to decorate
#' @param content the name of the help
#' @param ... additional parameters for shinyhelper::helper
#' @export
srhelp <- function(x, content, ...){
  shinyhelper::helper(x, ..., content=content, colour="lightgrey")
}
