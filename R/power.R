


#' simulate capture heterogineity
#' @param N Population size
#' @param heteroPerc The increase in odds of capture for the perc 90th percentile most likely to be captured individuals, compared to the average individual.
#' @param perc The percentile to use.
#' @examples
#' het <- simulateHeteroNormal(100, 1.1)
#' hist(het)
#' @export
simulateHeteroNormal <- function(N, heteroPerc=1, perc=.90){
  hetero <- rnorm(N)
  sd <- log(heteroPerc) / qnorm(perc)
  hetero * sd
}

.simulateSingleCapture <- function(hetero, p){
  pcap <- function(intercept){
    lpred <- intercept + hetero
    pc <- exp(lpred) / (1 + exp(lpred))
    mean(pc) - p
  }

  inter <- uniroot(pcap,c(-100,100), extendInt = "yes")$root
  lpred <- inter + hetero
  pc <- exp(lpred) / (1 + exp(lpred))
  result <- runif(length(pc)) < pc
  attr(result, "pc") <- pc
  result
}

#' Simulate Capture Re-capture with heterogeneity
#' @param hetero The heterogineity
#' @param p A vector of capture event probabilities
#' @examples
#' het <- simulateHeteroNormal(1000, 1.1)
#' cap <- simulateCapture(het, p = c(.05,.1,.05,.1))
#' summary(cap)
#' @export
simulateCapture <- function(hetero, p){
  result <- matrix(NA,nrow=length(hetero), ncol=length(p))
  for(i in seq_along(p)){
    result[,i] <- .simulateSingleCapture(hetero, p[i])
  }
  result[rowSums(result) > 0,]
}

.estimate <- function(dat, h="Normal"){
  if(h != "None")
    est <- closedpCI.t(dat, m="Mth",h=h)
  else
    est <- closedpCI.t(dat, m="Mt")
  est$results[1,1:2]
}

#' Simulates capture re-capture estimates
#' @param nsim number of simulations
#' @param N Population size
#' @param p A vector of capture event probabilities
#' @param htype The type of capture heterogeneity. Either "None" or "Normal"
#' @param heteroPerc The increase in odds of capture for the perc 90th percentile most likely to be captured individuals, compared to the average individual.
#' @param monitorFunc A function called after every iteration. Useful for monitoring simulation progress.
#' @examples
#' library(ggplot2)
#' # Simulate estimates from the Mt model with no population heterogeneity
#' ests <- simulateEstimates(30,500,c(.1,.1,.1))
#'
#' # Simulate estimates from the Mth (Normal) model with no population heterogeneity.
#' ests2 <- simulateEstimates(30,500,c(.1,.1,.1), htype="Normal")
#'
#' df <- data.frame(est = ests[[1]],type="Mt")
#' df <- rbind(df, data.frame(est = ests2[[1]],type="Mth (Normal)"))
#' qplot(x=est, color=type, data=df, geom="density") +
#'   geom_vline(xintercept=500,color="purple")
#'
#' @export
simulateEstimates <- function(nsim,
                              N,
                              p,
                              htype="None",
                              heteroPerc=1,
                              monitorFunc = function(i) {}){
  estimates <- rep(NA, nsim)
  serrs <- rep(NA, nsim)
  for(i in 1:nsim){
    try({
      if(htype == "Normal")
        het <- simulateHeteroNormal(N, heteroPerc)
      else
        het <- rep(0,N)
      dat <- simulateCapture(het, p)
      est <- .estimate(dat, htype)
      estimates[i] <- est[1]
      serrs[i] <- est[2]
    })
    monitorFunc(i)
  }
  data.frame(estimates, serrs)
}



#simulateHeteroGamma <- function(N, t, shape, scale){
#  rgamma(N, shape=shape, scal=scale)
#}
