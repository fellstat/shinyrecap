
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
    for(i in seq_along(r1)){
      d[,i] <- r1[i]
    }
    d
  }))
  as.data.frame(result)
}

