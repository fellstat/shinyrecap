#' Convert Unique Event Identifier Event Data into Multiple Source Capture Recapture Histories
#'
#' @param datasets An ordered list of capture event datasets. The first j-1 columns of the jth
#'     dataset should be a 0 or 1 indicator of beng captured in a previous event.
#' @param count_name The name of the column holding the counts of the number of individuals.
#' @param cov_names A character vector of column names for covariates to stratify by.
#'
#' @return
#'  A dataframe. The first length(datasets) columns of which contain all possible permutations of capture histories.
#'  The "__count__" column contains the number of observed cases. Other columns are the stratifying covariates.
#'
#' @examples
#' # First sample
#' dat1 <- data.frame(
#'   count=c(55,43),
#'   c1=c("m","f"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Second sample
#' dat2 <- data.frame(
#'   id1=c(1,0,1,0),
#'   count=c(10,11,5,12),
#'   c1=c("m","m","f","f"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Third Sample
#' dat3 <- data.frame(
#'   id1=c(1,0,1,0,1,0,1,0),
#'   id2=c(1,1,1,1,0,0,0,0),
#'   count=c(2,3,4,3,2,30,4,30),
#'   c1=c("m","m","f","f","m","m","f","f"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Fourth Sample
#' dat4 <- data.frame(
#'   id1=c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0),
#'   id2=c(1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0),
#'   id3=c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),
#'   count=c(1,1,1,1,0,1,0,0,1,1,1,1,1,0,1,10),
#'   c1=c("m","m","f","f","m","m","f","f","m","m","f","f","m","m","f","f"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Combine 4 capture recatpure events together
#' datasets <- list(dat1,dat2,dat3,dat4)
#'
#' # Get four sample capture recapture histories from unique event observations
#' extract_histories(datasets, "count")
#'
#' # Stratify by a covariate
#' extract_histories(datasets, "count", cov_names="c1")
#'
#' # Two sample capture recapture
#' extract_histories(datasets[1:2], "count", cov_names="c1")
#' @export
extract_histories <- function(datasets, count_name, cov_names=c()){
  n_cap <- length(datasets)

  opts <- as.list(as.data.frame(replicate(n_cap,0:1)))
  opts[cov_names] <- unique(do.call(rbind, lapply(datasets, function(dat) dat[cov_names])))

  histories <- expand.grid(opts)
  histories <- histories[rowSums(histories[,1:n_cap]) != 0,]
  histories$`__count__` <- NA

  for(i in 1:nrow(histories)){
    hist <- histories[i,1:n_cap]
    cv <- histories[i,cov_names,drop=FALSE]
    last_list <- max(which(hist == 1))
    df <- datasets[[last_list]]
    if(last_list != 1)
      df <- df[apply(df[1:(last_list - 1)], 1, function(x) all(x == hist[1:(last_list - 1)])), , drop=FALSE]
    if(length(cov_names) > 0)
      df <- df[apply(df[cov_names], 1, function(x) all(x == cv)), , drop=FALSE]
    n <- sum(df[[count_name]])
    if(last_list != n_cap){
      for(j in (last_list + 1):n_cap){
        df2 <- datasets[[j]]
        hist2 <- as.numeric(hist[1:last_list])
        hist2 <- c(hist2, rep(0, j - 1 - last_list))
        df2 <- df2[apply(df2[1:(j - 1)], 1, function(x) all(x == hist2)), , drop=FALSE]
        if(length(cov_names) > 0)
          df2 <- df2[apply(df2[cov_names], 1, function(x) all(x == cv)), , drop=FALSE]
        n <- n - sum(df2[[count_name]])
      }
    }
    histories$`__count__`[i] <- n
  }
  histories
}
