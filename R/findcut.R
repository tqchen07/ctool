#' Find optimal cutpoints for continuous covariates based on the method
#' proposed by Contal and O'Quigley
#'
#' @param dataframe Data to be used
#' @param survtime Survival time
#' @param survevent Survival event
#' @param cutvar Variable to be dichotomized
#'
#' @references
#' Contal and O’Quigley, 1999
#' Williams et al., 2006
#'
#' @export
findcut <- function(dataframe, survtime, survevent, cutvar){
    dt <- dataframe[!is.na(dataframe[, cutvar]), ] #only keep patients with none-missing cutvar
    r <- nrow(dt)
    cutpoint <- pvalue <- vector(length=r)
    for(i in 1:r){
        dt.temp <- dt[-i, ]
        temp <- findcuti(dt.temp, survtime, survevent, cutvar)
        cutpoint[i] <- temp$cutpoint
        pvalue[i]   <- temp$pvalue
    }
    return(findmode(cutpoint))
}








