#' Get p value
getp <- function(q, lim=1e3){
    if(q < 0.2) return(1)
    
    ind  <- seq(1, lim, by=1)
    temp <- (-1)^(ind+1)*exp(-2*ind^2*q^2)
    p    <- 2*sum(temp)
    return(p)
}