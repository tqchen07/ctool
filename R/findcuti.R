#' Find cutpoint in one iteration
findcuti <- function(dataframe, survtime, survevent, cutvar){
    dt <- dataframe
    
    ## Get risk tables for all patients
    dt.order <- dt[order(dt[, survtime], -dt[, survevent]), ]#order by increasing survtime and decreasing survi status
    time <- n.risk <- n.risk.high <- n.event <- n.event.high <- c()
    i <- 1
    while(i <= nrow(dt.order)){
        if(dt.order[i, survevent]==1){
            timepoint <- dt.order[i, survtime]
            time <- c(time, timepoint)
            n.risk <- c(n.risk, nrow(dt.order)-nrow(dt.order[0:(i-1), ])) #number of subjects at risk prior to timepoint i
            n.event <- c(n.event, sum(dt.order[dt.order[ , survtime]== timepoint, survevent])) #number of event at timepoint i
            i <- i+sum(dt.order[ , survtime]== timepoint)
        }else{
            i <- i+1
        }
    }
    risk.tab <- data.frame(time=time, n.risk=n.risk, n.event=n.event) # risk table for all patients
    
    ## Get risk table for patients with greater than cutpoint[i] and compute the statistics
    cutpoint <- unique(dt[, cutvar])[order(unique(dt[, cutvar]))]
    k <- length(risk.tab$time)
    s2 <- ssquare(k)
    L <- Q <- p <- vector(length=length(cutpoint))
    for(i in 1:length(cutpoint)){
        dt.order.high <-dt.order[dt.order[, cutvar] >= cutpoint[i], ]
        #risk.tab$n.risk.high <- risk.tab$n.event.high <- rep(NA, nrow(risk.tab))
        
        risk.tab$n.risk.high <- unlist(lapply(1:k, 
                                              function(x) nrow(dt.order.high) - nrow(dt.order.high[dt.order.high[ , survtime] < risk.tab$time[x], ]))) #number of risk prior to time t
        risk.tab$n.event.high <- unlist(lapply(1:k, 
                                               function(x) sum(dt.order.high[dt.order.high[, survtime]==risk.tab$time[x], survevent]))) #number of event at time t
        
        L[i] <- sum(unlist(lapply(1:k, 
                                  function(x){ (risk.tab$n.event.high[x]-risk.tab$n.event[x]*risk.tab$n.risk.high[x]/risk.tab$n.risk[x]) })))
        
        Q[i] <- abs(L[i])/(sqrt(s2)*sqrt(k-1))
        p[i] <- getp(Q[i])
    }
    rslt <- data.frame(uniquecutvar = cutpoint, L=L, absL=abs(L), s2=s2, Q=Q, p=p)
    return(list(cutpoint=rslt$uniquecutvar[which.min(rslt$p)], 
                pvalue  =rslt$p[which.min(rslt$p)],
                result.matrix=rslt))
}