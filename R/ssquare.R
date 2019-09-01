#' get sum of square
ssquare <- function(k){
    a2 <- c()
    for(j in 1:k){
        temp <- c()
        for(h in 1:j){
            temp <- c(temp, 1/(k-h+1))
        }
        a2 <- c(a2, (1-sum(temp))^2)
    }
    s2 <- sum(a2)/(k-1)
    return(s2)
}