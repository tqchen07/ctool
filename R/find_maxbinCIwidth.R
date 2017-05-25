#' Find the maximum width of confidence interval for binomial test 
#'
#' @param n  sample size
#' @param conf.level  confidence interval, defalt=0.95
#' @return the confidence interval for each possible number of success and the maximum width of CI
#'  
#' @export
#'
#' @example 
#' find_maxbinCIwidth(n=12, conf.level=0.90)


find_maxbinCIwidth <- function(n, conf.level=0.95){
  tab <- data.frame()
  for(i in 0:n){
    btest <- binom.test(x=i, n, alternative = "two.sided", conf.level=conf.level)
    vec <- c(i, round(i/n, 3), round(btest$conf.int, 3), round(btest$conf.int[2]-btest$conf.int[1], 3))
    tab <- rbind(tab, vec)
  }
  colnames(tab) <- c("Event", "Percent", paste0(100*conf.level, "%Lower"), paste0(100*conf.level, "%Upper"), "width")
  return(list(maximum_width=max(tab$width), table=tab))
}
