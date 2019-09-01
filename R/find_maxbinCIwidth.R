#' Find the maximum width of confidence interval for binomial test
#'
#' @param n  sample size
#' @param alpha  type I error, defalt=0.05
#' @param method "exact" means exact binomial test, "z-test" means normal approxiamate binomial test
#' @return the confidence interval for each possible number of success and the maximum width of CI
#'
#' @export
#'
#' @examples
#' find_maxbinCIwidth(n=12, alpha=0.10, method="z-test")


find_maxbinCIwidth <- function(n, alpha=0.05, method=c("exact", "z-test")){
  if(!is.element(method, c("exact", "z-test"))){stop("Please choose a method from exact or z-test!")}
  tab <- data.frame()
  for(i in 0:n){
    if(method=="exact"){
      btest <- binom.test(x=i, n, alternative = "two.sided", conf.level=(1-alpha))
      vec <- c(i, round(i/n, 3), round(btest$conf.int, 3), round(btest$conf.int[2]-btest$conf.int[1], 3))
    }else if(method=="z-test"){
      p <- i/n
      x <- round(qnorm(1-alpha/2)*sqrt(p*(1-p)/n), 3)
      vec <- c(i, round(i/n, 3), round(p-x, 3), round(p+x, 3), round(2*x, 3))
    }
    tab <- rbind(tab, vec)
  }
  colnames(tab) <- c("Event", "Percent", paste0(100*(1-alpha), "%Lower"), paste0(100*(1-alpha), "%Upper"), "width")
  return(list(maximum_width=max(tab$width), table=tab))
}
