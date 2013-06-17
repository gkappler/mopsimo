
getRel1.stan <- function (fit, parameter="theta") {
  E <- extract(fit)
  epsilon <- mean(aaply (E[[parameter]],
                          c(2),  # samples
                          function (x) sd(x)^2))  # se^2  mean of persons des posteriors
  tau <- mean(E[[paste("sigma_",parameter,sep="")]])^2  # mittlere Varianz der True Scores
  rel <- tau/(tau+epsilon)
  return(rel)
}
getRel3.stan <- function (fit, parameter="theta") {
  E <- extract(fit)
  tau <- diag(aaply (E[[paste("sigma_",parameter,sep="")]]^2,c(2,3),
                     function (x) mean(x)))  # mittlere Varianz der True Scores
  epsilon <- aaply(aaply (E[[parameter]],
                          c(2,3),  # samples
                          function (x) sd(x)^2),
                   2,mean)  # se^2  mean of persons des posteriors
  rel <- matrix(tau/(tau+epsilon),ncol=1)
  rownames(rel) <- motives
  colnames(rel) <- "rel"
  return(rel)
}
