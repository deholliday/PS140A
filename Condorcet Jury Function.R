condorcet_jury <- function(N = 13, p = 0.5, m = floor(N/2)+1) {
  if(N %% 2 == 0) {
    return("ERROR: N must be odd to calculate majority")
  } else {
    results <- rep(NA,length(seq(m,N,1)))
    for(i in seq(m,N,1)){
      results[i-(m-1)] <- (factorial(N)/(factorial(N-i)*factorial(i)))*(p^i)*((1-p)^(N-i))
    }
    return(sum(results))
  }
}