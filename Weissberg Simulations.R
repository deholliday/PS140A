weissberg_sim <- function(d_mean = 0, d_sd = 1, r_mean = 0, r_sd = 1, n = 435, sims = 100) {
  results <- matrix(NA, sims, 2)
  for(i in 1:sims){
    d <- rnorm(n, d_mean, d_sd)
    r <- rnorm(n, r_mean, r_sd)
    dr <- as.data.frame(cbind(d,r)) %>%
      mutate(diff = abs(d-r)) %>%
      summarise(formula_1 = sum(diff)/n,
                formula_2 = abs(sum(d)-sum(r))/n)
    results[i,1] <- dr[1,1]
    results[i,2] <- dr[1,2]
  }
  formula_means <- apply(results,2,mean)
  names(formula_means) <- c("Formula 1 Mean","Formula 2 Mean")
  return(formula_means)
}




weissberg_close_sim <- function(d_mean = 0, d_sd = 1, r_factor = 1, n = 435, sims = 100) {
  results <- matrix(NA, sims, 2)
  for(i in 1:sims){
    dr <- data.frame(d=rnorm(n, d_mean, d_sd)) %>%
      mutate(r = jitter(d, r_factor)) %>%
      mutate(diff = abs(d-r)) %>%
      summarise(formula_1 = sum(diff)/n,
                formula_2 = abs(sum(d)-sum(r))/n)
    results[i,1] <- dr[1,1]
    results[i,2] <- dr[1,2]
  }
  formula_means <- apply(results,2,mean)
  names(formula_means) <- c("Formula 1 Mean","Formula 2 Mean")
  return(formula_means)
}