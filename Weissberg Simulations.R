weissberg_sim <- function(d_mean = 0, d_sd = 1, r_mean = 0, r_sd = 1, n = 435, sims = 100) {
  results <- matrix(NA, sims, 3)
  for(i in 1:sims){
    d <- rnorm(n, d_mean, d_sd)
    r <- rnorm(n, r_mean, r_sd)
    dr <- as.data.frame(cbind(d,r)) %>%
      mutate(diff = abs(d-r)) %>%
      summarise(formula_1 = sum(diff)/n,
                formula_2 = abs(sum(d)-sum(r))/n,
                formula_diff = (sum(diff)/n) - (abs(sum(d)-sum(r))/n))
    results[i,1] <- dr[1,1]
    results[i,2] <- dr[1,2]
    results[i,3] <- dr[1,3]
  }
  formula_means <- apply(results,2,mean)
  formula_summary <- append(formula_means, (formula_means[1]-formula_means[2])/formula_means[1])
  names(formula_summary) <- c("Formula 1 Mean","Formula 2 Mean","Average Difference","Improvement")
  return(formula_summary)
}




weissberg_close_sim <- function(d_mean = 0, d_sd = 1, r_factor = 1, n = 435, sims = 100) {
  results <- matrix(NA, sims, 3)
  for(i in 1:sims){
    dr <- data.frame(d=rnorm(n, d_mean, d_sd)) %>%
      mutate(r = jitter(d, r_factor)) %>%
      mutate(diff = abs(d-r)) %>%
      summarise(formula_1 = sum(diff)/n,
                formula_2 = abs(sum(d)-sum(r))/n,
                formula_diff = (sum(diff)/n) - (abs(sum(d)-sum(r))/n))
    results[i,1] <- dr[1,1]
    results[i,2] <- dr[1,2]
    results[i,3] <- dr[1,3]
  }
  formula_means <- apply(results,2,mean)
  formula_summary <- append(formula_means, (formula_means[1]-formula_means[2])/formula_means[1])
  names(formula_summary) <- c("Formula 1 Mean","Formula 2 Mean","Average Difference","Improvement")
  return(formula_summary)
}