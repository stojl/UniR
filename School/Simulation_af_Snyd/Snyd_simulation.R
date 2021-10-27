library(Rcpp)
sourceCpp(file = "School/Simulation_af_Snyd/dcount.cpp")

rollbeforecomp <- function(dice_vector, m = 1){
  
  #Initialize
  total_length <- 6 * m
  onesix <- 1:6
  total_dice <- sum(dice_vector)
  result <- vector(mode = "double", length = total_length)

  #Simulation
  for(i in 1:m){
    result[((i-1)*6 + 1):(i*6)] <- sumDice(sample(onesix, total_dice, replace = TRUE), dice_vector)
  }
  
  #Output
  m <- matrix(c(rep(1:m, each = 6), rep(onesix, m), result), total_length, 3)
  colnames(m) <- c("gameid", "value", "count")
  return(m)
}

simulate_roll <- compiler::cmpfun(rollbeforecomp)