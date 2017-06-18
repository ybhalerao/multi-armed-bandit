# Returns the number of conversions
get_conversions <- function(theta, impressions) {
  k <- ncol(theta)
  y <- rep(0.0, k)
  for (i in 1:k) {
    y[i] <- rbinom(1, size=impressions[i], prob=theta[i])
  } 
  return(y)
}

# Returns the probability
get_probability <- function(conversion, impressions) {
  k <- length(conversion)
  theta <- matrix(nrow=1000, ncol=k)
  for (i in 1:k) {
    theta[,i] <- rbeta(1000, shape1=conversion[i]+1, shape2=impressions[i]-conversion[i]+1)
  } 
  return(theta)
}


#Compute allocations
get_allocations <- function(theta) {
  k <- ncol(theta)
  allocations <- table(factor(max.col(theta), level=1:k))
  return(allocations/sum(allocations))
}


# COnversion and impressions are cumulative counts
# Assumed conversion rate of each creative for testing
ndays <- 10 #days to simulate
assumed_conversion <- c(0.02, 0.01, 0.01, 0.02)
conversion <- matrix(0, nrow=ndays, ncol=4)
impressions <- matrix(0, nrow=ndays, ncol=4)
impressions[1,] <- c(1000, 0, 0, 0)
next_day_allocations <- matrix(0, nrow=ndays+1, ncol=4)

#Simulating day 1
conversion[1,] <- assumed_conversion * impressions[1,]
theta <- get_probability(conversion[1,], impressions[1,])
y <- get_conversions(theta, impressions[1,])
next_day_allocations[2,] <- get_allocations(theta) * 1000

#Simulating days
for (day in 2:ndays) {
  impressions[day,] <- impressions[day-1,] + next_day_allocations[day,]
  conversion[day,] <- conversion[day-1,] + (assumed_conversion * next_day_allocations[day,])
  theta <- get_probability(conversion[day,], impressions[day,])
  y <- get_conversions(theta, impressions[day,])
  next_day_allocations[day+1,] <- get_allocations(theta) * 1000
}

