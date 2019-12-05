# Set the seed so we get the same random numbers every time we run this code
set.seed(007)

# Generate a sequence of 10000 random numbers from a normal distribution with a mean of zero and a standard deviation of 3
# This represents variations in sea level due to climate variability
sea.lvl.var <- rnorm(10000, mean = 0, sd = 3) 

# Create a vector of possible storm surges
surge <- 0:3

# Create a vector to store the results of our model run
flood.prob.low <- c()
flood.prob.high <- c()

# Initializing variables
## Counter variables for number of anomalies greater than 5m
n.low <- 0
n.high <- 0
## Store the results of a single anomaly calculation
anomaly.low <- 0
anomaly.high <- 0
for (i in 1:4) { # For each of the four surge possibilities
  n.low <- 0 # Set the count back to zero
  n.high <- 0
  for (j in 1:10000){ # For each of the random numbers
    anomaly.low <- sea.lvl.var[j] - 1.25 + surge[i] # Calculate the anomaly for each tide level
    anomaly.high <- sea.lvl.var[j] + 1.25 + surge[i]
    if (anomaly.low > 5) { # Is the anomaly greater than 5m for low tide?
      n.low <- n.low + 1 # If so, increase the count by one
    }
    if (anomaly.high > 5) { # Is the anomaly greater than 5m for high tide?
      n.high <- n.high + 1 # If so, increase the count by one
    }
  }
  flood.prob.low[i] <- n.low/10000 # Finally, store the risk probability for each of the four surge possibilities
  flood.prob.high[i] <- n.high/10000
}


