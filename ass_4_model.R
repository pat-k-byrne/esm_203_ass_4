# Set the seed so we get the same random numbers every time we run this code
set.seed(007)

# Generate a sequence of 10000 random numbers from a normal distribution with a mean of zero and a standard deviation of 3
# This represents variations in sea level due to climate variability
sea.lvl.var <- rnorm(10000, mean = 0, sd = 3) 

# Generate a sequence of 10000 random numbers from a uniform distribution from a range of -1.25 to 1.25
# This represents variation of tidal levels
tidal.var <- runif(10000, -1.25, 1.25)

# Create a vector of possible storm surges
surge <- 0:3

# Create a vector to store the results of our model run
flood.prob <- c()

n <- 0
anomaly <- 0
for (i in 1:4) {
  n <- 0
  for (j in 1:10000){
    anomaly <- sea.lvl.var[j] + tidal.var[j] + surge[i]
  }
}


