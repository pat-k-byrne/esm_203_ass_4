# Set the seed so we get the same random numbers every time we run this code
set.seed(666)
library(tidyverse)

# Define our possible mean sea levels at each conservation pathway
rcp4.5 <- c(0, 2, 7)*0.0254
rcp8.5 <- c(0, 11, 31)*0.0254
yr <- c(2000, 2020, 2050)

# Generate a sequence of 10000 random numbers from a normal distribution with a mean of zero and a standard deviation of 3
# This represents variations in sea level due to climate variability
sea.lvl.var.4.5 <- c()
sea.lvl.var.8.5 <- c()

# Create a vector of possible storm surges
surge <- 0:3

# Create matrices to store the results of our combinations of surge and year for each combination of tide level and pathway
# Rows are time, columns are surges
flood.prob.low.4.5 <- matrix(, nrow = length(rcp4.5), ncol = length(surge))
flood.prob.low.8.5 <- matrix(, nrow = length(rcp4.5), ncol = length(surge))
flood.prob.mid.4.5 <- matrix(, nrow = length(rcp4.5), ncol = length(surge))
flood.prob.mid.8.5 <- matrix(, nrow = length(rcp8.5), ncol = length(surge))
flood.prob.high.4.5 <- matrix(, nrow = length(rcp8.5), ncol = length(surge))
flood.prob.high.8.5 <- matrix(, nrow = length(rcp8.5), ncol = length(surge))

# Initializing variables
## Counter variables for number of anomalies greater than 5m for each conservation pathway and tide level
n.low.4.5 <- 0
n.mid.4.5 <- 0
n.high.4.5 <- 0
n.low.8.5 <- 0
n.mid.8.5 <- 0
n.high.8.5 <- 0
## Store the results of a single anomaly calculation for each conservation pathway and tide level
anomaly.low.4.5 <- 0
anomaly.mid.4.5 <- 0
anomaly.high.4.5 <- 0
anomaly.low.8.5 <- 0
anomaly.mid.8.5 <- 0
anomaly.high.8.5 <- 0

# BROTHER: THE LOOPS 
for (i in 1:length(surge)) { # For each of the four surge possibilities
  for (j in 1:length(rcp4.5)) { # And each of the three mean sea levels
    n.low.4.5 <- 0 
    n.mid.4.5 <- 0
    n.high.4.5 <- 0 # Set alllllll the counts back to zero
    n.low.8.5 <- 0
    n.mid.8.5 <- 0
    n.high.8.5 <- 0
    sea.lvl.var.4.5 <- rnorm(10000, mean = rcp4.5[j], sd = 3) # Find new sea level variations
    sea.lvl.var.8.5 <- rnorm(10000, mean = rcp8.5[j], sd = 3) 
    for (k in 1:10000){ # For each of the random numbers
      anomaly.low.4.5 <- sea.lvl.var.4.5[k] - 1.25 + surge[i] # Calculate the anomaly for each tide level
      anomaly.mid.4.5 <- sea.lvl.var.4.5[k] + surge[i]
      anomaly.high.4.5 <- sea.lvl.var.4.5[k] + 1.25 + surge[i]
      anomaly.low.8.5 <- sea.lvl.var.8.5[k] - 1.25 + surge[i]
      anomaly.mid.8.5 <- sea.lvl.var.8.5[k] + surge[i]
      anomaly.high.8.5 <- sea.lvl.var.8.5[k] + 1.25 + surge[i]
      if (anomaly.low.4.5 > 5){ 
        n.low.4.5 <- n.low.4.5 + 1} # IF the anomaly is greater than 5m for low tide for rcp4.5, increase the count by 1
      if (anomaly.mid.4.5 > 5){
        n.mid.4.5 <- n.mid.4.5 + 1} # Etc for each combo of pathway and tide level
      if (anomaly.high.4.5 > 5){
        n.high.4.5 <- n.high.4.5 + 1}
      if (anomaly.low.8.5 > 5){
        n.low.8.5 <- n.low.8.5 + 1}
      if (anomaly.mid.8.5 > 5){
        n.mid.8.5 <- n.mid.8.5 + 1}
      if (anomaly.high.8.5 > 5){
        n.high.8.5 <- n.high.8.5 + 1}
    }
    flood.prob.low.4.5[j,i] <- n.low.4.5/10000
    flood.prob.mid.4.5[j,i] <- n.mid.4.5/10000
    flood.prob.high.4.5[j,i] <- n.high.4.5/10000
    flood.prob.low.8.5[j,i] <- n.low.8.5/10000
    flood.prob.mid.8.5[j,i] <- n.mid.8.5/10000
    flood.prob.high.8.5[j,i] <- n.high.8.5/10000
  }
}

## Everything below this point is stuff to display my results

## --------------
df.0m.tides.4.5 <- data.frame('year' = yr, 
                              'lowtide' = flood.prob.low.4.5[,1], 
                              'hightide' = flood.prob.high.4.5[,1])
df.0m.mid.4.5 <- data.frame('year' = yr, 
                            'notide' = flood.prob.mid.4.5[,1])
## ---
df.1m.tides.4.5 <- data.frame('year' = yr, 
                              'lowtide' = flood.prob.low.4.5[,2], 
                              'hightide' = flood.prob.high.4.5[,2])
df.1m.mid.4.5 <- data.frame('year' = yr, 
                            'notide' = flood.prob.mid.4.5[,2])
## ---
df.2m.tides.4.5 <- data.frame('year' = yr, 
                              'lowtide' = flood.prob.low.4.5[,3], 
                              'hightide' = flood.prob.high.4.5[,3])
df.2m.mid.4.5 <- data.frame('year' = yr, 
                            'notide' = flood.prob.mid.4.5[,3])
## ---
df.3m.tides.4.5 <- data.frame('year' = yr, 
                              'lowtide' = flood.prob.low.4.5[,4], 
                              'hightide' = flood.prob.high.4.5[,4])
df.3m.mid.4.5 <- data.frame('year' = yr, 
                            'notide' = flood.prob.mid.4.5[,4])

## --------------
df.0m.tides.8.5 <- data.frame('year' = yr, 
                              'lowtide' = flood.prob.low.8.5[,1], 
                              'hightide' = flood.prob.high.8.5[,1])
df.0m.mid.8.5 <- data.frame('year' = yr, 
                            'notide' = flood.prob.mid.8.5[,1])
## ---
df.1m.tides.8.5 <- data.frame('year' = yr, 
                              'lowtide' = flood.prob.low.8.5[,2], 
                              'hightide' = flood.prob.high.8.5[,2])
df.1m.mid.8.5 <- data.frame('year' = yr, 
                            'notide' = flood.prob.mid.8.5[,2])
## ---
df.2m.tides.8.5 <- data.frame('year' = yr, 
                              'lowtide' = flood.prob.low.8.5[,3], 
                              'hightide' = flood.prob.high.8.5[,3])
df.2m.mid.8.5 <- data.frame('year' = yr, 
                            'notide' = flood.prob.mid.8.5[,3])
## ---
df.3m.tides.8.5 <- data.frame('year' = yr, 
                              'lowtide' = flood.prob.low.8.5[,4], 
                              'hightide' = flood.prob.high.8.5[,4])
df.3m.mid.8.5 <- data.frame('year' = yr, 
                            'notide' = flood.prob.mid.8.5[,4])


ggplot() +
  geom_ribbon(data = df.0m.tides.4.5, 
              aes(x = year, ymin = lowtide, ymax = hightide),
              fill = 'lightblue1',
              alpha = 0.4) +
  geom_line(data = df.0m.mid.4.5, 
            aes(x = year, y = notide),
            color = 'dodgerblue') +
  geom_point(data = df.0m.mid.4.5, 
            aes(x = year, y = notide),
            color = 'dodgerblue') +
  geom_ribbon(data = df.1m.tides.4.5, 
              aes(x = year, ymin = lowtide, ymax = hightide),
              fill  = 'lightblue2',
              alpha = 0.4) +
  geom_line(data = df.1m.mid.4.5, 
            aes(x = year, y = notide),
            color = 'blue') +
  geom_point(data = df.1m.mid.4.5, 
            aes(x = year, y = notide),
            color = 'blue') +
  geom_ribbon(data = df.2m.tides.4.5, 
              aes(x = year, ymin = lowtide, ymax = hightide),
              fill  = 'lightblue3',
              alpha = 0.4) +
  geom_line(data = df.2m.mid.4.5, 
            aes(x = year, y = notide),
            color = 'darkblue') +
  geom_point(data = df.2m.mid.4.5, 
            aes(x = year, y = notide),
            color = 'darkblue') +
  geom_ribbon(data = df.3m.tides.4.5, 
              aes(x = year, ymin = lowtide, ymax = hightide),
              fill = 'lightblue4',
              alpha = 0.4) +
  geom_line(data = df.3m.mid.4.5, 
            aes(x = year, y = notide),
            color = 'midnightblue') + 
  geom_point(data = df.3m.mid.4.5, 
            aes(x = year, y = notide),
            color = 'midnightblue') +
  labs(
    x = 'Year',
    y = 'Flood Probability',
    title = 'Changing Annual Flood Probability Under RCP 4.5') +
  scale_x_continuous(limits = c(2000,2050), 
                     expand = c(0.01,0.1)) +
  scale_y_continuous(limits = c(0,0.6),
                     expand = c(0,0)) +
  annotate("text",
           x = 2020,
           y = 0.06,
           label = "No storm surge",
           size = 4,
           angle = 0) +
  annotate("text",
           x = 2020,
           y = 0.11,
           label = "1m storm surge",
           size = 4,
           angle = 0)+
  annotate("text",
           x = 2020,
           y = 0.18,
           label = "2m storm surge",
           size = 4,
           angle = 0)+
  annotate("text",
           x = 2020,
           y = 0.28,
           label = "3m storm surge",
           size = 4,
           angle = 0) +
  theme_minimal()

ggplot() +
  geom_ribbon(data = df.0m.tides.8.5, 
              aes(x = year, ymin = lowtide, ymax = hightide),
              fill = 'lightblue1',
              alpha = 0.4) +
  geom_line(data = df.0m.mid.8.5, 
            aes(x = year, y = notide),
            color = 'dodgerblue') +
  geom_point(data = df.0m.mid.8.5, 
            aes(x = year, y = notide),
            color = 'dodgerblue') +
  geom_ribbon(data = df.1m.tides.8.5, 
              aes(x = year, ymin = lowtide, ymax = hightide),
              fill  = 'lightblue2',
              alpha = 0.4) +
  geom_line(data = df.1m.mid.8.5, 
            aes(x = year, y = notide),
            color = 'blue') +
  geom_point(data = df.1m.mid.8.5, 
            aes(x = year, y = notide),
            color = 'blue') +
  geom_ribbon(data = df.2m.tides.8.5, 
              aes(x = year, ymin = lowtide, ymax = hightide),
              fill  = 'lightblue3',
              alpha = 0.4) +
  geom_line(data = df.2m.mid.8.5, 
            aes(x = year, y = notide),
            color = 'darkblue') +
  geom_point(data = df.2m.mid.8.5, 
            aes(x = year, y = notide),
            color = 'darkblue') +
  geom_ribbon(data = df.3m.tides.8.5, 
              aes(x = year, ymin = lowtide, ymax = hightide),
              fill = 'lightblue4',
              alpha = 0.4) +
  geom_line(data = df.3m.mid.8.5, 
            aes(x = year, y = notide),
            color = 'midnightblue') +
  geom_point(data = df.3m.mid.8.5, 
            aes(x = year, y = notide),
            color = 'midnightblue') +
  labs(
    x = 'Year',
    y = 'Flood Probability',
    title = 'Changing Annual Flood Probability Under RCP 8.5') +
  scale_x_continuous(limits = c(2000,2050), 
                     expand = c(0.01,0.1)) +
  scale_y_continuous(limits = c(0,0.6),
                     expand = c(0,0)) +
  annotate("text",
           x = 2020,
           y = 0.08,
           label = "No storm surge",
           size = 4,
           angle = 0) +
  annotate("text",
           x = 2020,
           y = 0.135,
           label = "1m storm surge",
           size = 4,
           angle = 0)+
  annotate("text",
           x = 2020,
           y = 0.21,
           label = "2m storm surge",
           size = 4,
           angle = 0)+
  annotate("text",
           x = 2020,
           y = 0.31,
           label = "3m storm surge",
           size = 4,
           angle = 0) +
  theme_minimal()
