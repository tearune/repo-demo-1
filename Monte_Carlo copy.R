# monte carlo sim
library(tidyverse)
#FIX LATER....
# creating a function for the difference, input n = sample size
#input dist, is distribution as a character
difference <- function(n, dist){
  if (dist == "discrete") {
    discrete <- (sample(
      x = 1:10,
      n, 
      replace = TRUE
    ))
    difference = median(discrete) - mean(discrete)
  } 
  else if (dist == "normal") {
    normal <- rnorm(
      n,
      mean = 0,
      sd = 1
    )
    difference = median(normal) - mean(normal) 
  }  
  else if (dist == "poisson") {
    pois = rpois(
      n,
      lambda = 0.75
    )
    difference = median(pois) - mean(pois)
  }   
  else if (dist == "exponential") {
    expo = rexp(
      n,
      rate = 0.5
    )
  
    difference = median(expo) - mean(expo)
  }
  return(difference)
  
}
  # sample size 10, replication
  discreetSample_10 <- replicate(
    n = 10000,
    expr = difference(n = 10,"discrete")
  )
  
  normalSample_10 <- replicate(
    n = 10000,
    expr = difference(10, "normal")
  )
  
  poissonSample_10 <- replicate(
    n = 10000,
    expr = difference(10, "poisson")
  )
  
  expoSample_10 <- replicate(
    n = 10000,
    expr = difference(10, "exponential")
  )
  
  
  # sample size 50, replication
  discreetSample_50 <- replicate(
    n = 10000,
    expr = difference(50, "discrete")
  )
  normalSample_50 <- replicate(
    n = 10000,
    expr = difference(50, "normal")
  )
  
  poissonSample_50 <- replicate(
    n = 10000,
    expr = difference(50, "poisson")
  )
  
  expoSample_50 <- replicate(
    n = 10000,
    expr = difference(50, "exponential")
  )
  
  
  # sample size 100, replication 
  discreetSample_100 <- replicate(
    n = 10000,
    expr = difference(100, "discrete")
  )
  
  normalSample_100 <- replicate(
    n = 10000,
    expr = difference(100, "normal")
  )
  
  poissonSample_100 <- replicate(
    n = 10000,
    expr = difference(100, "poisson")
  )
  
  expoSample_100 <- replicate(
    n = 10000, 
    expr = difference(100, "exponential")
  )
  
  expo_100 <- data_frame(expoSample_100)
  expo_50 <- data_frame(expoSample_50)
  expo_10  <- data_frame(expoSample_10)
expo_sample <- bind_cols(expo_10, expo_50, expo_100) 
View(expo_sample)
#cleaning Exponential Distribution dataframe
expo_clean <- expo_sample %>%
  rename(
    Exponential_10 = expoSample_10,
    Exponential_50 = expoSample_50,
    Exponential_100 = expoSample_100,
  ) %>%
  pivot_longer(
    cols = Exponential_10:Exponential_100,
    names_to = "Distribution",
    values_to = "Median - SAM"
  ) %>%
  separate_wider_delim(
    cols = Distribution,
    delim = "_",
    names = c("Distribution", "Sample Size")
  )
View(expo_clean)

#cleaning Discreet Uniform Data frame
str(discreetSample_10)
discreet_10 <- data_frame(discreetSample_10)
View(discreet_10)
discreet_50 <- data_frame(discreetSample_50)
discreet_100 <- data_frame(discreetSample_100)

discrete_sample <- bind_cols(discreet_10,discreet_50,discreet_100)
View(discreet_sample)
discrete_clean <- discrete_sample %>%
  rename(
    `Discrete Uniform_10` = discreetSample_10,
    `Discrete Uniform_50` = discreetSample_50,
    `Discrete Uniform_100` = discreetSample_100
  ) %>%
  pivot_longer(
    cols = `Discrete Uniform_10`:`Discrete Uniform_100`,
    names_to = "Distribution",
    values_to = "Median - SAM"
  ) %>%
  separate_wider_delim(
    cols = Distribution,
    delim = "_",
    names = c("Distribution", "Sample Size")
  )
View(discrete_clean)

#cleaning Normal
Normal_10 <- data_frame(normalSample_10)
Normal_50 <- data_frame(normalSample_50)
Normal_100 <- data_frame(normalSample_100)

normal_sample <- bind_cols(Normal_10, Normal_50, Normal_100)
View(normal_sample)
normal_clean <- normal_sample %>%
  rename(
    `Normal/Gaussian_10` = normalSample_10,
    `Normal/Gaussian_50` = normalSample_50,
    `Normal/Gaussian_100` = normalSample_100
  ) %>%
  pivot_longer(
    cols = `Normal/Gaussian_10`:`Normal/Gaussian_100`,
    names_to = "Distribution",
    values_to = "Median - SAM"
  ) %>%
  separate_wider_delim(
    cols = Distribution,
    delim = "_",
    names = c("Distribution", "Sample Size")
  )
View(normal_clean)

#cleaning Poisson
poisson_10 <- data_frame(poissonSample_10)
poisson_50 <- data_frame(poissonSample_50)
poisson_100 <- data_frame(poissonSample_100)

poisson_sample <- bind_cols(poisson_10, poisson_50, poisson_100)
View(poisson_sample)
poisson_clean <- poisson_sample %>%
  rename(
    Poisson_10 = poissonSample_10,
    Poisson_50 = poissonSample_50,
    Poisson_100 = poissonSample_100
  ) %>%
  pivot_longer(
    cols = Poisson_10:Poisson_100,
    names_to = "Distribution",
    values_to = "Median - SAM"
  ) %>%
  separate_wider_delim(
    cols = Distribution,
    delim = "_",
    names = c("Distribution", "Sample Size")
  )
View(poisson_clean)

#combining everything

filter_1 <- full_join(
  x = discrete_clean,
  y = normal_clean,
  by = join_by(Distribution == Distribution, `Median - SAM` == `Median - SAM`, `Sample Size` == `Sample Size`)
)

View(filter_1)

filter_2 <- full_join(
  x = filter_1,
  y = poisson_clean,
  by = join_by(Distribution == Distribution, `Median - SAM` == `Median - SAM`, `Sample Size` == `Sample Size`)
)

monte_carlo_clean <- full_join(
  x = filter_2,
  y = expo_clean,
  by = join_by(Distribution == Distribution, `Median - SAM` == `Median - SAM`, `Sample Size` == `Sample Size`)
)
View(monte_carlo_clean)