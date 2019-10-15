#############################
# Lab 02 - R Tutorial
#############################

# Setting Working Directory
setwd("C:/Users/fhalasan/Documents/Unimelb/MATH90044_S1_2014/MLabs/lab02/")  # Set your own working directory.


## 2.2 Distributions

# discrete probability distributions
curve(dbinom(x, p = 0.5, size = 10), from = 0, to = 10, type = "s", main = "Binomial")
curve(dgeom(x, prob = 0.2), from = 0, to = 10, type = "s", main = "Geometric")
curve(dpois(x, lambda = 3), from = 0, to = 10, type = "s", main = "Poisson")
curve(dnbinom(x, size = 5, prob = 0.75), from = 0, to = 10, type = "s", main = "Negative Binomial")

# continuous probability distributions
curve(dnorm, from = -3, to = 3, main = "Normal")
curve(dt(x, df = 5), from = -3, to = 3, main = "t (5 df)")
curve(dchisq(x, df = 10), from = 0.01, to = 30, main = "Chi-squared (10 df)")
curve(dgamma(x, shape = 2), from = 0.01, to = 8, main = "Gamma")
 
set.seed(83446410) # setting the seed 

## Formulating statistical models

# Example 1 (soil pH)
pH <- c(6, 5.7, 6.2, 6.3, 6.5, 6.4, 6.9, 6.6, 6.8, 6.7, 6.8, 7.1, 6.8, 7.1, 7.1, 7.5, 7)
hist(pH)

# Example 2 (Fuel economy of cars)
cars <- read.csv("data/racv.csv")
plot(lp100km ~ mass.kg, data = cars)

# Example 3 (Potato yield)
potatoes <- data.frame(treatment=rep(1:4,each=4),
                       wt.kg=c(752, 762, 686, 787,
                               621, 637, 670, 575,
                               642, 667, 655, 660,
                               645, 627, 596, 576))

plot(wt.kg ~ treatment, data = potatoes)


## Simulation
rnorm(10) # generate 10 standard normally-distributed numbers
normal.pop <- rnorm(1000)
qqnorm(normal.pop)
plot(density(normal.pop))


## Sampling
?sample
sample(normal.pop, size = 10) # take a sample of 10 from 'nermal.pop' data

exponential.pop <- rexp(1000, rate = 1)  # generate 1000 obs. from exponential distribution
exponential.sample <- sample(exponential.pop, size = 10) # take a sample of 10 from 'exponential.pop' data
# some plotes:
plot(density(exponential.pop), main = "Exponential Population")
qqnorm(exponential.pop, main = "")
qqline(exponential.pop)
plot(density(exponential.sample), main = "Just a sample")
qqnorm(exponential.sample, main = "")
qqline(exponential.sample)

## Sampling distributions

 # 'sapply' function instead of 'for' function
exp.means <- sapply(1:1000, function(x) mean(sample(exponential.pop, size= 15 )))
exp.means[1:10]
plot( density(exp.means) ) # plot the density distribution of the  means of the 1000 samples

# samples of 50
exp.means <- sapply(1:1000, function(x) mean(sample(exponential.pop, size= 20 )))
plot( density(exp.means) )

# many sample sizes at once
levels <- rep(c(1, 10, 20, 40, 80, 160), each = 1000)
sample.means <- sapply(levels, function(x) mean(sample(exponential.pop, size = x)))

boxplot(sample.means ~ levels, xlab = "n", ylab = expression(paste(bar(x))))  # boxplots
abline(h = 1, col = "darkgrey")







































