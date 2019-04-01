#############################
# Lab 03 - R Tutorial
#############################

# Setting Woeking Directory
setwd("C:/Users/jpolak/Documents/Unimelb/MATH90044_S1_2014/MLabs/lab03/")  # 


## 3.3.1 Probabilistic definitions
x1 <- 0:10
pmf <- dbinom(x1, 10, 0.5)   # probability mass function, x1 ~  Bi(10, 0,5)
pmf
plot(pmf ~ x1, type = "h")   # plot "histogram-like" vertical lines

cdf <- pbinom(x1, 10, 0.5)   # cumulative distribution function, x1 ~  Bi(10, 0,5)
cdf
plot(cdf ~ x1, type = "s")   # plot estimated cumulative distribution function

x2 <- seq(-3, 3, by = 0.1)   # points for evaluation
x2
cdf <- pnorm(x2, 0, 1)       # probability density function, x2 ~ N(0,1)
plot(cdf ~ x2, type = "l")   # plot cdf


#### 3.5 Interval estimation

## 3.5.1 Wald
n <- 200745
p.hat <- 57/n
p.hat
p.hat + c(-1.96, 1.96) * sqrt(p.hat * (1 - p.hat)/n)  # Confidence interval

## 3.5.2 Agresti-Coull
(n.tilde <- 200745 + 4)
(p.tilde <- (57 + 2)/n.tilde)
p.tilde + c(-1.96, 1.96) * sqrt(p.tilde * (1 - p.tilde)/n.tilde)

## 3.5.3 Jeffreys prior
qbeta(c(0.025, 0.975), 57 + 0.5, 200745 - 57 + 0.5)

#### 3.7 More than two categories
ufc <- read.csv("data/ufc.csv")
table(ufc$species)
table(ufc$species)/dim(ufc)[1]  # proportion of trees in each of the four species, by dividing by the number of observations
head(table(ufc$plot, ufc$species))/dim(ufc)[1]


#### 3.8 Difference between two proportions
x1=54
x2=27
n1=100
n2=60
p1.hat=x1/n1
p2.hat=x2/n2
pdiff=p1.hat-p2.hat
ucl=pdiff+1.96*sqrt((p1.hat*(1-p1.hat)/n1)+(p2.hat*(1-p2.hat)/n2))
lcl=pdiff-1.96*sqrt((p1.hat*(1-p1.hat)/n1)+(p2.hat*(1-p2.hat)/n2))
prop.test(x=c(54,27),n=c(100,60))  # We can also use prop.test for normal approximation for equality of two proportions









