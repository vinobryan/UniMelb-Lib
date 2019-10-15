######################################
# Solutions for Exercises from Lab 3
######################################


### Exercise 1 - Drink driving
# We have 189 'yes' answers and 111 'no' or 'unsure' answers.
n <- 300
(p.hat <- 189/n)
(n.tilde <- n + 4)
(p.tilde <- (189 + 2)/n.tilde)
round(ci <- p.tilde + c(-1.96, 1.96) * sqrt(p.tilde * (1 - p.tilde)/n.tilde),4)
ci  # CI without rounding

### Exercise 2 - Mendel's peas
peas <- c(rep("A-RY", 315), rep("B-WY", 101), rep("C-RG", 108),rep("D-WG", 32))
table(peas)

p.peas <- table(peas)/length(peas) # Get point estimates of the proportions in each category
round(p.peas, 3)


### Exercise 3 - Blood groups
group <- c(rep("O", 51), rep("A", 38), rep("B", 9), rep("AB", 2))  # create the data
table(group)
p.hat = table(group)/length(group)  # proportion of each group
p.hat
p.hat.AB <- p.hat[2]  # proportion of group AB
p.hat.AB + c(-1, 1) * 1.96 * sqrt(p.hat.AB * (1 - p.hat.AB)/length(group))  # Wald CI

qbeta(c(0.025, 0.975), 2 + 0.5, 100 - 2 + 0.5)  # Jeffreys prior

### Exercise 4 - Kitchen appliance preferences
# (a) Enter the data
finish <- c(rep("A-White", 70), rep("B-Steel", 130), rep("A-White", 48), rep("B-Steel", 52))
product <- c(rep("C-Fridge", 200), rep("D-Washer", 100))
goods <- data.frame(product, finish)
table(goods)
table(goods$finish) # total line

# (b) Point estimate for the proportion and CI, using Agresti-Coull
p.finish <- table(goods$finish)/length(goods$finish)
round(p.finish,3)
n.tilde <- length(goods$finish) + 4
p.tilde <- (table(goods$finish) + 2)/n.tilde
# lower confidence limit
lcl <- p.tilde - 1.96 * sqrt(p.tilde * (1 - p.tilde)/n.tilde)
round(lcl,3)

# upper confidence limit
ucl <- p.tilde + 1.96 * sqrt(p.tilde * (1 - p.tilde)/n.tilde)
round(ucl,3)


# (c) estimate the difference between the proportion of customers that prefer
# stainless steel for refrigerators as opposed to the proportion of customers that prefer
# stainless steel for dish washers.
p.steel <- table(goods)[, 2]/(table(goods)[, 1] + table(goods)[,2])
p.steel
p.diff <- p.steel[1] - p.steel[2]
s2.steel <- p.steel * (1 - p.steel)/(table(goods)[, 1] + table(goods)[,2])
var.diff <- sum(s2.steel)
ci.diff <- p.diff + qnorm(c(0.025, 0.975)) * sqrt(var.diff)
round(ci.diff, 3)



### Exercise 5 - Wallet on the street
# (a) separate approximate 95% condence intervals for the proportions 
sex <- c("Female", "Male")
n <- c(93, 75)
yes <- c(84, 53)
p.hat <- round(yes/n, 3) # proportion say "YES"
n.tilde <- n + 4
p.tilde <- (yes + 2)/n.tilde
lcl <- c(-1, -1) # low CI
ucl <- c(1, 1) # upper CU
ci <- round(p.tilde + cbind(lcl, ucl) * 1.96 * sqrt(p.tilde *(1 - p.tilde)/n.tilde), 3)
data.frame(sex, p.hat, ci)

#(b) CI for difference in proportions
p <- yes/n
s2.p <- p * (1 - p)/n
difference <- p[1] - p[2]
s2.diff <- sum(s2.p)
round(ci <- difference + qnorm(c(0.025, 0.975)) * sqrt(s2.diff),3)

#### Exercise 6 -  Aspirin and stroke
# (b) sample proportion in the treatment and the control groups

treat <- c("aspirin", "control")
n <- c(78, 77)
yes <- c(63, 43)
p.hat <- round(yes/n, 3)
data.frame(treat, p.hat)

# (c) 95% confidence interval for the difference 
p <- yes/n
s2.p <- p * (1 - p)/n
difference <- p[1] - p[2]
s2.diff <- sum(s2.p)
round(ci <- difference + qnorm(c(0.025, 0.975)) * sqrt(s2.diff),3)