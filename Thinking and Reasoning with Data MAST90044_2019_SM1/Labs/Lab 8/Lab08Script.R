#############################
# Lab 08 - R Tutorial
#############################

# Setting Working Directory
setwd("C:/Users/jpolak/Documents/Unimelb/MATH90044_S1_2014/MLabs/lab08/")  #


# 8.3.1 The logistic regression model
x <-seq(-10,10,.01)
p <- exp(x)/(1+exp(x))
plot(x,p,type="l")

# 8.3.2 One binary explanatory variable
# The hospital example:
admissions <- data.frame(hospital = factor(c(1, 2)), death = c(24, 6), total = c(100, 60))
admissions
admissions.1 <- glm(death/total ~ hospital, family = binomial,  # logistic regression
                    weight = total, data = admissions)
summary(admissions.1)
confint.default(admissions.1)        # CI for log-OR
exp(1)^(confint.default(admissions.1))  # CI for OR


# 8.3.3 One categorical explanatory variable (more than two levels)
admissions3 <- data.frame(hospital = factor(c(1, 2, 3)), death = c(24,6, 14), total = c(100, 60, 40))
admissions3.1 <- glm(death/total ~ hospital, family = binomial,
                        weight = total, data = admissions3)
summary(admissions3.1)


# 8.3.4 One numerical explanatory variable
#Example: Voting behaviour
voters <- read.csv("data/voting.csv")
voters
plot(voters$views, voters$reagan/voters$total, xlab = "Political Views",
        ylab = "Proportion for Reagan")
vote.1 <- glm(reagan/total ~ views,
               family = binomial,
               weight = total,
               data = voters)
summary(vote.1)
deviance(vote.1)



















