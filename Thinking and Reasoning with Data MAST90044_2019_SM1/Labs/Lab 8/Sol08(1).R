######################################
# Solutions for Exrcises from Lab 8
######################################


### Exercise 1 - Aspirin and stroke
#(a)
stroke <- data.frame(treat=c("aspirin", "control"),y=c(63,43), n=c(78, 77))
stroke$rate <- stroke$y/stroke$n
stroke
aspirin.1 <- glm(rate ~ factor(treat)
                  , family = binomial
                  , weight = n, data=stroke)
summary(aspirin.1)
#(b)
exp(summary(aspirin.1)$coef[2,1])
exp(summary(aspirin.1)$coef[2,1]+1.96*c(-1,1)*summary(aspirin.1)$coef[2,2])


### Exercise 2 - Control of budworm
moths <- data.frame(dose = c(1, 2, 4, 8, 16, 32), dead = c(1, 4, 9, 13, 18, 20), total = rep(20, 6))
#(a)
moths$rate <- moths$dead / 20
plot(rate ~ dose, data = moths)
moths.1 <- glm( rate ~ dose
                 , family = binomial
                 , weight = total
                 , data = moths )
summary(moths.1)

#(b)
exp(0.29723)

#(c)
moths.1$coef
# 0 = -1.92771 + 0.29723 * dose
1.92771/0.29723 # the dose which kills half the insects
#(d)
moths$logdose<-log(moths$dose)
plot(rate ~ logdose, data = moths)
moths.2 <- glm( rate ~ logdose
                 , family = binomial
                 , weight = rep( 20, 6)
                 , data = moths )
summary(moths.2)

par(mfrow = c(1, 2))
plot(rate ~ dose, type = "l", data = moths)
dse <- seq(0, 32, 0.5)
lines(dse, predict(moths.1, newdata = data.frame(dose = dse),
                     type = c("response")), lty = 2)
plot(rate ~ logdose, type = "l", data = moths)
dse <- seq(0, 3.5, 0.1)
lines(dse, predict(moths.2, newdata = data.frame(logdose = dse),
                     type = c("response")), lty = 2)
# dev.off()


### Exercise 3 - Powdery mildew on broccoli
#(a)
broccoli <- data.frame(treat=c(rep("control",5), rep("fungicide1",5), rep("fungicide2",5)),
                        alive=c(3,2,2,1,1,10,8,15,14,8,17,14,13,10,16),
                        emerged=c(17,15,16,11,15,16,11,18,16,12,18,15,16,13,16))
broccoli$rate <- broccoli$alive/broccoli$emerged
broccoli
tapply(broccoli$rate,broccoli$treat,mean)

#(b)
broccoli.1 <- glm(rate ~ treat,
                   family = binomial,
                   weight = emerged,
                   data = broccoli )
summary(broccoli.1)

#(c)
exp(summary(broccoli.1)$coef[2, 1])
exp(summary(broccoli.1)$coef[2, 1] + 1.96 * c(-1, 1) * summary(broccoli.1)$coef[2, 2])


### Exercise 4 - Goalkicking
goalkicking <- read.csv("data/goalkicking.csv")
goal.glm1 <- glm(goal~distance+angle,
                 family=binomial,
                 data=goalkicking) 
summary(goal.glm1)
# Odds ratios:
beta<-summary(goal.glm1)$coef[2:3,1]
sebeta<-summary(goal.glm1)$coef[2:3,2]

logitphat<-predict(goal.glm1, newdata=data.frame(distance=c(40,10,30), angle=c(20,0,60)))
logitphat
exp(logitphat)/(1+exp(logitphat))

library(lattice)
xyplot(distance~angle,groups=goal,data=goalkicking,auto.key=T)



