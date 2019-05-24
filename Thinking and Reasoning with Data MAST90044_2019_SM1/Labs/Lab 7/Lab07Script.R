#############################
# Lab 07 - R Tutorial
#############################

# Setting Woeking Directory
setwd("C:/Users/jpolak/Documents/Unimelb/MATH90044_S1_2014/MLabs/lab07/")  # 


### 7.2 Multiple Regression
# Example: Soil-sediment

soil <- data.frame(pai = c(4,18,14,18,26,26,21,30,28,36,65,62,40),
                iron = c(61,175,111,124,130,173,169,169,160,244,257,333,199),
                aluminium = c(13,21,24,23,64,38,33,61,39,71,112,88,54))
soil.lm.1 <- lm(pai ~ iron + aluminium, data = soil) # fit the model
summary(soil.lm.1)
predict(soil.lm.1, newdata = data.frame(iron=150, aluminium=40), interval = "prediction") # predict


### 7.2.1 Polynomial regression
# Example: Nursery plants
plants <- data.frame(age = rep(2:7, rep(4, 6)),
                      height = c(
                        5.6, 4.8, 5.3, 5.7, 6.2, 5.9, 6.4, 6.1,
                        6.2, 6.7, 6.4, 6.7, 7.1, 7.3, 6.9, 6.9,
                        7.2, 7.5, 7.8, 7.8, 8.9, 9.2, 8.5, 8.7))
plot(height ~ age, data = plants)
plants.lm.1 <- lm(height ~ age, data = plants)
plants.lm.3 <- lm(height ~ age + I(age^2) + I(age^3), data = plants)
summary(plants.lm.3)

par(mfrow = c(2, 2), las = 1)
plot(plants.lm.3) # diagnostics
 dev.off()

plot(height ~ age, data = plants, xlab="Age (y)", ylab="Height (ft)") # graphical comparison of the models
curve(predict(plants.lm.1, newdata=data.frame(age = x)), add=TRUE, col="red")
curve(predict(plants.lm.3, newdata=data.frame(age = x)), add=TRUE, col="blue")

#### 7.3 Model Selection
## 7.3.2 Nested models
# Example: Nursery plants
anova(plants.lm.1, plants.lm.3)

## 7.3.3 Checking the utility of a model
summary(plants.lm.1)


## 7.3.5 Stepwise (sequential) methods
# Example: Application of selection strategies to the hardening of cement example.
heat <- read.csv("data/heat.csv")

step(heat.lm1, ~.) # Start with the `full' model.
heat.lm1 <- lm(y~x1+x2+x3+x4, data=heat)
coef(summary(heat.lm1))

heat.lm2 <- lm(y~1, data=heat) # Start with the `null' model.
step(heat.lm2, ~. + x1 + x2 + x3 + x4)
heat.lm3 <- lm(y ~ x1 + x2 + x4, data = heat)
anova(heat.lm3)
summary(heat.lm3)




















