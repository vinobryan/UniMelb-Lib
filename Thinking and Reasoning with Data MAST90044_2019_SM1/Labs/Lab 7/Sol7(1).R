######################################
# Solutions for Exrcises from Lab 7
######################################


###### Exercise 1 - Taste of cheese
# (a)
cheese <- read.csv("data/cheese.csv")
plot(cheese)

# (b)
taste.lm1 <- lm(taste ~ acetic, data = cheese)
summary(taste.lm1)

taste.lm2 <- lm(taste ~ H2S, data = cheese)
summary(taste.lm2)

taste.lm3 <- lm(taste ~ lactic, data = cheese)
summary(taste.lm3)

#(c)
taste.lm4 <- lm(taste ~ acetic + H2S + lactic, data = cheese)
summary(taste.lm4)

taste.lm5 <- lm(taste ~ H2S + lactic, data = cheese) # remove 'acetic'
summary(taste.lm5)

#(d)
cor(cheese)





###### Exercise 2 - Soil Data
# (a)
soil <- data.frame(pai = c(4,18,14,18,26,26,21,30,28,36,65,62,40),
                    iron = c(61,175,111,124,130,173,169,169,160,244,257,333,199),
                    aluminium = c(13,21,24,23,64,38,33,61,39,71,112,88,54))
plot(soil)

# (b)
soil.lm.iron <- lm(pai ~ iron, data = soil)
anova(soil.lm.iron)
soil.lm.1 <- lm(pai ~ iron + aluminium, data = soil)
anova(soil.lm.1)
anova(soil.lm.1, soil.lm.iron)

# (c)
predict(soil.lm.iron, newdata = data.frame(iron = 150), interval = "prediction")

# (d)
par(mfrow = c(2, 2), las = 1, mar = c(4, 4, 2, 1))
plot(soil.lm.1)

#(e)
step(soil.lm.1, ~.)





###### Exercise 3 - Girth of horses
# (a)
girth <- data.frame(weight = c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5, 16.5, 18.5, 20.5), 
                    stretch = c(0.0136, 0.0172, 0.023, 0.0301, 0.0372, 0.043, 0.0469, 0.0448, 0.0401, 0.0301))
par(mar = c(1, 1, 1, 1))
plot(stretch ~ weight, data = girth)
cor(girth)

#(b)
girth.lm.2 <- lm(stretch ~ weight + I(weight^2), data = girth)
summary(girth.lm.2)
par(mfrow = c(2, 2), las = 1)
plot(girth.lm.2)

girth.lm.3 <- lm(stretch ~ weight + I(weight^2) + I(weight^3), data = girth)
summary(girth.lm.3)
plot(girth.lm.3)

# (c)
predict(girth.lm.3, newdata = data.frame(weight = 10.5), interval = "confidence")

###### Exercise 4 - Phosphorus content in corn.
# (a)
Pcorn <- read.csv("data/Pcorn.csv")
plot(Pcorn)
cor(Pcorn)

#(b)
Pcorn.lm <- lm(avP ~ ino + org1 + org2, data = Pcorn)  # backward selection.
step(Pcorn.lm, ~.)

Pcorn.lm1 <- lm(avP ~ 1, data = Pcorn) # forward selection.
step(Pcorn.lm1, ~. + ino + org1 + org2)

#(c)

( (2131.2 - 2087.2) / (15-13) ) / (  2087.2/13   )  # F-statistic

 

###### Exercise 5 - Estimating timber volume
# Taking logs of each variable is a good place to start.
timber <- read.csv("data/timber.csv")
timber$logvol = log(timber$vol)
timber$logdiam = log(timber$diam)
timber$loght = log(timber$ht)
par(mfrow = c(1, 2), las = 1, mar = c(4, 6, 2, 1))
plot(logvol ~ logdiam, data = timber)
plot(logvol ~ loght, data = timber)


timber.lm <- lm(logvol ~ logdiam + loght, data = timber)
summary(timber.lm)


















