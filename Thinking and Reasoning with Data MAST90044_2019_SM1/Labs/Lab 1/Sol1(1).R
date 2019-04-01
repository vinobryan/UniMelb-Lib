######################################
# Solutions for Exrcises from Lab 1
######################################


# 1. Effect of `the pill' on blood pressure.

bp <- data.frame(class = c("085-090", "090-095", "095-100",
 "100-105", "105-110", "110-115", "115-120", "120-125", "125-130",
 "130-135", "135-140", "140-145", "145-150", "150-155", "155-160"),
 nonusers = c(1,1,5,11,11,17,18,11,9,7,4,2,2,1,0),
 users = c(0,0,4,5,10,15,17,14,12,10,5,4,2,1,1))

bp

par(mfrow = c(2,1))
plot(nonusers~class,data=bp)
plot(users~class,data=bp)
# dev.off()


# 2. Pulse rate and exercise.

pulse <- read.table("data/ms212.txt", header = TRUE)
head(pulse, n = 10)

    #(a)
    # Example: level of other exercise.
tapply(pulse$Pulse1, pulse$Exercise, mean)
tapply(pulse$Pulse1, pulse$Exercise, sd)

   # Example: level of Smoking.
tapply(pulse$Pulse1, pulse$Smokes, mean)
tapply(pulse$Pulse1, pulse$Smokes, sd)

  # Example: Height
plot(pulse$Pulse1 ~ pulse$Height)

# Example: Weight
plot(pulse$Pulse1 ~ pulse$Weight)
    
    # (b)
boxplot(pulse$Pulse2 ~ pulse$Ran, horizontal = TRUE)
    # change in pulse
pulse$Pulsechange <- pulse$Pulse2 - pulse$Pulse1
boxplot(pulse$Pulsechange ~ pulse$Ran, horizontal = TRUE)    


# 3. Test results.
scores <- c(16, 14, 15, 16, 14, 20, 18, 16, 13, 17, 16, 13, 18,
 16, 22, 20, 24, 18, 16, 20, 26, 19, 14, 17, 14, 15, 19, 12,
 11, 16, 19, 13, 13, 18, 8, 16, 28, 16, 27, 17, 12, 15, 7,
 12, 22, 20, 16, 10, 8, 16, 13, 14, 14, 16, 18, 15, 21, 23,
 16, 5, 14, 23, 17, 15, 14, 22, 20, 22, 13, 20, 18, 13, 14,
 21, 14, 18, 10, 20, 24, 17, 21, 15, 18, 12, 23, 17, 10, 15,
 11, 5, 16, 19, 22, 10, 15, 17, 13, 23, 20, 3, 18, 15, 22,
 12, 9, 20, 16, 17, 17, 16, 21, 18, 11, 14, 6, 21, 25, 18,
 26, 18, 18, 15)

summary(scores)
hist(scores)
plot(density(scores))

# 4. Soil water evaporation.
soil <- data.frame(air.speed = c(0.5, 0.5, 0.5, 1, 1, 1, 1.5,
 1.5, 1.5, 2, 2, 2, 2.5, 2.5, 2.5), evaporation = c(5.39,
 4.43, 5.5, 7.7, 6.2, 6.14, 5.62, 6.12, 7.2, 6.88, 7.73, 6.01,
 5.1, 7.29, 7.28))

cor(soil$evaporation, soil$air.speed)

plot(evaporation ~ air.speed, data = soil)

plot(evaporation ~ air.speed, data = soil)
abline(lm(soil$evaporation ~ soil$air.speed))


# 5. Melon yields.
melons <- data.frame(variety = rep(c("A", "B", "C", "D"), 6),
 yield = c(25, 40, 18, 28, 17, 35, 23, 29, 26, 32, 26, 33,
 16, 37, 15, 32, 22, 43, 11, 30, 16, 37, 24, 28))

melons
plot(melons)
boxplot(yield ~ variety, varwidth = TRUE, data = melons, horizontal = FALSE)  # box plot


# 6. Tree inventory data.

ehc <- read.csv("data/ehc.csv")
plot(height.m ~ dbh.cm, data = ehc)
plot(height.m ~ dbh.cm, pch = as.character(species), data = ehc)
print(xyplot(height.m ~ dbh.cm | species, data = ehc))


# 7. Lymphocyte counts.
count <- c(7.1, 6.7, 7.1, 6.7, 6.1, 5.1, 5.8, 5.4, 6.9, 5.9, 6.2, 5.7, 5.6, 5.1, 5, 5.2, 6.4, 5.8, 6.2, 5.3)
litter <- rep(1:5, each = 4)
drug <- rep(c("A", "B", "C", "D"), 5)
lymph <- data.frame(count, litter, drug)
lymph

plot(count ~ drug, data = lymph)
plot(count ~ litter, data = lymph)
