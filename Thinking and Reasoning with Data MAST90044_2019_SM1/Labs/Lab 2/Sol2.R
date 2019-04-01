######################################
# Solutions for Exrcises from Lab 2
######################################


# Exercise 1
salinity <- c(9.3, 10.7, 5.5, 9.6, 12.2, 16.6, 9.2, 10.5, 7.9, 13.2, 11, 8.8, 13.7, 12.1, 9.8)
hist(salinity)
# y_i = mu + error_i
mean(salinity)
sd(salinity)

# Exercise 2
pollution <- data.frame(location = rep(c("Above", "Below"), each = 15),
                         d.o. = c(5.2, 4.8, 5.1, 5, 4.9, 4.8, 5, 4.7, 4.7, 5, 4.7,
                                    5.1, 5, 4.9, 4.9, 4.2, 4.4, 4.7, 4.9, 4.6, 4.8, 4.9,
                                    4.6, 5.1, 4.3, 5.5, 4.7, 4.9, 4.8, 4.9))
plot(pollution)
mean(pollution[pollution$location == "Above", ]$d.o.)  # Estimate mu_1
mean(pollution[pollution$location == "Below", ]$d.o.)  # Estimate mu_2
sd1 <- sd(pollution[pollution$location == "Above", ]$d.o.)
sd1
sd2 <- sd(pollution[pollution$location == "Below", ]$d.o.)
sd2
wsd <- sqrt((sd1^2 + sd2^2)/2)
wsd

# Exercise 6
salinity <- c(7.6, 7.7, 4.3, 5.9, 5, 6.5, 8.3, 8.2, 13.2, 12.6,
              10.4, 10.8, 13.1, 12.3, 10.4)
afterflow <- c(23, 24, 26, 25, 30, 24, 23, 22, 22, 24, 25, 22, 22, 22, 24)
# y_i = x_i + eror_i
model1 <- lm(salinity ~ afterflow) # regression 
summary(model1)  # summary of the estimated parameters
plot(salinity ~ afterflow)
curve(30.8 - 0.91 * x, add = TRUE)








