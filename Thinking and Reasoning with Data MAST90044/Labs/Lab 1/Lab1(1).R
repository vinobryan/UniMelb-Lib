################################
# Lab 1 - R Tutorial
################################

1 + 1

# Setting Woeking Directory
getwd()                 # Prints the working directory.
setwd("C:/Temp")        # Set "C:/Temp" to be the working directory.

getwd()


# Reading Data 
ufc <- read.csv("data/ufc.csv")   # read file named 'ufc' (in csv format) taht inside of 'data' directory (folder)

# Examine the data
str(ufc)  # (str is short for \structure

# Work Space
ls()   # give a list of objects that we have
temp <- c(1,5,6)
ls()
rm(temp)  # remove object 'temp' from the memory
ls()


# Descriptive Statistics
table(ufc$species)            # Tabilate the 'speacies' variable in the datase 'ufc'
table(ufc$species, ufc$tree)  # Crosstable
prop.table(  table(ufc$species, ufc$tree)  , 1)   # proportions in crosstable

mean(ufc$dbh.cm)   # mean
tapply(ufc$dbh.cm, ufc$species, mean)    # conditional mean
median(ufc$dbh.cm) # median
tapply(ufc$dbh.cm, ufc$species, median)  # conditioanl median
sd(ufc$dbh.cm)    # SD
tapply(ufc$dbh.cm, ufc$species, sd)  # SD by species

summary(ufc$dbh.cm)  # five-number summary
tapply(ufc$dbh.cm, ufc$species, summary)  # summary by species

cor(ufc$dbh.cm, ufc$height.m) # correlation

# Graph
barplot(table(ufc$species), horiz = TRUE)  # bar chart
hist(ufc$dbh.cm)  # histogram
plot(density(ufc$dbh.cm)) # dendity plot
boxplot(dbh.cm ~ species, varwidth = TRUE, data = ufc, horizontal = TRUE)  # box plot

plot(height.m ~ dbh.cm, data = ufc)    # Scatterplots
abline(lm(height.m ~ dbh.cm, data = ufc))

library(lattice)  # load the 'lattice' library

xyplot(height.m ~ dbh.cm | species, data = ufc)  # ploting several plots in different panals

xyplot(height.m ~ dbh.cm, groups = species, auto.key = list(space="right"), data = ufc) # giving different colors to different species

xyplot(height.m ~ dbh.cm | species, type = c("p", "smooth"), data = ufc) # scatterplots with smoothers.

# Save plot

pdf("dbh-height.pdf")
   plot(height.m ~ dbh.cm, data = ufc)
dev.off() 
