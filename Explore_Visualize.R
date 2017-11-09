library(ggplot2)
library(forecast)

###1 Creating Time Series Objects in R

# Read the data from Excel into
Rmydata <- read_excel("exercise1.xlsx")
# Look at the first few lines of mydata
head(mydata)
# Create a ts object called 
mytsmyts <- ts(mydata[,2:4], start = c(1981, 1), frequency = 4)

###2 Time Series Plots

# Plot the data with facetting
autoplot(myts, facets = TRUE)
# Plot the data without facetting
autoplot(myts, facets = FALSE)
# Plot the three series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)
# Find the outlier in the gold series
goldoutlier <- which.max(gold)
# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)

###3 Seasonal Plots

# Load the fpp2 package
library(fpp2)
# Create plots of the a10 data
autoplot(a10)
ggseasonplot(a10)
# Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = TRUE)
# Restrict the ausbeer data to start in 1992
beer <- window(ausbeer, start=1992)
# Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)




