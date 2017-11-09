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

###4 Autocorrelation of non-seasonal time series

# Create an autoplot of the oil data
autoplot(oil)

# Create a lag plot of the oil data
gglagplot(oil)

# Create an ACF plot of the oil data
ggAcf(oil)

###5 Autocorrelation of seasonal and cyclic time series

# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- 1

# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)

# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7

###6 Stock prices and white noise

# Plot the original series
autoplot(goog)

# Plot the differenced series
autoplot(diff(goog))

# ACF of the differenced series
ggAcf(diff(goog))

# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")







