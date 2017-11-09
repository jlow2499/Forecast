###1 Simple exponential smoothing

# Use ses() to forecast the next 10 years of winning times
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))

###SES vs naive

# Create a training set using subset.ts()
train <- subset.ts(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)

# Calculate forecast accuracy measures
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)

# Save the best forecasts as fcbest
fcbest <- fcnaive

###3 Holt's trend methods

# Produce 10 year forecasts of austa using holt()
fcholt <- holt(austa,h=10)

# Look at fitted model using summary()
summary(fcholt)

# Plot the forecasts
autoplot(fcholt)

# Check that the residuals look like white noise
checkresiduals(fcholt)

###4 Holt-Winters with monthly data

# Plot the data
autoplot(a10)

# Produce 3 year forecasts
fc <- hw(a10, seasonal = 'multiplicative', h = 3*12)

# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE

# Plot forecasts
autoplot(fc)

###5 Holt-Winters method with daily data

# Create training data with subset()
train <- subset.ts(hyndsight, end = length(hyndsight)-(4*7))

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = 'additive', h = (4*7))

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train,h=4*7)

# Find better forecasts with accuracy()
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)

###6 Automatic forecasting with exponential smoothing

# Fit ETS model to austa in fitaus
fitaus <- ets(austa)

# Check residuals
checkresiduals(fitaus)

# Plot forecasts
autoplot(forecast(fitaus))

# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
autoplot(forecast(fiths))

# Which model(s) fails test? (TRUE or FALSE)
fitausfail <- FALSE
fithsfail <- TRUE

###7 ETS vs seasonal naive

# Function to return ETS forecasts
fets <- function(y, h) {
  forecast(ets(y), h = h)
}

# Apply tsCV() for both methods
e1 <- tsCV(cement, fets, h = 4)
e2 <- tsCV(cement, snaive, h = 4)

# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

# Copy the best forecast MSE
bestmse <- mean(e2^2, na.rm=TRUE)

###* When does ETS fail?

# Plot the lynx series
autoplot(lynx)

# Use ets() to model the lynx series
fit <- ets(lynx)

# Use summary() to look at model and parameters
summary(fit)

# Plot 20-year forecasts of the lynx series
fit %>% forecast(h=20) %>% autoplot()
