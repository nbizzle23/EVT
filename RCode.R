# Supplementary R Code 

## General Extreme Value Distributions

x <- seq(-10, 10, by=0.1)
Gumbel_density <- exp(-x-exp(-x))
Frechet_density <- dgev(x, xi=0.8, mu=0)
Weibull_density <- dgev(x, xi=-0.3, mu=0)
plot(c(x,x,x), c(Gumbel_density,Frechet_density, Weibull_density),
     type='n', xlab="x", ylab=" ",las=1)
lines(x, Gumbel_density, type='l', lty=1, col='green')
lines(x, Weibull_density, type='l', lty=2, col='blue')
lines(x, Frechet_density, type='l', lty=3, col='red')
legend('topright', legend=c('Gumbel','Frechet','Weibull'), lty=c(1,2,3), col=c('green','blue','red'))


## Gumbel Distribution

GumbelDistribution <- data.frame(x, Gumbel_density)
names(GumbelDistribution) <- c("Time", "Observations")
head(GumbelDistribution)
plot(GumbelDistribution, col="green", pch=19, cex=0.8,
     main="Plot of Extreme Value Distribution - Gumbel")

#Simulation of the Gumbel Distribution    

x <- seq(-10, 10, by=0.1)
Gumbel_density <- exp(-x-exp(-x))
GumbelDistribution <- data.frame(x, Gumbel_density)
names(GumbelDistribution) <- c("Time", "Observations")
head(GumbelDistribution)



#Frechet Distribution

FrechetDistribution <- data.frame(x, Frechet_density)
names(FrechetDistribution) <- c("Time", "Observations")
head(FrechetDistribution)
plot(FrechetDistribution, col="blue", pch=19, cex=0.8,
     main="Plot of Extreme Value Distribution - Frechet")

#Simulation of the Frechet Distribution 

x <- seq(-10, 10, by=0.1)
Frechet_density <- dgev(x, xi=0.8, mu=0)
FrechetDistribution <- data.frame(x, Frechet_density)
names(FrechetDistribution) <- c("Time", "Observations")
head(FrechetDistribution)
plot(FrechetDistribution, col="blue", pch=19, cex=0.8,
     main="Plot of Extreme Value Distribution - Frechet")

#Weibull Distribution   
WeibullDistribution <- data.frame(x, Weibull_density)
names(WeibullDistribution) <- c("Time", "Observations")
head(WeibullDistribution)
plot(WeibullDistribution, col="red", pch=19, cex=0.8,
     main="Plot of Extreme Value Distribution - Weibull")

#Simulation the Weibull Distribution 
x <- seq(-10, 10, by=0.1)
Weibull_density <- dgev(x, xi=-0.3, mu=0)
WeibullDistribution <- data.frame(x, Weibull_density)
names(WeibullDistribution) <- c("Time", "Observations")
head(WeibullDistribution)
plot(WeibullDistribution, col="red", pch=19, cex=0.8,
     main="Generated Plot of the Weibull Distribution")


#Block Maxima Method 
set.seed(123)
library(extRemes)

## Block size
n <- 12
original_mean <- 5
original_sd <- 2

## Create a series of maxima
series_length <- 200
maxima <- c()

## Simulate blocks of data
data_series <- list()
for (i in 1:series_length) {
  data_series[[i]] <- rnorm(n = n, mean = original_mean, sd = original_sd)
}
maxima <- unlist(lapply(data_series, max))

# Block Maxima Plot
plot(maxima, main = "Block maxima", type = "l")

# Empirical vs Gumbel Plot
fit <- fevd(maxima, type = "Gumbel")
plot(fit, type = "density", main = "Empirical density vs estimated Gumbel distribution")

#Peak-Over-Threshold Method 

## Setting the threshold u
threshold <- 7.5

#Peak Over Thresholds
plot(x = rep(1:series_length, each = n), y = unlist(data_series), main = "Peak Over Thresholds",
     sub = paste("threshold =", threshold), xlab = "series", ylab = "value")
pot_points <- unlist(data_series) > threshold
points(x = rep(1:series_length, each = n)[pot_points], y = unlist(data_series)[pot_points], col = "red")
abline(h = threshold, col = "red")

#Empirical POT vs  GP Plot
gp_fit <- fevd(unlist(data_series), threshold = threshold, type = "GP")
plot(gp_fit, type = "density", main = "Empirical POT exceedances density vs estimated GP distribution")

#Return level Plot
plot(gp_fit, type = "rl", main = "Return level")

