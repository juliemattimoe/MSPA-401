#1a
install.packages("rockchalk")
library(rockchalk)
mydata <- read.csv(file.path("//Users/juliemattimoe/Desktop/MSPA_401/", "abalones.csv"), sep = " ")
RATIO <- mydata$RATIO
hist(RATIO)
qqnorm(RATIO)
qqline(RATIO)
skewness(RATIO, na.rm = TRUE, unbiased = TRUE)
kurtosis(RATIO, na.rm = TRUE, unbiased = TRUE)
# Checking for normal distribution of RATIO
normality <- shapiro.test(RATIO)
# Shapiro-Wilks Test is used to check for normality. If P-Value is >0.05 then the
distribution is normal
ifelse(normality$p.value > 0.05,"Normal","Not Normal")
> Not Normal
#1b
L_RATIO=log10(RATIO)
hist(L_RATIO)
qqnorm(L_RATIO)
qqline(L_RATIO)
skewness(L_RATIO, na.rm = TRUE, unbiased = TRUE)
kurtosis(L_RATIO, na.rm = TRUE, unbiased = TRUE)
par(mfrow = c(1,1)) #changed from 6 to 1 so that only one chart is displayed
plot(L_RATIO~mydata$CLASS ,ylab = "Log Ratio",xlab="Class") #added x axis and y-axis
label
#1c
bartlett.test(L_RATIO ~ CLASS, data=mydata)
p-value is 0.6884
Thus, we cannot reject the null hypothesis that the variance is same across all CLASS
#2a
anova1 <- aov(L_RATIO ~ CLASS*SEX, data = mydata)
summary(anova1)
anova2 <- aov(L_RATIO ~ CLASS+SEX, data = mydata)
summary(anova2)
#2b
TukeyHSD(anova2)
# The results suggest that the difference between Male and Female is not significant.
#Hence they can be combined
#3a
mydata$TYPE <- combineLevels(mydata$SEX, levs = c("M","F"), "ADULT")
TYPE <- mydata$TYPE
par(mfrow=c(1,2))
hist(mydata$VOLUME[mydata$TYPE == "I"],xlab = "Volume",main="Histogram - I")
hist(mydata$VOLUME[mydata$TYPE == "ADULT"],xlab = "Volume",main="Histogram -
     ADULT")
# ADULT RESULTED IN A LESS SKEWNESS OF THE DATA
#3b
install.packages('ggplot2')
library(ggplot2)
mycolors = c('pink','yellow','green','blue','violet','red')
with(mydata, plot(SHUCK,VOLUME, col = mycolors[CLASS],pch = 18))
with(mydata, legend('topright',legend=levels(CLASS),col=mycolors, pch = 18,
                    title='CLASS'))
mydata$L_SHUCK <- log(mydata$SHUCK)
mydata$L_VOLUME <- log(mydata$VOLUME)
with(mydata, plot(L_SHUCK,L_VOLUME, col = mycolors[CLASS],pch = 18))
with(mydata, legend('topright',legend=levels(CLASS),col=mycolors, pch = 18,
                    title='CLASS'))
# Based on the classes using log each of them is segregated. Class A1 appears in (0,3) to
(0,3) range. Other classes as one goes towards top right.
mycolors = c('green','blue')
with(mydata, plot(SHUCK,VOLUME, col = mycolors[TYPE],pch = 18))
with(mydata, legend('topright',legend=levels(TYPE),col=mycolors, pch = 18,
                    title='CLASS'))
mydata$L_SHUCK <- log(mydata$SHUCK)
mydata$L_VOLUME <- log(mydata$VOLUME)
with(mydata, plot(L_SHUCK,L_VOLUME, col = mycolors[TYPE],pch = 18))
with(mydata, legend('topright',legend=levels(TYPE),col=mycolors, pch = 18,
                    title='CLASS'))
#4a
Model1 <- lm(L_SHUCK~L_VOLUME+CLASS+TYPE, data = mydata)
summary(Model1)
#4b
index <- (mydata$CLASS == "A5")|(mydata$CLASS == "A4")
mydata$TYPE[index] <- combineLevels(mydata$TYPE[index],
                                    levs = c("I", "ADULT"), "ADULT")
Model2 <- lm(L_SHUCK~L_VOLUME+CLASS+TYPE, data = mydata)
summary(Model2)
# The coefficients of classA4 and classA5 have decreased in value
#5a
Model2.stdres = rstandard(Model2)
x = Model2$residuals
h <- hist(Model2$residuals, breaks=20, col="blue",xlab="Residuals")
qqnorm(Model2.stdres, ylab="Sample Quantiles", xlab="Theoretical Quantiles", col =
         "red")
qqline(Model2.stdres,ylab="Sample Quantiles", xlab="Theoretical Quantiles", col = 
         "black")
library(rockchalk)
skewness(x, na.rm = TRUE, unbiased = TRUE)
kurtosis(x, na.rm = TRUE, unbiased = TRUE)
#5b
#Plot the residuals versus L_VOLUME coloring the data points by CLASS
ggplot(Model1, aes(x = L_VOLUME,y = Model1$residuals)) + geom_point(aes(color =
                                                                          CLASS)) + labs(x = "L_VOLUME", y = "Residual")
#second time coloring the data points by SEX
ggplot(Model1, aes(x = L_VOLUME,y = Model1$residuals)) + geom_point(aes(color =
                                                                          TYPE)) + labs(x = "L_VOLUME", y = "Residual")
#boxplot
install.packages('RColorBrewer')
library(RColorBrewer)
boxplot(split(Model1$residuals, mydata$CLASS))
boxplot(split(Model1$residuals, mydata$TYPE))
bartlett.test(c ~ CLASS, data = mydata)
#6a
idxi <- mydata$TYPE=="I"
idxa <- mydata$TYPE=="ADULT"
max.v <- max(mydata$VOLUME)
min.v <- min(mydata$VOLUME)
delta <- (max.v - min.v)/1000
prop.infants <- numeric(0)
prop.adults <- numeric(0)
volume.value <- numeric(0)
total.infants <- length(mydata$TYPE[idxi])
total.adults <- length(mydata$TYPE[idxa])
for (k in 1:1000) {
  value <- min.v + k*delta
  volume.value[k] <- value
  prop.infants[k] <- sum(mydata$VOLUME[idxi] <= value)/total.infants
  prop.adults[k] <- sum(mydata$VOLUME[idxa] <= value)/total.adults
}
n.infants <- sum(prop.infants <= 0.5)
split.infants <- min.v + (n.infants + 0.5)*delta # This estimates the desired volume.
n.adults <- sum(prop.adults <= 0.5) split.adults <- min.v + (n.adults + 0.5)*delta
#6b
par(mfrow = c(1,1))
plot(volume.value, prop.infants, col = "red", main = "Proportion of Infants Not
     Harvested",
     type = "l", lwd = 2)
abline(h=0.5)
abline(v = split.infants)
lines(volume.value, prop.adults, col = "blue", main = "Proportion of adults Not
Harvested",
      type = "l", lwd = 2)
abline(h=0.5)
abline(v = split.adults)
#7a
difference <- (1-prop.adults) - (1-prop.infants)
plot(difference,volume.value)
# ?plot(), ?abline(), ?text() to review documentation pages
#7b
# loess, local polynomial regression fitting
y.loess.a <- loess(1-prop.adults ~ volume.value, span = 0.25, family = c("symmetric"))
y.loess.i <- loess(1-prop.infants ~ volume.value, span = 0.25, family = c("symmetric"))
smooth.difference <- predict(y.loess.a) - predict(y.loess.i)
#7c
plot(volume.value, difference, col = "red", main = "Difference in Harvest Proportion",
     type = "l", lwd = 2)
abline(v = volume.value[which(difference == max (difference))])
#7d
(1-prop.infants)[which.max(smooth.difference)]
#8a
volume.value[volume.value > max(mydata[mydata$CLASS == "A1" &
                                         mydata$TYPE == "I", "VOLUME"])][1]
# Now, to determine the proportions harvested, we can look to the proportions
# of infants and adults with VOLUMEs greater than this threshold.
# For example, for infants:
sum(mydata[mydata$TYPE == "I", "VOLUME"] > 211.9447) /
  sum(mydata$TYPE == "I")
#8b
volume.value[which.min(abs(prop.adults - (1-prop.infants)))]
sum(mydata[mydata$TYPE == "I", "VOLUME"] > 211.9447) /
  sum(mydata$TYPE == "I")
#9
plot(a,b, col = "blue", xlab = "Infant harvest proportion", ylab = "Adult harvest
     proportion",
     type = "l", lwd = 2)
abline(0,1)
install.packages("flux")
library(flux)
auc(a,b)
#10
TruePostiveRate <- 1-prop.adults
FalsePositiveRate <- 1-prop.infants
TotalHarvestPopulation <- volume.value/ (total.adults + total.infants) 
df <- data.frame(TruePostiveRate,FalsePositiveRate,TotalHarvestPopulation)