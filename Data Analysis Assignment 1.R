# (a) import the csv file
mydata <- read.csv(file.path("//Users/juliemattimoe/Desktop/MSPA_401/", "abalones.csv"), sep = " ")

# (b) check that you have 1036 observations with 8 variables
str(mydata)
# (c) Define VOLUME and RATIO variables
mydata$VOLUME <- mydata$LENGTH * mydata$DIAM * mydata$HEIGHT
mydata$RATIO <- mydata$SHUCK / mydata$VOLUME
# (1)(a) get the summary
summary(mydata)
# (1)(b) get the table
sex <- mydata$SEX
class <- mydata$CLASS
mytable <- xtabs(~ SEX + CLASS, data = mydata)
addmargins(mytable)
#create barplot
barplot(mytable, main="Class Membership, Sex-Differentiated",
        ylab = "Frequency", xlab="Class",
        beside = TRUE, col=c("blue","red", "green"),legend = c("Female","Infant", "Male")
)
# (1)(c) using set.seed
set.seed(123)
work <- mydata[sample(1:nrow(mydata), 200,
                      replace=FALSE),]
plot(work[, 2:6])
# (2)(a) ?plot() to review documentation page
plot(mydata$VOLUME, mydata$WHOLE, main = "Whole Weight as a Function of Volume",
     xlab="Volume", ylab = "Whole Weight", col = "blue")
# (2)(b) ?plot(), ?abline() to review documentation pages
## Example plot(), using abline()
plot(mydata$WHOLE, mydata$SHUCK, main = "Shuck Weight as a Function of Whole Weight",
     xlab="Whole Weight", ylab = "Shuck Weight", col = "red")
slope <- max(mydata$SHUCK/mydata$WHOLE)
abline(a=0,b=slope, col = "black")
# (3)(a) Use “mydata” to present a display showing histograms, boxplots and Q-Q plots of RATIO
# differentiated by sex.
FemaleRatio <- mydata[mydata$SEX == "F", "RATIO"]
InfantRatio <- mydata[mydata$SEX == "I", "RATIO"]
MaleRatio <- mydata[mydata$SEX == "M", "RATIO"]
par(mfrow = c(3,3))
hist(FemaleRatio, col = "red", xlab = "Ratio")
hist(InfantRatio, col = "green", xlab = "Ratio")
hist(MaleRatio, col = "blue", xlab = "Ratio")
boxplot(FemaleRatio, col = "red", ylim = c(0,0.35))
boxplot(InfantRatio, col = "green", ylim = c(0,0.35))
boxplot(MaleRatio, col = "blue", ylim = c(0,0.35))
qqnorm(FemaleRatio, col = "red", ylim = c(0,0.3))
qqline(FemaleRatio)
qqnorm(InfantRatio, col = "green", ylim = c(0,0.3))
qqline(InfantRatio)
qqnorm(MaleRatio, col = "blue", ylim = c(0,0.3))
qqline(MaleRatio)
par(mfrow = c(1, 1))
# (4)(a) Side-by-side boxplots and scatter base R
par(mfrow = c(2, 2))
boxplot(mydata$VOLUME ~ mydata$CLASS, data = mydata, xlab = "CLASS", ylab = "VOLUME")
boxplot(mydata$WHOLE ~ mydata$CLASS, data = mydata, xlab = "CLASS", ylab = "WHOLE")
plot(mydata$RINGS, mydata$VOLUME, xlab = "RINGS", ylab = "VOLUME")
plot(mydata$RINGS, mydata$WHOLE, xlab = "RINGS", ylab = "WHOLE")
par(mfrow = c(1, 1))
# (5)(a) compute the mean values of VOLUME, SHUCK and RATIO for each combination of SEX and
CLASS.
Volume1 <- aggregate(VOLUME ~ SEX+CLASS, data = mydata, mean)
Shuck1 <- aggregate(SHUCK ~ SEX+CLASS, data = mydata, mean)
Ratio1 <- aggregate(RATIO ~ SEX+CLASS, data = mydata, mean)
dNames <- list(c("Female","Infant","Male"), levels(vMean$CLASS))
Volume <- matrix(nrow=3, ncol=6, dimnames=dNames)
Volume [cbind(vMean$SEX, vMean$CLASS)] <- vMean$VOLUME
Volume <- round(Volume, digits = 2)
Volume
Shuck <- matrix(nrow=3, ncol=6, dimnames=dNames)
Shuck [cbind(sMean$SEX, sMean$CLASS)] <- sMean$SHUCK
Shuck <- round(Shuck, digits = 2)
Shuck
Ratio <- matrix(nrow=3, ncol=6, dimnames=dNames)
Ratio [cbind(rMean$SEX, rMean$CLASS)] <- rMean$RATIO

Ratio <- round(Ratio, digits = 4)
Ratio
# (5)(b) Present three graphs.
library(ggplot2)
ggplot(rMean, aes(x = CLASS, y = RATIO, group = SEX, color = SEX)) +
  geom_line() +
  ggtitle("Mean RATIO per CLASS")
ggplot(vMean, aes(x = CLASS, y = VOLUME, group = SEX, color = SEX)) +
  geom_line() +
  ggtitle("Mean VOLUME per CLASS")
ggplot(sMean,aes(x = CLASS, y = SHUCK, group = SEX, color = SEX)) +
  geom_line() +
  ggtitle("Mean SHUCK per CLASS")