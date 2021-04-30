##Install and load the car and mosaic libraries
install.packages("car")
install.packages("mosaic")
library(mosaic)
library(car)
data("mtcars")#Load data


##Motor Trend is a magazine about the automobile industry. 
##Looking at a data set of a collection of cars, they are interested in exploring the relationship between a set of variables and miles per gallon (MPG) (outcome).
##The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models).
head(mtcars)
summary(mtcars) # Exploratory Analysis
# mean
aggregate(mpg~am, data=mtcars, mean)


#Make sure values are numeric!
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Automatic", "Manual")
levels(mtcars$am)


#Null hypothesis(H0)=transmission type (am) affect miles/gallon (mpg).
#Alternative hypothesis(Ha).
cortest <- cor.test(mtcars$mpg, as.numeric(mtcars$am))
cortest$p.value; cortest$conf.intdata()
##p value is much less than alpha(0.05) hence significant correlation.Which the null hypothesis could be rejected



#Null hypothesis(H0)= horsepower (hp) affect 1/4 mile time (qsec).
#Alternative hypothesis(Ha).
cortest <- cor.test(mtcars$hp, as.numeric(mtcars$qsec))
cortest$p.value; cortest$conf.int
##p value is much less than alpha(0.05) hence significant corelation.Which the null hypothesis could be rejected



##Perform t test
autoData <- mtcars[mtcars$am == "Automatic",]
manualData <- mtcars[mtcars$am == "Manual",]
ttest <- t.test(autoData$mpg, manualData$mpg)
ttest
##The p-value is 0.0014, so we can reject the null hypothesis and conclude automatic has low mpg compared with manual cars. This ratifies ourobservation as seen in the above boxplot graph titled "MPG by Transmission Type".


##boxplot for EX1
library(ggplot2)
library(gridExtra)
ggplot(mtcars, aes(x=factor(am),y=mpg,fill=factor(am)))+
  geom_boxplot(notch=F)+ 
  scale_x_discrete("Transmission")+
  scale_y_continuous("Miles per Gallon")+
  ggtitle("MPG by Transmission Type")



#Check if mpg from EX1 is normally distributed
par(mfrow=c(1, 2))
# Histogram with Normal Curve
x <- mtcars$mpg
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon",
        main="Histogram Of MPG")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
# Kernel Density Plot
d <- density(mtcars$mpg)
plot(d, xlab="MPG", main ="Density Of MPG")


best_fit <- lm(mpg~am+wt+qsec, data=mtcars)
par(mfrow=c(2,2))
plot(best_fit)


#t-test to determine if manual vs. automatic transmission cars have different mpg
library(knitr)
install.packages("printr")
library(printr)
kable(head(mtcars),align = 'c')
test <- t.test(mpg ~ am, data= mtcars, var.equal = FALSE, paired=FALSE ,conf.level = .95)
result <- data.frame( "t-statistic"  = test$statistic, 
                      "df" = test$parameter,
                      "p-value"  = test$p.value,
                      "lower CL" = test$conf.int[1],
                      "upper CL" = test$conf.int[2],
                      "automatic mean" = test$estimate[1],
                      "manual mean" = test$estimate[2],
                      row.names = "")
kable(x = round(result,3),align = 'c')
##Notice that each line of mtcars represents one model of car, which we can see in the row names. Each column is then one attribute of that car, such as the miles per gallon (or fuel efficiency), the number of cylinders, the displacement (or volume) of the car's engine in cubic inches, whether the car has an automatic or manual transmission, and so on.


#Wilcoxon-Mann_Whitney Test
wilcox.test(mpg ~ am, data=mtcars) 

wilcox.test(mtcars$mpg, as.numeric(mtcars$am), paired = TRUE, alternative = "two.sided")
##Unlike the independent-samples t-test, the Mann-Whitney U test allows you to draw different conclusions about your data depending on the assumptions you make about your data's distribution.