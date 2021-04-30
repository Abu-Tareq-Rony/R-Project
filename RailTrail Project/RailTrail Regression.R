library(mosaic)
data(RailTrail)
head(RailTrail)
##Dataset Description:
##The Pioneer Valley Planning Commission (PVPC) collected data north of Chestnut Street in Florence, MA for ninety days from April 5, 2005 to November 15, 2005. Data collectors set up a laser sensor, with breaks in the laser beam recording when a rail-trail user passed the data collection station.


##Build a regression model to predict the volume of trail users using hightemp, and precip.
##response variable=Dependent variable==volume
trailusers_lm <- lm(volume ~ hightemp + precip, 
                    data = RailTrail)

# View summary of model 1
summary(trailusers_lm)

##numeric variable=decimal value
##explanatory variable = Independent variable==hightemp
##main term =independent variable==hightemp
trailusers_lm1 <- lm(volume ~ hightemp, 
                    data = RailTrail)
summary(trailusers_lm1)
##r2, when expressed as a percent, represents the percent of variation in the dependent (predicted) variable y that can be explained by variation in the independent (explanatory) variable x using the regression (best-fit) line.
trailusers_lm1$coefficients
p <- ggplot(RailTrail, aes(cloudcover , hightemp)) +geom_point()
p+geom_abline()

##catagorical variable=weekday
q=lm(weekday ~hightemp,data=RailTrail)
p <- ggplot(RailTrail, aes(weekday, hightemp)) +geom_point()
p+geom_abline()
q=lm(weekday ~hightemp*lowtemp  ,data=RailTrail)
q
p <- ggplot(RailTrail, aes(weekday, hightemp*lowtemp)) +geom_point()
p+geom_abline()

lm(volume ~ hightemp + lowtemp + avgtemp,data=RailTrail)


library(ggplot2)
ggplot(data = RailTrail, aes(x = volume  , y = lowtemp))+ 
  geom_point(color = "black", alpha = 0.25, shape = 16, size = 1) +
  geom_smooth(method = MASS::glm.nb, formula = y ~ x, color = "darkgreen", se = TRUE, fill = "green", alpha = 0.35) +
  labs(x = "something", y = "N") +
  theme_bw()+geom_abline(intercept = 60)

