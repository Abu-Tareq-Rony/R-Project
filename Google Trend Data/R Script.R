library(dplyr)

df <- read.csv("gtrends_TinderPremium.csv", skip = 2)

df$Months <- format(as.Date(df$Week, format="%Y-%m-%d"), "%Y-%m")

df <- df %>% 
  group_by(Months) %>% 
  summarize(`Tinder Premium` = sum(Tinder.Premium...Worldwide.))

df
df2 <- read.csv("Tinder_IAPrM.csv", skip = 2)
df2
df2 <- df2 %>% rename(Months = X)
df2

df3 <- merge(x=df, y=df2, by="Months")
data <- na.omit(df3)                           
data

x = select(data, `Tinder Premium`)
x

#Calculate Pearson's correlation coefficient 
x<-as.numeric(as.factor(data$`Tinder Premium`))
x
y<-as.numeric(as.factor(data$SUM))
y
#correlation
cor.test(x, y)

#testing for data normality
ggplot(data=data, aes(x=x)) + stat_bin(bins = 30)
#Check if the data is normally distributed?
shapiro.test(x)
#Calculate the correlation again, this time with method="kendall"
cor.test(x, y, method="kendall")

#Build a regression model
Model <- lm(y ~ x, data=data)
# Have a look at it, using summary
summary(Model)
