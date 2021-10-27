#Loading package
library(tidyverse)
library(skimr)
library(corrplot)
library(lubridate)
library(car)
library(leaps)
library(caret)
library(ggplot2)

#Loading data
house <- "dataAfterPrecessing_2.csv" %>% 
  read_csv() %>% 
  na.omit()  %>% 
  mutate(tradeYear =  year(tradeTime),
         tradeMonth =  month(tradeTime),
         tradeQuarter =  quarter(tradeTime))%>%
  select(-tradeTime, -price, -Outliers)
house %>% glimpse()

# Checking Distribution
hist(house$totalPrice)
hist(log(house$totalPrice))
hist(sqrt(house$totalPrice))
hist(1/(house$totalPrice))

#Checking linear relationship
corr <- cor(house)
house1 <- house %>%
  select(-livingRoom, -drawingRoom,-bathRoom)
pdf(file = "corrPlot.pdf")
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  
         tl.col = "indianred4", addCoef.col = "black", number.digits = 2, 
         number.cex = 0.60, tl.cex = 0.7, 
         cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))
head(house)
leaps<-regsubsets(log(totalPrice) ~.,
                  data = house, nbest=5)
plot(leaps,scale = 'adjr2')
leaps<-regsubsets(log(totalPrice) ~.,
                  data = house1, nbest=5)
plot(leaps,scale = 'adjr2')

dev.off()
#model
model.fit = lm(log(totalPrice) ~., data = house)
summary(model.fit)
pdf(file = "plot.pdf")
par(mfrow=c(2,2)) 
plot(model.fit)
dev.off()

#model2
model2 <- house1 %>%
  select(totalPrice, Lng, Lat, square, renovationCon,
          district, communityAve, tradeYear)

model2.fit = lm(log(totalPrice) ~., data = model2)
summary(model2.fit)
pdf(file = "model2_plot.pdf")
par(mfrow=c(2,2)) 
plot(model2.fit)
dev.off()
#model1
model1 <- house %>%
  select(totalPrice, Lat, followers, square, renovationCon,
         buildingStruc, district, communityAve, tradeYear)

model1.fit = lm(log(totalPrice) ~., data = model1)
summary(model1.fit)
pdf(file = "plot_model1.pdf")
par(mfrow=c(2,2)) 
plot(model1.fit)
dev.off()

library(gvlma)
gvmodel <- gvlma(model1.fit)
summary(gvmodel)

plot(x=fitted(model1.fit),y=rstudent(model1.fit))
abline(h=3,col="red",lty=2)
abline(h=-3,col="red",lty=2)

#Plot Residule
hist(resid(model1.fit),probability = T)
lines(density(resid(model1.fit)),col='red')
# Find outlier


#optimization model
house <- "dataAfterPrecessing_2.csv" %>% 
  read_csv() %>% 
  na.omit()  %>% 
  mutate(tradeYear =  year(tradeTime) )%>%
  filter( Outliers == 0 ) %>%
  select(-tradeTime, -price)
house %>% glimpse()

model2 <- house %>%
  select(totalPrice, Lat, followers, square, renovationCon,
         buildingStruc, district, communityAve, tradeYear)

model2.fit = lm(log(totalPrice) ~., data = model2)
summary(model2.fit)
pdf(file = "plot_model2.pdf")
par(mfrow=c(2,2)) 
plot(model2.fit)
dev.off()

which(rstudent(model2.fit)>3)
hist(resid(model2.fit),probability = T)
lines(density(resid(model2.fit)),col='red')

gvmodel <- gvlma(model2.fit)
summary(gvmodel)