library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(fda)

################################################################################
###################################################################### GAM MODEL
################################################################################

library(mgcv)
data <- read.csv("data_regression.csv", header=TRUE, dec = ',', sep = ';')

Y_PM10 = data[,7]  # extract vector of all Pm10 values ordered by date
Y_PM10 = replace(Y_PM10, Y_PM10==0, 1)
Y_PM10_log = log(Y_PM10)

rain = as.numeric(data[,19])
#rain as a binary variable: 1 rain, 0 no rain
rain_binary=rain
rain_binary[which(rain_binary>0)]=1

wind = as.numeric(data[,20])
#wind as a binary variable: 1 wind, 0 no wind
wind_binary=wind
wind_binary[which(wind_binary>20)]=1

data[which(data$Date>'2015-09-01' & data$Date<'2016-12-31'),22]=1516
data[which(data$Date>'2016-09-01' & data$Date<'2017-12-31'),22]=1617
data[which(data$Date>'2017-09-01' & data$Date<'2018-12-31'),22]=1718
data[which(data$Date>'2018-09-01' & data$Date<'2019-12-31'),22]=1819
data[which(data$Date>'2019-09-01' & data$Date<'2020-12-31'),22]=1920
data[which(data$Date>'2020-09-01' & data$Date<'2021-12-31'),22]=2021
anno=data[,22] # categorical variable over years

data_gam = as.data.frame(cbind(Y_PM10_log,data[,18],as.numeric(data[,23]),as.numeric(data[,11]),
                               rain_binary,as.numeric(data[,20]),data[,17],data[,16],anno))
names(data_gam) = c('Y_PM10_log','temp','date','quota','rain_binary','wind','long','lat','anno')
data_gam[,5]=as.factor(data_gam[,5])
data_gam[,9]=as.factor(data_gam[,9])

#gam
model_gam = gam(Y_PM10_log ~ s(temp) + s(date,bs='cc') + 
                  quota + rain_binary + wind +
                  s(long,lat), data = data_gam)

#wind is better as linear

##############################
######################## plots
##############################

library(mgcViz)

model_viz = getViz(model_gam)

#1. temperature 
plot( sm(model_viz, 1) )+ l_fitLine(colour = "firebrick3", lwd=1.2) +
  l_rug(mapping = aes(x=x, y=y), alpha = 0.01) +
  l_ciLine(mul = 5, colour = "dodgerblue4", linetype = 2, lwd=1.2) + 
  l_points(shape = 18, size = 0.5, alpha = 0.08) +
  theme_get() +
  labs(x="Temperature", y="Smoothing", title="Temperature spline")

#2. time
plot( sm(model_viz, 2) )+ l_fitLine(colour = "darkorchid", lwd=1.2) +
  l_rug(mapping = aes(x=x, y=y), alpha = 0.01) +
  l_ciLine(mul = 5, colour = "dodgerblue4", linetype = 2, lwd=1.2) + 
  l_points(shape = 18, size = 0.5, alpha = 0.08) +
  theme_get() +
  labs(x="Time", y="Smoothing", title="Time cyclic spline")

#3. altitude
plot( model_viz, select=4, ylim =c(-0.8, 0.5))+ l_fitLine(colour = "chartreuse4", lwd=1.2) +
  l_ciLine(mul = 5, colour = "dodgerblue4", linetype = 2, lwd=1.2) + 
  theme_get() +
  labs(x="Altitude", y="Regression Line", title="Altitude contribution")

#4. rain
plot( model_viz, select=5 ) +
  theme_get() +
  labs(x="Rain (binary)", y="Regression coefficients", title="Rain contribution")

#5. wind
plot( model_viz, select=6)+ l_fitLine(colour = "darkorange1", lwd=1.2) +
  l_ciLine(mul = 5, colour = "dodgerblue4", linetype = 2, lwd=1.2) + 
  theme_get() +
  labs(x="Wind", y="Regression Line", title="Wind contribution")


#6. plot of latitude and longitude regressor + Lombardy map

library(sp)
library(rgdal)
library(raster)
library(plotly)
library(akima)
# load Italy spatial data
italy <- getData('GADM', country = 'Italy', level = 1)
italy$NAME_1
lomb_map <- italy[10,]  #Lombardy columns
m= as(lomb_map,"data.frame")
rwa2 <- fortify(lomb_map)

write.table(rwa2, "coordinate_lombardia.csv", sep = ";", dec = ",", row.names=FALSE)
coords <- read.csv("coordinate_lombardia.csv", header=TRUE, dec = ',', sep = ';')

vis.gam(model_gam, view = c("long", "lat"), plot.type = "contour", too.far = 0.8,xlim=c(8.7,11.3),ylim=c(45,46.5))
points(coords[1:6992, 1],coords[1:6992, 2],pch=16,type = 'l', lwd=2)

#add 5 important cities on the map:
#MILANO 
points(x=9.235491, y=45.47900, lwd=3, pch=16,col='black')
text(9.285491, 45.52900, labels = "Milano")
#BRESCIA 
points(x=10.2088905, y=45.5287318, lwd=3, pch=16,col='black')
text(10.2588905, 45.5787318, labels = "Brescia")
#CREMONA
points(x=10.02307, y=45.13335,  lwd=3, pch =16,col='black')
text(10.07307, 45.18335, labels = "Cremona")
#LECCO 
points(x=9.3971321, y=45.8608416,  lwd=3, pch =16,col='black')
text(9.4471321, 45.9108416, labels = "Lecco")
#SONDRIO 
points(x=9.8801831, y=46.1684478,  lwd=3, pch =16,col='black')
text(9.9301831, 46.2184478, labels = "Sondrio")

#3d plot
vis.gam(model_gam, view = c("long", "lat"), plot.type = "persp", too.far = 0.8, theta = 220)

################################################################################
########################################### GAM MODEL with the dummy on the year
################################################################################

model_gam1 = gam(Y_PM10_log ~ s(temp) + s(date,bs='cc') + 
                   quota + rain_binary + wind + anno +
                   s(long,lat), data = data_gam)

model_viz = getViz(model_gam1) 

plot( model_viz, select=7 ) +
  theme_get() +
  labs(x="", y="Regression coefficients", title="Rain contribution")

############ Validation ##############

hist(model_gam$residuals)
qqnorm(model_gam$residuals)
qqline(model_gam$residuals)

hist(model_gam1$residuals)
qqnorm(model_gam1$residuals)
qqline(model_gam1$residuals)

########## prediction

##prediction on last months of 2021
data_pred <- read.csv("data_pred2021.csv", header=TRUE, dec = ',', sep = ';')

Y_PM102021 = data_pred[,7]  # extract vector of all Pm10 values ordered by date
Y_PM102021 = replace(Y_PM102021, Y_PM102021==0, 1)
Y_PM102021_log = log(Y_PM102021)

rain2021 = as.numeric(data_pred[,19])
#rain as a binary variable: 1 rain, 0 no rain
rain_binary2021=rain2021
rain_binary2021[which(rain_binary2021>0)]=1

data_try = as.data.frame(cbind(as.numeric(data_pred[,23]),Y_PM102021_log,data_pred[,18],as.numeric(data_pred[,20]),
                               rain_binary2021,as.numeric(data_pred[,11]),data_pred[,16],data_pred[,17]))
names(data_try) = c("date", "Y_PM10_log","temp","wind","rain_binary","quota","lat","long")

data_prediction = predict.gam(model_gam, newdata=data_try)

#log(Y_PM10)
data_pred[,7] = replace(data_pred[,7], data_pred[,7]==0, 1)
data_pred[,7] = log(data_pred[,7])

stazione_prova = which(data_pred$Idstazione==705) #milano citta studi
predict_stazione_prova = data_prediction[stazione_prova]
plot(1:118, data_prediction[stazione_prova], col='red', type='l', ylim = c(0,5))
points(1:118, data_pred[stazione_prova,7], type='l', ylim = c(0,5))
