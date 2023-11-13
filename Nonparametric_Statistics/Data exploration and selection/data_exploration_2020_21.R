#loading necessary libraries
library(plyr)

################################################################################
################################## PLOT PM10 ###################################
################################################################################

PM10_lomb_2021 <- read.csv("matrixPM10_2021_noNA_sett.csv", header=TRUE, dec = ',', sep = ';')
PM10_lomb_2021 <- PM10_lomb_2021[,-8]   
colnames(PM10_lomb_2021)[66] = c("Numeric Date")
PM10_lomb_2020 <- read.csv("matrixPM10_2020_noNA_sett.csv", header=TRUE, dec = ',', sep = ';')
colnames(PM10_lomb_2020)[66] = c("Numeric Date")
PM10_lomb_20_21 <- rbind(PM10_lomb_2020,PM10_lomb_2021)

write.table(PM10_lomb_20_21, "matrixPM10_20_21_noNa_sett.csv", sep = ";", dec = ",", row.names=FALSE)

# daily average for each row

means = rowMeans(as.matrix(PM10_lomb_20_21[,-c(1,66)]), na.rm = TRUE)
PM10_lomb_20_21 <- data.frame(PM10_lomb_20_21, means)
mean_stations_lomb = colMeans(as.matrix(PM10_lomb_20_21[,-c(1,66)]), na.rm = TRUE)

# creating functional data
library(roahd)

PM10_mat = data.matrix(PM10_lomb_20_21)
PM10_mat = PM10_mat[,-c(1,66,67)]
PM10_func = fData( seq(1,365),t(PM10_mat))

PM10_mat_mean = data.matrix(PM10_lomb_20_21)
PM10_mean = PM10_mat_mean[,67]
PM10_func_mean = fData( seq(1,365),t(PM10_mean))

#plot of PM10 level of all the stations
x11()
quartz()
plot(PM10_func, ylim=c(0,150),main="PM10 level in 2020-2021", xlab='Days', ylab='PM10 level')
par(new=TRUE)
plot(PM10_func_mean, ylim=c(0,150), lwd='2', col = 'black')
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

################################################################################
################################ PLOT STAZIONI #################################
################################################################################

library("rnaturalearth")
library(ggplot2)
library(mapview)
library(dplyr)

# take stations coordinates

stazioni <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
stazioni_nomi <- as.character(stazioni_lomb)
stazioni <- stazioni[which(stazioni$NomeTipoSensore== 'PM10 (SM2005)'),]

stazioni_new <- stazioni[which(stazioni$NomeStazione== stazioni_nomi[1]),]
for (i in 2:length(stazioni_nomi)) {
  stazioni_new <- rbind(stazioni_new, stazioni[which(stazioni$NomeStazione== stazioni_nomi[i]),])
}
colnames(stazioni_new)[5] <- "Stazione"

# get italy provinces coordinates

it <- ggplot2::map_data("italy")
it <- ne_states(geounit = "italy", returnclass = "sf")

# plot all stations within Pianura Padana

lombardy= it[which(it$region %in% c("Lombardia")),]

mapview(stazioni_new, xcol="lng", ycol="lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations", 
        cex=3) + mapview(lombardy,map.types="OpenStreetMap",
                         alpha.regions=0.3, layer.name= "Regions",
                         col.regions="cornflowerblue")

################################################################################
############################## PLOT TEMPERATURE ################################
################################################################################

temperatura_lomb_2021 <- read.csv("matrixtemp_2021_noNa_sett.csv", header=TRUE, dec = ',', sep = ';')
temperatura_lomb_2021 <- temperatura_lomb_2021[,-8]   # elimino la stazione in più
colnames(temperatura_lomb_2021)[64] = c("Numeric Date")
temperatura_lomb_2020 <- read.csv("matrixtemp_2020_noNa_sett.csv", header=TRUE, dec = ',', sep = ';')
colnames(temperatura_lomb_2020)[64] = c("Numeric Date")
temp_lomb_20_21 <- rbind(temperatura_lomb_2020,temperatura_lomb_2021)

write.table(temp_lomb_20_21, "matrixtemp_20_21_noNa_sett.csv", sep = ";", dec = ",", row.names=FALSE)

# daily average for each row
means = rowMeans(as.matrix(temp_lomb_20_21[,-c(1,64)]), na.rm = TRUE)
temp_lomb_20_21 <- data.frame(temp_lomb_20_21, means)
mean_stations_temp = colMeans(as.matrix(temp_lomb_20_21[,-c(1,64)]), na.rm = TRUE)

# creating functional data
library(roahd)

temp_mat = data.matrix(temp_lomb_20_21)
temp_mat = temp_mat[,-c(1,64,65)]
temp_func = fData( seq(1,365),t(temp_mat))

temp_mat_mean = data.matrix(temp_lomb_20_21)
temp_mean = temp_mat_mean[,65]
temp_func_mean = fData( seq(1,365),t(temp_mean))

x11(width=14)
quartz()
plot(temp_func, ylim=c(-25,50), main="Temperature level in 2020-21", xlab='Days', ylab='Temperature level')
par(new=TRUE)
plot(temp_func_mean, ylim=c(-25,50), lwd='2', col = 'black')
legend(x='bottomright',legend = c('mean'), col = c('black'), lwd='2')

################################################################################
################################## PLOT RAIN ###################################
################################################################################

rain_lomb_2021 <- read.csv("matrixrain_2021_noNa_sett.csv", header=TRUE, dec = ',', sep = ';')
rain_lomb_2021 <- rain_lomb_2021[,-8]   # elimino la stazione in più
colnames(rain_lomb_2021)[66] = c("Numeric Date")
rain_lomb_2020 <- read.csv("matrixrain_2020_noNa_sett.csv", header=TRUE, dec = ',', sep = ';')
colnames(rain_lomb_2020)[66] = c("Numeric Date")
rain_lomb_20_21 <- rbind(rain_lomb_2020,rain_lomb_2021)

write.table(rain_lomb_20_21, "matrixrain_20_21_noNa_sett.csv", sep = ";", dec = ",", row.names=FALSE)

# daily average for each row
means = rowMeans(as.matrix(rain_lomb_20_21[,-c(1,66)]), na.rm = TRUE)
rain_lomb_20_21 <- data.frame(rain_lomb_20_21, means)
mean_stations_temp = colMeans(as.matrix(rain_lomb_20_21[,-c(1,66,67)]), na.rm = TRUE)

# creating functional data
library(roahd)

rain_mat = data.matrix(rain_lomb_20_21)
rain_mat = rain_mat[,-c(1,66,67)]
rain_func = fData( seq(1,365),t(rain_mat))

rain_mat_mean = data.matrix(rain_lomb_20_21)
rain_mean = rain_mat_mean[,67]
rain_func_mean = fData( seq(1,365),t(rain_mean))

x11(width=14)
quartz()
plot(rain_func, ylim=c(0,5), main="Rain level in 2020-21", xlab='Days', ylab='Rain level')
par(new=TRUE)
plot(rain_func_mean, ylim=c(0,5), lwd='2', col = 'black')
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

################################################################################
################################## PLOT WIND ###################################
################################################################################

wind_lomb_2021 <- read.csv("matrixwind_2021_noNa_sett.csv", header=TRUE, dec = ',', sep = ';')
wind_lomb_2021 <- wind_lomb_2021[,-c(7,35,36)]   # elimino la stazione in più
colnames(wind_lomb_2021)[56] = c("Numeric Date")

wind_lomb_2020 <- read.csv("matrixwind_2020_noNa_sett.csv", header=TRUE, dec = ',', sep = ';')
wind_lomb_2020 <- wind_lomb_2020[,-c(6,21,32,36,37,39,46,60)]
colnames(wind_lomb_2020)[56] = c("Numeric Date")

wind_lomb_20_21 <- rbind(wind_lomb_2020,wind_lomb_2021)

write.table(wind_lomb_20_21, "matrixwind_20_21_noNa_sett.csv", sep = ";", dec = ",", row.names=FALSE)

# daily average for each row
means = rowMeans(as.matrix(wind_lomb_20_21[,-c(1,56)]), na.rm = TRUE)
wind_lomb_20_21 <- data.frame(wind_lomb_20_21, means)
mean_stations_vel_vento = colMeans(as.matrix(wind_lomb_20_21[,-c(1,56,57)]), na.rm = TRUE)

# creating functional data

library(roahd)

wind_mat = data.matrix(wind_lomb_20_21)
wind_mat = wind_mat[,-c(1,56,57)]
wind_func = fData( seq(1,365),t(wind_mat))

wind_mat_mean = data.matrix(wind_lomb_20_21)
wind_mean = wind_mat_mean[,57]
wind_func_mean = fData( seq(1,365),t(wind_mean))

x11(width=14)
quartz()
plot(wind_func, ylim=c(0,30), main="Wind speed level in 2020-21", xlab='Days', ylab='Wind speed level')
par(new=TRUE)
plot(wind_func_mean, ylim=c(0,30), lwd='2', col = 'black')
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

################################################################################
############################# THREE PLOTS IN ONE ###############################
################################################################################

x11()
quartz()
par(mfrow=c(3,1))

plot(temp_func, ylim=c(-25,60), main="Temperature level in 2020-21", xlab='Days', ylab='Temperature level')
par(new=TRUE)
plot(temp_func_mean, ylim=c(-25,60), lwd='2', col = 'black')
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

plot(rain_func, ylim=c(0,5), main="Rain level in 2020-21", xlab='Days', ylab='Rain level')
par(new=TRUE)
plot(rain_func_mean, ylim=c(0,5), lwd='2', col = 'black')
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

plot(wind_func, ylim=c(0,10), main="Wind speed level in 2020-21", xlab='Days', ylab='Wind speed level')
par(new=TRUE)
plot(wind_func_mean, ylim=c(0,10), lwd='2', col = 'black')
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

