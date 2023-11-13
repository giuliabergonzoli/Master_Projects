new_data_emil <- read.csv("matrixemil.csv", header=TRUE, dec = ',', sep = ';')
new_data_lomb <- read.csv("matrixlomb.csv", header=TRUE, dec = ',', sep = ';')
new_data_ven <- read.csv("matrixven.csv", header=TRUE, dec = ',', sep = ';')
new_data_piem <- read.csv("matrixpiem.csv", header=TRUE, dec = ',', sep = ';')

stazioni <- read.csv("Stations_Coord.csv", header=TRUE, dec = '.', sep = ',')

new_data_emil[,1] = as.Date(new_data_emil[,1])
new_data_lomb[,1] = as.Date(new_data_lomb[,1])
new_data_ven[,1] = as.Date(new_data_ven[,1])
new_data_piem[,1] = as.Date(new_data_piem[,1])
#PLOTS EMILIA & LOMBARDIA
cols <- rainbow(dim(new_data_lomb)[2])

#Emilia
quartz()
x11()
par(mfrow=c(2,1))

plot(new_data_emil[,1],new_data_emil[,2], col=cols[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Emilia Romagna", ylab="PM2.5 level", xlab="Date")

for (i in 3:31) {
  par(new=TRUE)
  plot(new_data_emil[,1],new_data_emil[,i], col=cols[i], ylim=c(0,100), type='l', main="2018 PM2.5 level - Emilia Romagna", ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}
#daily average for each row
means_emil = rowMeans(as.matrix(new_data_emil[,-1]), na.rm = TRUE)
new_data_emil <- data.frame(new_data_emil, means_emil)
mean_stations_emil = colMeans(as.matrix(new_data_emil[,-1]), na.rm = TRUE)


par(new=TRUE)
plot(new_data_emil[,1],new_data_emil[,32], col='black', ylim=c(0,100), lwd = '2', type='l', main="2018 PM2.5 level - Emilia Romagna", ylab="PM2.5 level", xlab="Date", axes=FALSE)
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

#Lombardia

plot(new_data_lomb[,1],new_data_lomb[,2], col=cols[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Lombardia", ylab="PM2.5 level", xlab="Date")

for (i in 3:33) {
  par(new=TRUE)
  plot(new_data_lomb[,1],new_data_lomb[,i], col=cols[i], ylim=c(0,100), type='l', main="2018 PM2.5 level - Lombardia", ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}
#daily average for each row
means_lomb = rowMeans(as.matrix(new_data_lomb[,-1]), na.rm = TRUE)
new_data_lomb <- data.frame(new_data_lomb, means_lomb)
mean_stations_lomb = colMeans(as.matrix(new_data_lomb[,-1]), na.rm = TRUE)


par(new=TRUE)
plot(new_data_lomb[,1],new_data_lomb[,34], col='black', ylim=c(0,100), lwd = '2', type='l', main="2018 PM2.5 level - Lombardia", ylab="PM2.5 level", xlab="Date", axes=FALSE)
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

#LOG

for(i in dim(new_data_emil)[1]){
    for(j in 2:31){
      if(is.na(new_data_emil[i,j])==TRUE || new_data_emil[i,j]==0){
        new_data_emil[i,j]=1
      }
    }
}
new_data_emil[,2:31]=log(new_data_emil[,2:31])

for(i in dim(new_data_lomb)[1]){
  for(j in 2:33){
    if(is.na(new_data_lomb[i,j])==TRUE || new_data_lomb[i,j]==0){
      new_data_lomb[i,j]=1
    }
  }
}
new_data_lomb[,2:33]=log(new_data_lomb[,2:33])


cols <- rainbow(dim(new_data_lomb)[2])

#Emilia
quartz()
x11()
par(mfrow=c(2,1))

plot(new_data_emil[,1],new_data_emil[,2], col=cols[1], ylim=c(0,5), type='l', main="2018 PM2.5 level - Emilia Romagna", ylab="PM2.5 level", xlab="Date")

for (i in 3:31) {
  par(new=TRUE)
  plot(new_data_emil[,1],new_data_emil[,i], col=cols[i], ylim=c(0,5), type='l', main="2018 PM2.5 level - Emilia Romagna", ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}
#daily average for each row
means_emil = rowMeans(as.matrix(new_data_emil[,-1]), na.rm = TRUE)
new_data_emil <- data.frame(new_data_emil, means_emil)
mean_stations_emil = colMeans(as.matrix(new_data_emil[,-1]), na.rm = TRUE)


par(new=TRUE)
plot(new_data_emil[,1],new_data_emil[,32], col='black', ylim=c(0,5), lwd = '2', type='l', main="2018 PM2.5 level - Emilia Romagna", ylab="PM2.5 level", xlab="Date", axes=FALSE)
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

#Lombardia

plot(new_data_lomb[,1],new_data_lomb[,2], col=cols[1], ylim=c(0,5), type='l', main="2018 PM2.5 level - Lombardia", ylab="PM2.5 level", xlab="Date")

for (i in 3:33) {
  par(new=TRUE)
  plot(new_data_lomb[,1],new_data_lomb[,i], col=cols[i], ylim=c(0,5), type='l', main="2018 PM2.5 level - Lombardia", ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}
#daily average for each row
means_lomb = rowMeans(as.matrix(new_data_lomb[,-1]), na.rm = TRUE)
new_data_lomb <- data.frame(new_data_lomb, means_lomb)
mean_stations_lomb = colMeans(as.matrix(new_data_lomb[,-1]), na.rm = TRUE)


par(new=TRUE)
plot(new_data_lomb[,1],new_data_lomb[,34], col='black', ylim=c(0,5), lwd = '2', type='l', main="2018 PM2.5 level - Lombardia", ylab="PM2.5 level", xlab="Date", axes=FALSE)
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')


#PLOTS VENETO & PIEMONTE

#Veneto
x11(width=8)
quartz()
par(mfrow=c(2,1))
plot(new_data_ven[,1],new_data_ven[,2], col=cols[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Veneto", ylab="PM2.5 level", xlab="Date")

for (i in 3:21) {
  par(new=TRUE)
  plot(new_data_ven[,1],new_data_ven[,i], col=cols[i], ylim=c(0,100), type='l', main="2018 PM2.5 level - Veneto", ylab="PM2.5 level", xlab="Date",axes=FALSE)

}

#daily average for each row
means_ven= rowMeans(as.matrix(new_data_ven[,-1]), na.rm = TRUE)
new_data_ven <- data.frame(new_data_ven, means_ven)
mean_stations_ven = colMeans(as.matrix(new_data_ven[,-1]), na.rm = TRUE)


par(new=TRUE)
plot(new_data_ven[,1],new_data_ven[,22], col='black', ylim=c(0,100), lwd = '2', type='l', main="2018 PM2.5 level - Veneto", ylab="PM2.5 level", xlab="Date", axes=FALSE)
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

#Piemonte

plot(new_data_piem[,1],new_data_piem[,2], col=cols[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Piemonte", ylab="PM2.5 level", xlab="Date")

for (i in 3:25) {
  par(new=TRUE)
  plot(new_data_piem[,1],new_data_piem[,i], col=cols[i], ylim=c(0,100), type='l', main="2018 PM2.5 level - Piemonte", ylab="PM2.5 level", xlab="Date",axes=FALSE)

}

#daily average for each row
means_piem= rowMeans(as.matrix(new_data_piem[,-1]), na.rm = TRUE)
new_data_piem <- data.frame(new_data_piem, means_piem)
mean_stations_piem = colMeans(as.matrix(new_data_piem[,-1]), na.rm = TRUE)

par(new=TRUE)
plot(new_data_piem[,1],new_data_piem[,26], col='black', ylim=c(0,100), lwd = '2', type='l', main="2018 PM2.5 level - Piemonte", ylab="PM2.5 level", xlab="Date", axes=FALSE)
legend(x='topright',legend = c('mean'), col = c('black'), lwd='2')

#MEANS emil & lomb

x11(width=14)
quartz()
plot(new_data_emil[,1], new_data_emil[,32], col='red2', ylim=c(0,100), lwd = '2', type='l', main="mean 2018_PM2.5 level", ylab="PM2.5 level", xlab="Date")

par(new=TRUE)
plot(new_data_lomb[,1], new_data_lomb[,34], col='orange', ylim=c(0,100), lwd = '2', type='l', main="mean 2018_PM2.5 level", ylab="PM2.5 level", xlab="Date", axes=FALSE)

par(new=TRUE)
plot(new_data_ven[,1], new_data_ven[,22], col='turquoise', ylim=c(0,100), lwd = '2', type='l', main="mean 2018_PM2.5 level", ylab="PM2.5 level", xlab="Date", axes=FALSE)

par(new=TRUE)
plot(new_data_piem[,1], new_data_piem[,26], col='green', ylim=c(0,100), lwd = '2', type='l', main="mean 2018_PM2.5 level", ylab="PM2.5 level", xlab="Date", axes=FALSE)

legend(x='topright',legend = c('Emilia Romagna', 'Lombardia', 'Veneto', 'Piemonte'), col = c('red2', 'orange', 'turquoise', 'green'), lwd='2')



## Doing the histograms per month in 2018 EMILIA
emil <- read.csv("PM2.5Emilia.csv", header=TRUE, dec = ',', sep = ';')
emil_2018= emil[which(emil$Anno==2018),]

cols <- rainbow(12)
x11()
quartz()
par(mfrow=c(4,3))
vec_months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
for (i in 1:12){
  month=emil_2018[which(emil_2018$Mese==i),6]
  hist(month,breaks=20,freq=FALSE,col=cols[i],main=vec_months[i], xlim=c(0,100), ylim=c(0,0.12), xlab="Value")
}

# With logarithmic transformation
cols <- rainbow(12)
x11()
quartz()
par(mfrow=c(4,3))
vec_months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
for (i in 1:12){
  month=log(emil_2018[which(emil_2018$Mese==i),6])
  hist(month,breaks=20,freq=FALSE,col=cols[i],main=vec_months[i], xlim=c(0,5), ylim=c(0,2), xlab="Value")
}


## Doing the histograms per month in 2018 LOMBARDIA
lomb <- read.csv("PM2.5Lombardia_new.csv", header=TRUE, dec = ',', sep = ';')
lomb_2018 = lomb[which(lomb$Anno==2018),]

cols <- rainbow(12)
x11()
quartz()
par(mfrow=c(4,3))
vec_months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
for (i in 1:12){
  month=lomb_2018[which(lomb_2018$Mese==i),3]
  hist(month,breaks=20,freq=FALSE,col=cols[i],main=vec_months[i], xlim=c(0,100), ylim=c(0,0.1), xlab="Value")
}

# With logarithmic transoformation:
cols <- rainbow(12)
x11()
quartz()
par(mfrow=c(4,3))
vec_months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
for (i in 1:12){
  month=log(lomb_2018[which(lomb_2018$Mese==i),3])
  hist(month,breaks=20,freq=FALSE,col=cols[i],main=vec_months[i], xlim=c(0,5), ylim=c(0,1), xlab="Value")
}


######################### PIANURA PADANA MAPS ################################################################  

library("rnaturalearth")
library(ggplot2)
library(mapview)
library(dplyr)
library(maps)
library(rnaturalearthhires)
#remotes::install_github("ropensci/rnaturalearthhires")

# take stations coordinates

stazioni <- read.csv("Stations_Coord.csv", header=TRUE, dec = '.', sep = ',')
colnames(stazioni)[2] <- "Stazione"

# get italy provinces coordinates

it <- ggplot2::map_data("italy")
it <- ne_states(geounit = "italy", returnclass = "sf")


# plot all stations within Pianura Padana

pian_pad= it[which(it$region %in% c("Lombardia","Veneto","Emilia-Romagna","Piemonte")),]

mapview(stazioni, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations", 
        cex=3) + mapview(pian_pad,map.types="OpenStreetMap",
                         alpha.regions=0.3, layer.name= "Regions",
                         col.regions="cornflowerblue")

# options: alpha is the opacità of the lines, alpha.regions is 
# the opacità of the regions, cex is the size of the points, 
# layer.name to change legend

# 1. LOMBARDIA

lomb_reg= it[which(it$region== "Lombardia"),]
staz_lomb= stazioni[which(stazioni$Regione=="Lombardia"),]

#All stations (PM10 and PM2.5) (DON'T USE!)
mapview(staz_lomb, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations",
        cex=3) + mapview(lomb_reg,map.types="OpenStreetMap",alpha.regions=0.3, 
                         layer.name= "Lombardia",
                         col.regions="cornflowerblue")

#1.1 Plot of only PM2.5 stations

colnames(lomb)[14] <- "Stazione"
colnames(lomb)[24] <- "Long"
colnames(lomb)[23] <- "Lat"
lomb_merged = merge(lomb, stazioni, by="Stazione")

lomb_all= distinct(lomb_merged, Stazione, .keep_all = TRUE)
lomb_urb= lomb_all[which(lomb_all$AreaStazione== "U"),]
lomb_rur= lomb_all[which(lomb_all$AreaStazione== "R"),]
lomb_sub= lomb_all[which(lomb_all$AreaStazione== "S"),]


mapview(lomb_all, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations",
        cex=4) + mapview(lomb_reg,map.types="OpenStreetMap",alpha.regions=0.3, 
                         layer.name= "Lombardia",
                         col.regions="cornflowerblue")

#1.2 plot divided by zone (Urban-Suburban-Rural)

mapview(lomb_urb, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Urban stations", 
        cex=4) + mapview(lomb_rur, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
                         map.types="OpenStreetMap", alpha=1, color="springgreen4",
                         col.regions="springgreen4", layer.name= "Rural stations", 
                         cex=4) + mapview(lomb_sub, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
                                          map.types="OpenStreetMap", alpha=1, color="orange",
                                          col.regions="orange", layer.name= "Suburban stations", 
                                          cex=4) + mapview(lomb_reg,map.types="OpenStreetMap",
                                                           alpha.regions=0.3, 
                                                           layer.name= "Lombardia",
                                                           col.regions="cornflowerblue")

#1.3 plot divided by area (Fondo-Industriale_Traffico (B-I-T))  

lomb_fon= lomb_all[which(lomb_all$TipoStazione== "B"),]
lomb_ind= lomb_all[which(lomb_all$TipoStazione== "I"),]
lomb_traf= lomb_all[which(lomb_all$TipoStazione== "T"),]

mapview(lomb_fon, xcol="Long.x", ycol="Lat.x", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Background stations", 
        cex=4) + mapview(lomb_ind, xcol="Long.x", ycol="Lat.x", crs=4326, grid=FALSE, 
                         map.types="OpenStreetMap", alpha=1, color="springgreen4",
                         col.regions="springgreen4", layer.name= "Industrial stations", 
                         cex=4) + mapview(lomb_traf, xcol="Long.x", ycol="Lat.x", crs=4326, grid=FALSE, 
                                          map.types="OpenStreetMap", alpha=1, color="orange",
                                          col.regions="orange", layer.name= "Traffic stations", 
                                          cex=4) + mapview(lomb_reg,map.types="OpenStreetMap",
                                                           alpha.regions=0.3, 
                                                           layer.name= "Lombardia",
                                                           col.regions="cornflowerblue")
# 2. VENETO


ven_reg= it[which(it$region== "Veneto"),]
staz_ven= stazioni[which(stazioni$Regione=="Veneto"),]

#All stations (PM10 and PM2.5) (DON'T USE!)
mapview(staz_ven, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations", 
        cex=3) + mapview(ven_reg,map.types="OpenStreetMap",alpha.regions=0.3, 
                         layer.name= "Veneto",
                         col.regions="cornflowerblue")



#2.1 Plot of only PM2.5 stations

colnames(ven)[9] <- "Stazione"
colnames(ven)[12] <- "Long"
colnames(ven)[11] <- "Lat"
ven_merged = merge(ven, stazioni, by="Stazione")

ven_all= distinct(ven_merged, Stazione, .keep_all = TRUE)
ven_urb= ven_all[which(ven_all$Tipo.zona== "Urbano"),]
ven_rur= ven_all[which(ven_all$Tipo.zona== "Rurale"),]
ven_sub= ven_all[which(ven_all$Tipo.zona== "Suburbano"),]


mapview(ven_all, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations",
        cex=4) + mapview(ven_reg,map.types="OpenStreetMap",alpha.regions=0.3, 
                         layer.name= "Veneto",
                         col.regions="cornflowerblue")

#2.2 plot divided by zone (Urban-Suburban-Rural)

mapview(ven_urb, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Urban stations", 
        cex=4) + mapview(ven_rur, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
                         map.types="OpenStreetMap", alpha=1, color="springgreen4",
                         col.regions="springgreen4", layer.name= "Rural stations", 
                         cex=4) + mapview(ven_sub, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
                                          map.types="OpenStreetMap", alpha=1, color="orange",
                                          col.regions="orange", layer.name= "Suburban stations", 
                                          cex=4) + mapview(ven_reg,map.types="OpenStreetMap",
                                                           alpha.regions=0.3, 
                                                           layer.name= "Veneto",
                                                           col.regions="cornflowerblue")


#2.3 plot divided by area (Fondo-Industriale_Traffico)  

ven_fon= ven_all[which(ven_all$Classe.Stazione== "Fondo"),]
ven_ind= ven_all[which(ven_all$Classe.Stazione== "Industriale"),]
ven_traf= ven_all[which(ven_all$Classe.Stazione== "Traffico"),]

mapview(ven_fon, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Background stations", 
        cex=4) + mapview(ven_ind, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
                         map.types="OpenStreetMap", alpha=1, color="springgreen4",
                         col.regions="springgreen4", layer.name= "Industrial stations", 
                         cex=4) + mapview(ven_traf, xcol="Long.y", ycol="Lat.y", crs=4326, grid=FALSE, 
                                          map.types="OpenStreetMap", alpha=1, color="orange",
                                          col.regions="orange", layer.name= "Traffic stations", 
                                          cex=4) + mapview(ven_reg,map.types="OpenStreetMap",
                                                           alpha.regions=0.3, 
                                                           layer.name= "Veneto",
                                                           col.regions="cornflowerblue")
# 3. EMILIA-rOMAGNA

emil_reg= it[which(it$region== "Emilia-Romagna"),]
staz_em_rom= stazioni[which(stazioni$Regione=="Emilia-Romagna"),]

#All stations (PM10 and PM2.5) (DON'T USE!)
mapview(staz_em_rom, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations", 
        cex=3) + mapview(emil_reg,map.types="OpenStreetMap",alpha.regions=0.3, 
                         layer.name= "Emilia-Romagna",
                         col.regions="cornflowerblue")

#3.1 Plot of only PM2.5 stations
colnames(emil)[13] <- "Stazione"
emil_merged = merge(emil, stazioni, by="Stazione")


library(dplyr)

emil_all= distinct(emil_merged, COD_STAZ, .keep_all = TRUE)
emil_urb= emil_all[which(emil_all$TipoArea== "Urbano"),]
emil_rur= emil_all[which(emil_all$TipoArea== "Rurale"),]
emil_sub= emil_all[which(emil_all$TipoArea== "Suburbano"),]

mapview(emil_all, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations",
        cex=4) + mapview(emil_reg,map.types="OpenStreetMap",alpha.regions=0.3, 
                         layer.name= "Veneto",
                         col.regions="cornflowerblue")

#3.2 plot divided by zone (Urban-Suburban-Rural)
mapview(emil_urb, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Urban stations", 
        cex=4) + mapview(emil_rur, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
                         map.types="OpenStreetMap", alpha=1, color="springgreen4",
                         col.regions="springgreen4", layer.name= "Rural stations", 
                         cex=4) + mapview(emil_sub, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
                                          map.types="OpenStreetMap", alpha=1, color="orange",
                                          col.regions="orange", layer.name= "Suburban stations", 
                                          cex=4) + mapview(emil_reg,map.types="OpenStreetMap",
                                                           alpha.regions=0.3, 
                                                           layer.name= "Emilia-Romagna",
                                                           col.regions="cornflowerblue")

#3.3 plot divided by area (Fondo-Industriale_Traffico (B-I-T))  

emil_fon= emil_all[which(emil_all$TipoStazione== "Fondo"),]
emil_ind= emil_all[which(emil_all$TipoStazione== "Industriale"),]
emil_traf= emil_all[which(emil_all$TipoStazione== "Traffico"),]

mapview(emil_fon, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Background stations", 
        cex=4) + mapview(emil_ind, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
                         map.types="OpenStreetMap", alpha=1, color="springgreen4",
                         col.regions="springgreen4", layer.name= "Industrial stations", 
                         cex=4) + mapview(emil_traf, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
                                          map.types="OpenStreetMap", alpha=1, color="orange",
                                          col.regions="orange", layer.name= "Traffic stations", 
                                          cex=4) + mapview(emil_reg,map.types="OpenStreetMap",
                                                           alpha.regions=0.3, 
                                                           layer.name= "Emilia-Romagna",
                                                           col.regions="cornflowerblue")


#4. PIEMONTE

piem_reg= it[which(it$region== "Piemonte"),]
staz_piem= stazioni[which(stazioni$Regione=="Piemonte"),]

#All stations (PM10 and PM2.5) (DON'T USE!)
mapview(staz_piem, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations", 
        cex=3) + mapview(piem_reg,map.types="OpenStreetMap",alpha.regions=0.3, 
                         layer.name= "Piemonte",
                         col.regions="cornflowerblue")

#4.1 Plot of only PM2.5 stations

colnames(piem)[1] <- "Stazione"
piem_merged = merge(piem, stazioni, by="Stazione")

piem_all= distinct(piem_merged, Stazione, .keep_all = TRUE)
piem_urb= piem_all[which(piem_all$AreaStazione== "Urban"),]
piem_rur= piem_all[which(piem_all$AreaStazione== "Rural"),]
piem_sub= piem_all[which(piem_all$AreaStazione== "Suburban"),]

mapview(piem_all, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Stations",
        cex=4) + mapview(piem_reg,map.types="OpenStreetMap",alpha.regions=0.3, 
                         layer.name= "Piemonte",
                         col.regions="cornflowerblue")

#4.2 plot divided by zone (Urban-Suburban-Rural)

mapview(piem_urb, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Urban stations", 
        cex=4) + mapview(piem_rur, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
                         map.types="OpenStreetMap", alpha=1, color="springgreen4",
                         col.regions="springgreen4", layer.name= "Rural stations", 
                         cex=4) + mapview(piem_sub, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
                                          map.types="OpenStreetMap", alpha=1, color="orange",
                                          col.regions="orange", layer.name= "Suburban stations", 
                                          cex=4) + mapview(piem_reg,map.types="OpenStreetMap",
                                                           alpha.regions=0.3, 
                                                           layer.name= "Piemonte",
                                                           col.regions="cornflowerblue")
#4.3 plot divided by area (Fondo-Traffico (B-T))

piem_fon= piem_all[which(piem_all$TipoStazione== "Background"),]
piem_traf= piem_all[which(piem_all$TipoStazione== "Traffic"),]

mapview(piem_fon, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
        map.types="OpenStreetMap", alpha=1, color="red2",
        col.regions="red2", layer.name= "Background stations", 
        cex=4) + mapview(piem_traf, xcol="Long", ycol="Lat", crs=4326, grid=FALSE, 
                         map.types="OpenStreetMap", alpha=1, color="orange",
                         col.regions="orange", layer.name= "Traffic stations", 
                         cex=4) + mapview(piem_reg,map.types="OpenStreetMap",
                                          alpha.regions=0.3, 
                                          layer.name= "Piemonte",
                                          col.regions="cornflowerblue")

