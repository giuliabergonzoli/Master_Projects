# PLOT BY AREA

# Emilia-Romagna 

emil <- read.csv("PM2.5Emilia.csv", header=TRUE, dec = ',', sep = ';')
emil_2018 = emil[which(emil$Anno==2018),c(5,2,6,16,19,20,21)]
colnames(emil_2018) = c("Data","NS","Valore","Provincia","Tipo","Area","Zonizzazione")

#Area
#rurale
rurale_emil <- emil_2018[which(emil_2018$Area=='Rurale'),]
staz_rurale_emil <- levels(as.factor(rurale_emil$NS))
#suburbano
suburbano_emil <- emil_2018[which(emil_2018$Area=='Suburbano'),]
staz_suburbano_emil <- levels(as.factor(suburbano_emil$NS))
#urbano
urbano_emil <- emil_2018[which(emil_2018$Area=='Urbano'),]
staz_urbano_emil <- levels(as.factor(urbano_emil$NS))

#rurale
stazioni_emil = staz_rurale_emil
Data = as.factor(levels(as.factor(emil_2018$Data)))

new_data_rur_emil = as.data.frame(Data)

for(i in 2:(length(stazioni_emil)+1)){
  stazione_i = emil_2018[which(emil_2018$NS==stazioni_emil[i-1]),c(1,3)]
  new_data_rur_emil = merge(new_data_rur_emil,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_rur_emil) = c("Date",as.character(stazioni_emil))

#fix dates
count <- seq(1:364)
new_data_rur_emil <- cbind(new_data_rur_emil,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:363)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_rur_emil = merge(new_data_rur_emil,new_col, by = "count", all.x = TRUE)
new_data_rur_emil <- new_data_rur_emil[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_rur_emil = merge(new_data_rur_emil,new_col, by = "Data", all = TRUE)
new_data_rur_emil <- new_data_rur_emil[,-2]

#suburbano
stazioni_emil = staz_suburbano_emil
Data = as.factor(levels(as.factor(emil_2018$Data)))

new_data_suburb_emil = as.data.frame(Data)

for(i in 2:(length(stazioni_emil)+1)){
  stazione_i = emil_2018[which(emil_2018$NS==stazioni_emil[i-1]),c(1,3)]
  new_data_suburb_emil = merge(new_data_suburb_emil,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_suburb_emil) = c("Date",as.character(stazioni_emil))

#fix dates
count <- seq(1:364)
new_data_suburb_emil <- cbind(new_data_suburb_emil,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:363)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_suburb_emil = merge(new_data_suburb_emil,new_col, by = "count", all.x = TRUE)
new_data_suburb_emil <- new_data_suburb_emil[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_suburb_emil = merge(new_data_suburb_emil,new_col, by = "Data", all = TRUE)
new_data_suburb_emil <- new_data_suburb_emil[,-2]

#urbano
stazioni_emil = staz_urbano_emil
Data = as.factor(levels(as.factor(emil_2018$Data)))

new_data_urb_emil = as.data.frame(Data)

for(i in 2:(length(stazioni_emil)+1)){
  stazione_i = emil_2018[which(emil_2018$NS==stazioni_emil[i-1]),c(1,3)]
  new_data_urb_emil = merge(new_data_urb_emil,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_urb_emil) = c("Date",as.character(stazioni_emil))

#fix dates
count <- seq(1:364)
new_data_urb_emil <- cbind(new_data_urb_emil,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:363)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_urb_emil = merge(new_data_urb_emil,new_col, by = "count", all.x = TRUE)
new_data_urb_emil <- new_data_urb_emil[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_urb_emil = merge(new_data_urb_emil,new_col, by = "Data", all = TRUE)
new_data_urb_emil <- new_data_urb_emil[,-2]

# Lombardia
lomb <- read.csv("PM2.5Lombardia_new.csv", header=TRUE, dec = ',', sep = ';')
lomb_2018 = lomb[which(lomb$Anno==2018),c(2,13,3,16,27,26,25)]
colnames(lomb_2018) = c("Data","NS","Valore","Provincia","Tipo","Area","Zonizzazione")

#Area
#rurale_lomb
rurale_lomb <- lomb_2018[which(lomb_2018$Area=='R'),]
staz_rurale_lomb <- levels(as.factor(rurale_lomb$NS))
#suburbano_lomb
suburbano_lomb <- lomb_2018[which(lomb_2018$Area=='S'),]
staz_suburbano_lomb <- levels(as.factor(suburbano_lomb$NS))
#urbano_lomb
urbano_lomb <- lomb_2018[which(lomb_2018$Area=='U'),]
staz_urbano_lomb <- levels(as.factor(urbano_lomb$NS))

#rurale_lomb
stazioni_lomb = staz_rurale_lomb
Data = as.factor(levels(as.factor(lomb_2018$Data)))

new_data_rur_lomb = as.data.frame(Data)

for(i in 2:(length(stazioni_lomb)+1)){
  stazione_i = lomb_2018[which(lomb_2018$NS==stazioni_lomb[i-1]),c(1,3)]
  new_data_rur_lomb = merge(new_data_rur_lomb,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_rur_lomb) = c("Date",as.character(stazioni_lomb))

#date giuste
count <- seq(1:365)
new_data_rur_lomb <- cbind(new_data_rur_lomb,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_rur_lomb = merge(new_data_rur_lomb,new_col, by = "count", all.x = TRUE)
new_data_rur_lomb <- new_data_rur_lomb[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_rur_lomb = merge(new_data_rur_lomb,new_col, by = "Data", all = TRUE)
new_data_rur_lomb <- new_data_rur_lomb[,-2]

#suburbano
stazioni_lomb = staz_suburbano_lomb
Data = as.factor(levels(as.factor(lomb_2018$Data)))

new_data_suburb_lomb = as.data.frame(Data)

for(i in 2:(length(stazioni_lomb)+1)){
  stazione_i = lomb_2018[which(lomb_2018$NS==stazioni_lomb[i-1]),c(1,3)]
  new_data_suburb_lomb = merge(new_data_suburb_lomb,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_suburb_lomb) = c("Date",as.character(stazioni_lomb))

#fix dates
count <- seq(1:365)
new_data_suburb_lomb <- cbind(new_data_suburb_lomb,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_suburb_lomb = merge(new_data_suburb_lomb,new_col, by = "count", all.x = TRUE)
new_data_suburb_lomb <- new_data_suburb_lomb[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_suburb_lomb = merge(new_data_suburb_lomb,new_col, by = "Data", all = TRUE)
new_data_suburb_lomb <- new_data_suburb_lomb[,-2]

#urbano
stazioni_lomb = staz_urbano_lomb
Data = as.factor(levels(as.factor(lomb_2018$Data)))

new_data_urb_lomb = as.data.frame(Data)

for(i in 2:(length(stazioni_lomb)+1)){
  stazione_i = lomb_2018[which(lomb_2018$NS==stazioni_lomb[i-1]),c(1,3)]
  new_data_urb_lomb = merge(new_data_urb_lomb,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_urb_lomb) = c("Date",as.character(stazioni_lomb))

#fix dates
count <- seq(1:365)
new_data_urb_lomb <- cbind(new_data_urb_lomb,count)
colnames(new_data_urb_lomb)[1]=c("Data")

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_urb_lomb = merge(new_data_urb_lomb,new_col, by = "count", all.x = TRUE)
new_data_urb_lomb <- new_data_urb_lomb[,-1]
colnames(new_data_urb_lomb)[24] = c("Data")
colnames(new_data_urb_lomb)[1] = c("Date")

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_urb_lomb = merge(new_data_urb_lomb,new_col, by = "Data", all = TRUE)
new_data_urb_lomb <- new_data_urb_lomb[,-2]

##################

# Plot di Emilia-Romagna e Lombardia (unico grafico)

# New datasets that include both Emilia-Romagna and Lombardia 
new_data_rur_lomb_bis <- new_data_rur_lomb[,-1]
rural_tot <- cbind(new_data_rur_emil,new_data_rur_lomb_bis)
new_data_suburb_lomb_bis <- new_data_suburb_lomb[,-1]
suburban_tot <- cbind(new_data_suburb_emil,new_data_suburb_lomb_bis)
new_data_urb_lomb_bis <- new_data_urb_lomb[,-1]
urban_tot <- cbind(new_data_urb_emil,new_data_urb_lomb_bis)

# Adding the daily means for each row
means_rur = rowMeans(as.matrix(rural_tot[,-1]), na.rm = TRUE)
rural_tot <- data.frame(rural_tot, means_rur)
# mean_stations_rur_tot = colMeans(as.matrix(rural_tot[,-1]), na.rm = TRUE)
means_suburb = rowMeans(as.matrix(suburban_tot[,-1]), na.rm = TRUE)
suburban_tot <- data.frame(suburban_tot, means_suburb)
# mean_stations_suburb_tot = colMeans(as.matrix(suburban_tot[,-1]), na.rm = TRUE)
means_urb = rowMeans(as.matrix(urban_tot[,-1]), na.rm = TRUE)
urban_tot <- data.frame(urban_tot, means_urb)
# mean_stations_urb_tot = colMeans(as.matrix(urban_tot[,-1]), na.rm = TRUE)

cols <- rainbow(length(rural_tot) + length(suburban_tot) + length(urban_tot))
cols1 <- cols[1:length(rural_tot)]
cols2 <- cols[length(rural_tot)+1:length(suburban_tot)]
cols3 <- cols[length(suburban_tot)+1:length(urban_tot)]

tot_mean_rur <- colMeans(as.matrix(rural_tot[,18]), na.rm = TRUE)
tot_mean_suburb <- colMeans(as.matrix(suburban_tot[,14]), na.rm = TRUE)
tot_mean_urb <- colMeans(as.matrix(urban_tot[,36]), na.rm = TRUE)

x11(width=14)
quartz()
par(mfrow=c(1,3))
plot(rural_tot[,1],rural_tot[,2], col=cols1[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Rural", ylab="PM2.5 level", xlab="Date")

for (i in 3:17) {
  par(new=TRUE)
  plot(rural_tot[,1],rural_tot[,i], col=cols1[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}

par(new=TRUE)
plot(rural_tot[,1],rural_tot[,18], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
abline(h=tot_mean_rur, col="black", lwd=2, lty=2)

plot(suburban_tot[,1],suburban_tot[,2], col=cols2[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Suburban", ylab="PM2.5 level", xlab="Date")

for (i in 3:13) {
  par(new=TRUE)
  plot(suburban_tot[,1],suburban_tot[,i], col=cols2[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}

par(new=TRUE)
plot(suburban_tot[,1],suburban_tot[,14], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
abline(h=tot_mean_suburb, col="black", lwd=2, lty=2)

plot(urban_tot[,1],urban_tot[,2], col=cols3[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Urban", ylab="PM2.5 level", xlab="Date")

for (i in 3:35) {
  par(new=TRUE)
  plot(urban_tot[,1],urban_tot[,i], col=cols3[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}

par(new=TRUE)
plot(urban_tot[,1],urban_tot[,36], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
abline(h=tot_mean_urb, col="black", lwd=2, lty=2)


##############

# # Plot di Emilia-Romagna e Lombardia (separati)

# cols <- rainbow(length(staz_rurale_emil) + length(staz_suburbano_emil) + length(staz_urbano_emil) + length(sta_rurale_lomb) + length(staz_suburbano_lomb) + length(staz_urbano_lomb))
# cols1 <- cols[1:length(staz_rurale)]
# cols2 <- cols[length(staz_rurale)+1:length(staz_suburbano)]
# cols3 <- cols[length(staz_suburbano)+1:length(staz_urbano)]
# cols4 <- cols[length(staz_urbano)+1:length(staz_rurale_lomb)]
# cols5 <- cols[length(staz_rurale_lomb)+1:length(staz_suburbano_lomb)]
# cols6 <- cols[length(staz_suburbano_lomb)+1:length(staz_urbano_lomb)]
# 
# # I dataset new_data_... devono comprendere le medie giornaliere per ogni riga 
# # => scommentare le righe 31,84,137,207,260,313 prima di runnare il resto
# tot_mean_rur_emil <- colMeans(as.matrix(new_data_rur_emil[,13]), na.rm = TRUE)
# tot_mean_suburb_emil <- colMeans(as.matrix(new_data_suburb_emil[,9]), na.rm = TRUE)
# tot_mean_urb_emil <- colMeans(as.matrix(new_data_urb_emil[,14]), na.rm = TRUE)
# tot_mean_rur_lomb <- colMeans(as.matrix(new_data_rur_lomb[,7]), na.rm = TRUE)
# tot_mean_sub_lomb <- colMeans(as.matrix(new_data_suburb_lomb[,7]), na.rm = TRUE)
# tot_mean_urb_lomb <- colMeans(as.matrix(new_data_urb_lomb[,24]), na.rm = TRUE)
# 
# # Emilia Romagna
# 
# x11(width=14)
# par(mfrow=c(1,3))
# plot(new_data_rur_emil[,1],new_data_rur_emil[,2], col=cols1[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Rural (Emilia-Romagna)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:12) {
#   par(new=TRUE)
#   plot(new_data_rur_emil[,1],new_data_rur_emil[,i], col=cols1[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
# }
# 
# par(new=TRUE)
# plot(new_data_rur_emil[,1],new_data_rur_emil[,13], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_rur_emil, col="black", lwd=2, lty=2)
# 
# plot(new_data_suburb_emil[,1],new_data_suburb_emil[,2], col=cols2[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Suburban (Emilia-Romagna)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:8) {
#   par(new=TRUE)
#   plot(new_data_suburb_emil[,1],new_data_suburb_emil[,i], col=cols2[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
#   
# }
# 
# par(new=TRUE)
# plot(new_data_suburb_emil[,1],new_data_suburb_emil[,9], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_suburb_emil, col="black", lwd=2, lty=2)
# 
# plot(new_data_urb_emil[,1],new_data_urb_emil[,2], col=cols3[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Urban (Emilia-Romagna)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:13) {
#   par(new=TRUE)
#   plot(new_data_urb_emil[,1],new_data_urb_emil[,i], col=cols3[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
#   
# }
# 
# par(new=TRUE)
# plot(new_data_urb_emil[,1],new_data_urb_emil[,14], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_urb_emil, col="black", lwd=2, lty=2)
# 
# # Lombardia 
# 
# x11(width=14)
# par(mfrow=c(1,3))
# plot(new_data_rur_lomb[,1],new_data_rur_lomb[,2], col=cols4[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Rural (Lombardia)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:6) {
#   par(new=TRUE)
#   plot(new_data_rur_lomb[,1],new_data_rur_lomb[,i], col=cols4[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
#   
# }
# 
# par(new=TRUE)
# plot(new_data_rur_lomb[,1],new_data_rur_lomb[,7], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_rur_lomb, col="black", lwd=2, lty=2)
# 
# plot(new_data_suburb_lomb[,1],new_data_suburb_lomb[,2], col=cols5[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Suburban (Lombardia)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:6) {
#   par(new=TRUE)
#   plot(new_data_suburb_lomb[,1],new_data_suburb_lomb[,i], col=cols5[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
#   
# }
# 
# par(new=TRUE)
# plot(new_data_suburb_lomb[,1],new_data_suburb_lomb[,7], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_sub_lomb, col="black", lwd=2, lty=2)
# 
# plot(new_data_urb_lomb[,1],new_data_urb_lomb[,2], col=cols6[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Urban (Lombardia)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:23) {
#   par(new=TRUE)
#   plot(new_data_urb_lomb[,1],new_data_urb_lomb[,i], col=cols6[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
#   
# }
# 
# par(new=TRUE)
# plot(new_data_urb_lomb[,1],new_data_urb_lomb[,24], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_urb_lomb, col="black", lwd=2, lty=2)

