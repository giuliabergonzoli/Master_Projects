# PLOT BY TYPE

# Emilia-Romagna 

emil <- read.csv("PM2.5Emilia.csv", header=TRUE, dec = ',', sep = ';')
emil_2018 = emil[which(emil$Anno==2018),c(5,2,6,16,19,20,21)]
colnames(emil_2018) = c("Data","NS","Valore","Provincia","Tipo","Area","Zonizzazione")

#Tipo
#fondo
fondo_emil <- emil_2018[which(emil_2018$Tipo=='Fondo'),]
staz_fondo_emil <- levels(as.factor(fondo_emil$NS))
#industriale
industriale_emil <- emil_2018[which(emil_2018$Tipo=='Industriale'),]
staz_industriale_emil <- levels(as.factor(industriale_emil$NS))
#traffico
traffico_emil <- emil_2018[which(emil_2018$Tipo=='Traffico'),]
staz_traffico_emil <- levels(as.factor(traffico_emil$NS))

#Fondo
stazioni_emil = staz_fondo_emil
Data = as.factor(levels(as.factor(emil_2018$Data)))

new_data_fond_emil = as.data.frame(Data)

for(i in 2:(length(stazioni_emil)+1)){
  stazione_i = emil_2018[which(emil_2018$NS==stazioni_emil[i-1]),c(1,3)]
  new_data_fond_emil = merge(new_data_fond_emil,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_fond_emil) = c("Date",as.character(stazioni_emil))

# #daily average for each row
# means_emil = rowMeans(as.matrix(new_data_fond_emil[,-1]), na.rm = TRUE)
# new_data_fond_emil <- data.frame(new_data_fond_emil, means_emil)
# 
# #mean_stations_emil = colMeans(as.matrix(new_data_fond_emil[,-1]), na.rm = TRUE)

#date giuste
count <- seq(1:364)
new_data_fond_emil <- cbind(new_data_fond_emil,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:363)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_fond_emil = merge(new_data_fond_emil,new_col, by = "count", all.x = TRUE)
new_data_fond_emil <- new_data_fond_emil[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_fond_emil = merge(new_data_fond_emil,new_col, by = "Data", all = TRUE)
new_data_fond_emil <- new_data_fond_emil[,-2]

# #replace the NAs
# for(j in 2:(length(stazioni_emil)+1)){
#   stazione_j = new_data_fond_emil[,j]
#   rows_na = which(is.na(stazione_j)==TRUE)
#   for (i in 1:length(rows_na)) {
#     if(rows_na[i] == 1){
#       stazione_j[rows_na[i]] <- mean_stations_emil[j-1]
#     }
#     else
#       stazione_j[rows_na[i]] <- stazione_j[rows_na[i]-1]
#   }
#   new_data_fond_emil[,j] = stazione_j
# }


#Industriale
stazioni_emil = staz_industriale_emil
Data = as.factor(levels(as.factor(emil_2018$Data)))

new_data_ind_emil = as.data.frame(Data)

for(i in 2:(length(stazioni_emil)+1)){
  stazione_i = emil_2018[which(emil_2018$NS==stazioni_emil[i-1]),c(1,3)]
  new_data_ind_emil = merge(new_data_ind_emil,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_ind_emil) = c("Date",as.character(stazioni_emil))

# #daily average for each row
# means = rowMeans(as.matrix(new_data_ind_emil[,-1]), na.rm = TRUE)
# new_data_ind_emil <- data.frame(new_data_ind_emil, means)
# 
# #mean_stations_emil = colMeans(as.matrix(new_data_ind_emil[,-1]), na.rm = TRUE)

#date giuste
count <- seq(1:364)
new_data_ind_emil <- cbind(new_data_ind_emil,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:363)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_ind_emil = merge(new_data_ind_emil,new_col, by = "count", all.x = TRUE)
new_data_ind_emil <- new_data_ind_emil[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_ind_emil = merge(new_data_ind_emil,new_col, by = "Data", all = TRUE)
new_data_ind_emil <- new_data_ind_emil[,-2]

#replace the NAs
# for(j in 2:(length(stazioni_emil)+1)){
#   stazione_j = new_data_ind_emil[,j]
#   rows_na = which(is.na(stazione_j)==TRUE)
#   for (i in 1:length(rows_na)) {
#     if(rows_na[i] == 1){
#       stazione_j[rows_na[i]] <- mean_stations_emil[j-1]
#     }
#     else
#       stazione_j[rows_na[i]] <- stazione_j[rows_na[i]-1]
#   }
#   new_data_ind_emil[,j] = stazione_j
# }


#Traffico
stazioni_emil = staz_traffico_emil
Data = as.factor(levels(as.factor(emil_2018$Data)))

new_data_traff_emil = as.data.frame(Data)

for(i in 2:(length(stazioni_emil)+1)){
  stazione_i = emil_2018[which(emil_2018$NS==stazioni_emil[i-1]),c(1,3)]
  new_data_traff_emil = merge(new_data_traff_emil,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_traff_emil) = c("Date",as.character(stazioni_emil))

# #daily average for each row
# means = rowMeans(as.matrix(new_data_traff_emil[,-1]), na.rm = TRUE)
# new_data_traff_emil <- data.frame(new_data_traff_emil, means)
# 
# #mean_stations_emil = colMeans(as.matrix(new_data_traff_emil[,-1]), na.rm = TRUE)

#date giuste
count <- seq(1:364)
new_data_traff_emil <- cbind(new_data_traff_emil,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:363)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_traff_emil = merge(new_data_traff_emil,new_col, by = "count", all.x = TRUE)
new_data_traff_emil <- new_data_traff_emil[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_traff_emil = merge(new_data_traff_emil,new_col, by = "Data", all = TRUE)
new_data_traff_emil <- new_data_traff_emil[,-2]

#replace the NAs
# for(j in 2:(length(stazioni_emil)+1)){
#   stazione_j = new_data_traff_emil[,j]
#   rows_na = which(is.na(stazione_j)==TRUE)
#   for (i in 1:length(rows_na)) {
#     if(rows_na[i] == 1){
#       stazione_j[rows_na[i]] <- mean_stations_emil[j-1]
#     }
#     else
#       stazione_j[rows_na[i]] <- stazione_j[rows_na[i]-1]
#   }
#   new_data_traff_emil[,j] = stazione_j
# }


# Lombardia

lomb <- read.csv("PM2.5Lombardia_new.csv", header=TRUE, dec = ',', sep = ';')
lomb_2018 = lomb[which(lomb$Anno==2018),c(2,13,3,16,27,26,25)]
colnames(lomb_2018) = c("Data","NS","Valore","Provincia","Tipo","Area","Zonizzazione")

#Tipo
#fondo
fondo_lomb <- lomb_2018[which(lomb_2018$Tipo=='B'),]
staz_fondo_lomb <- levels(as.factor(fondo_lomb$NS))
#industriale
industriale_lomb <- lomb_2018[which(lomb_2018$Tipo=='I'),]
staz_industriale_lomb <- levels(as.factor(industriale_lomb$NS))
#traffico
traffico_lomb <- lomb_2018[which(lomb_2018$Tipo=='T'),]
staz_traffico_lomb <- levels(as.factor(traffico_lomb$NS))

#Fondo
stazioni_lomb = staz_fondo_lomb
Data = as.factor(levels(as.factor(lomb_2018$Data)))

new_data_fond_lomb = as.data.frame(Data)

for(i in 2:(length(stazioni_lomb)+1)){
  stazione_i = lomb_2018[which(lomb_2018$NS==stazioni_lomb[i-1]),c(1,3)]
  new_data_fond_lomb = merge(new_data_fond_lomb,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_fond_lomb) = c("Date",as.character(stazioni_lomb))

# #daily average for each row
# means = rowMeans(as.matrix(new_data_fond_lomb[,-1]), na.rm = TRUE)
# new_data_fond_lomb <- data.frame(new_data_fond_lomb, means)
# 
# #mean_stations_lomb = colMeans(as.matrix(new_data_fond_lomb[,-1]), na.rm = TRUE)

#date giuste
count <- seq(1:365)
new_data_fond_lomb <- cbind(new_data_fond_lomb,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_fond_lomb = merge(new_data_fond_lomb,new_col, by = "count", all.x = TRUE)
new_data_fond_lomb <- new_data_fond_lomb[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_fond_lomb = merge(new_data_fond_lomb,new_col, by = "Data", all = TRUE)
new_data_fond_lomb <- new_data_fond_lomb[,-2]

# #replace the NAs
# for(j in 2:(length(stazioni_lomb)+1)){
#   stazione_j = new_data_fond_lomb[,j]
#   rows_na = which(is.na(stazione_j)==TRUE)
#   for (i in 1:length(rows_na)) {
#     if(rows_na[i] == 1){
#       stazione_j[rows_na[i]] <- mean_stations_lomb[j-1]
#     }
#     else
#       stazione_j[rows_na[i]] <- stazione_j[rows_na[i]-1]
#   }
#   new_data_fond_lomb[,j] = stazione_j
# }


#Industriale
stazioni_lomb = staz_industriale_lomb
Data = as.factor(levels(as.factor(lomb_2018$Data)))

new_data_ind_lomb = as.data.frame(Data)

for(i in 2:(length(stazioni_lomb)+1)){
  stazione_i = lomb_2018[which(lomb_2018$NS==stazioni_lomb[i-1]),c(1,3)]
  new_data_ind_lomb = merge(new_data_ind_lomb,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_ind_lomb) = c("Date",as.character(stazioni_lomb))

# #daily average for each row
# means = rowMeans(as.matrix(new_data_ind_lomb[,-1]), na.rm = TRUE)
# new_data_ind_lomb <- data.frame(new_data_ind_lomb, means)
# 
# #mean_stations_lomb = colMeans(as.matrix(new_data_ind_lomb[,-1]), na.rm = TRUE)

#date giuste
count <- seq(1:365)
new_data_ind_lomb <- cbind(new_data_ind_lomb,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_ind_lomb = merge(new_data_ind_lomb,new_col, by = "count", all.x = TRUE)
new_data_ind_lomb <- new_data_ind_lomb[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_ind_lomb = merge(new_data_ind_lomb,new_col, by = "Data", all = TRUE)
new_data_ind_lomb <- new_data_ind_lomb[,-2]

#replace the NAs
# for(j in 2:(length(stazioni_lomb)+1)){
#   stazione_j = new_data_ind_lomb[,j]
#   rows_na = which(is.na(stazione_j)==TRUE)
#   for (i in 1:length(rows_na)) {
#     if(rows_na[i] == 1){
#       stazione_j[rows_na[i]] <- mean_stations_lomb[j-1]
#     }
#     else
#       stazione_j[rows_na[i]] <- stazione_j[rows_na[i]-1]
#   }
#   new_data_ind_lomb[,j] = stazione_j
# }


#Traffico
stazioni_lomb = staz_traffico_lomb
Data = as.factor(levels(as.factor(lomb_2018$Data)))

new_data_traff_lomb = as.data.frame(Data)

for(i in 2:(length(stazioni_lomb)+1)){
  stazione_i = lomb_2018[which(lomb_2018$NS==stazioni_lomb[i-1]),c(1,3)]
  new_data_traff_lomb = merge(new_data_traff_lomb,stazione_i, by = "Data", all.x = TRUE)
}

colnames(new_data_traff_lomb) = c("Date",as.character(stazioni_lomb))

# #daily average for each row
# means = rowMeans(as.matrix(new_data_traff_lomb[,-1]), na.rm = TRUE)
# new_data_traff_lomb <- data.frame(new_data_traff_lomb, means)
# 
# #mean_stations_lomb = colMeans(as.matrix(new_data_traff_lomb[,-1]), na.rm = TRUE)

#date giuste
count <- seq(1:365)
new_data_traff_lomb <- cbind(new_data_traff_lomb,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_traff_lomb = merge(new_data_traff_lomb,new_col, by = "count", all.x = TRUE)
new_data_traff_lomb <- new_data_traff_lomb[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_traff_lomb = merge(new_data_traff_lomb,new_col, by = "Data", all = TRUE)
new_data_traff_lomb <- new_data_traff_lomb[,-2]

#replace the NAs
# for(j in 2:(length(stazioni_lomb)+1)){
#   stazione_j = new_data_traff_lomb[,j]
#   rows_na = which(is.na(stazione_j)==TRUE)
#   for (i in 1:length(rows_na)) {
#     if(rows_na[i] == 1){
#       stazione_j[rows_na[i]] <- mean_stations_lomb[j-1]
#     }
#     else
#       stazione_j[rows_na[i]] <- stazione_j[rows_na[i]-1]
#   }
#   new_data_traff_lomb[,j] = stazione_j
# }

##############

# Plot di Emilia-Romagna e Lombardia (unico grafico)

# New datasets that include both Emilia-Romagna and Lombardia 
new_data_fond_lomb_bis <- new_data_fond_lomb[,-1]
fond_tot <- cbind(new_data_fond_emil,new_data_fond_lomb_bis)
new_data_ind_lomb_bis <- new_data_ind_lomb[,-1]
industrial_tot <- cbind(new_data_ind_emil,new_data_ind_lomb_bis)
new_data_traff_lomb_bis <- new_data_traff_lomb[,-1]
traffic_tot <- cbind(new_data_traff_emil,new_data_traff_lomb_bis)

# Adding the daily means for each row
means_fond = rowMeans(as.matrix(fond_tot[,-1]), na.rm = TRUE)
fond_tot <- data.frame(fond_tot, means_fond)
# mean_stations_fond_tot = colMeans(as.matrix(fond_tot[,-1]), na.rm = TRUE)
means_ind = rowMeans(as.matrix(industrial_tot[,-1]), na.rm = TRUE)
industrial_tot <- data.frame(industrial_tot, means_ind)
# mean_stations_ind_tot = colMeans(as.matrix(industrial_tot[,-1]), na.rm = TRUE)
means_traff = rowMeans(as.matrix(traffic_tot[,-1]), na.rm = TRUE)
traffic_tot <- data.frame(traffic_tot, means_traff)
# mean_stations_traff_tot = colMeans(as.matrix(traffic_tot[,-1]), na.rm = TRUE)

cols <- rainbow(length(fond_tot)-2+ length(industrial_tot)-2 + length(traffic_tot)-2)
cols1 <- cols[1:(length(fond_tot)-2)]
cols2 <- cols[length(fond_tot)-2+1:(length(industrial_tot)-2)]
cols3 <- cols[length(industrial_tot)-2+1:(length(traffic_tot)-2)]

tot_mean_fond <- colMeans(as.matrix(fond_tot[,45]), na.rm = TRUE)
tot_mean_ind <- colMeans(as.matrix(industrial_tot[,9]), na.rm = TRUE)
tot_mean_traff <- colMeans(as.matrix(traffic_tot[,14]), na.rm = TRUE)

x11(width=14)
par(mfrow=c(1,3))
plot(fond_tot[,1],fond_tot[,2], col=cols1[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Background", ylab="PM2.5 level", xlab="Date")

for (i in 3:44) {
  par(new=TRUE)
  plot(fond_tot[,1],fond_tot[,i], col=cols1[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}

par(new=TRUE)
plot(fond_tot[,1],fond_tot[,45], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
abline(h=tot_mean_fond, col="black", lwd=2, lty=2)

plot(industrial_tot[,1],industrial_tot[,2], col=cols2[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Industrial", ylab="PM2.5 level", xlab="Date")

for (i in 3:8) {
  par(new=TRUE)
  plot(industrial_tot[,1],industrial_tot[,i], col=cols2[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}

par(new=TRUE)
plot(industrial_tot[,1],industrial_tot[,9], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
abline(h=tot_mean_ind, col="black", lwd=2, lty=2)

plot(traffic_tot[,1],traffic_tot[,2], col=cols3[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Traffic", ylab="PM2.5 level", xlab="Date")

for (i in 3:13) {
  par(new=TRUE)
  plot(traffic_tot[,1],traffic_tot[,i], col=cols3[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
  
}

par(new=TRUE)
plot(traffic_tot[,1],traffic_tot[,14], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
abline(h=tot_mean_traff, col="black", lwd=2, lty=2)



##############

# # Plot di Emilia-Romagna e Lombardia (separati)
# 
# cols <- rainbow(length(staz_fondo_emil) + length(staz_industriale_emil) + length(staz_traffico_emil) + length(staz_fondo_lomb) + length(staz_industriale_lomb) + length(staz_traffico_lomb))
# cols1 <- cols[1:length(staz_fondo_emil)]
# cols2 <- cols[length(staz_fondo_emil)+1:length(staz_industriale_emil)]
# cols3 <- cols[length(staz_industriale_emil)+1:length(staz_traffico_emil)]
# cols4 <- cols[length(staz_traffico_emil)+1:length(staz_fondo_lomb)]
# cols5 <- cols[length(staz_fondo_lomb)+1:length(staz_industriale_lomb)]
# cols6 <- cols[length(staz_industriale_lomb)+1:length(staz_traffico_lomb)]
# 
# # # I dataset new_data_... devono comprendere le medie giornaliere per ogni riga 
# # # => scommentare le righe 31,84,137,207,260,313 prima di runnare il resto
# tot_mean_fond_emil <- colMeans(as.matrix(new_data_fond_emil[,26]), na.rm = TRUE)
# tot_mean_ind_emil <- colMeans(as.matrix(new_data_ind_emil[,7]), na.rm = TRUE)
# tot_mean_traff_emil <- colMeans(as.matrix(new_data_traff_emil[,3]), na.rm = TRUE)
# tot_mean_fond_lomb <- colMeans(as.matrix(new_data_fond_lomb[,21]), na.rm = TRUE)
# tot_mean_ind_lomb <- colMeans(as.matrix(new_data_ind_lomb[,4]), na.rm = TRUE)
# tot_mean_traff_lomb <- colMeans(as.matrix(new_data_traff_lomb[,13]), na.rm = TRUE)
# 
# # Emilia-Romagna 
# 
# x11(width=14)
# par(mfrow=c(1,3))
# plot(new_data_fond_emil[,1],new_data_fond_emil[,2], col=cols1[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Background (Emilia-Romagna)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:25) {
#   par(new=TRUE)
#   plot(new_data_fond_emil[,1],new_data_fond_emil[,i], col=cols1[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
# }
# 
# par(new=TRUE)
# plot(new_data_fond_emil[,1],new_data_fond_emil[,26], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_fond_emil, col="black", lwd=2, lty=2)
# 
# plot(new_data_ind_emil[,1],new_data_ind_emil[,2], col=cols2[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Industrial (Emilia-Romagna)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:6) {
#   par(new=TRUE)
#   plot(new_data_ind_emil[,1],new_data_ind_emil[,i], col=cols2[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
#   
# }
# 
# par(new=TRUE)
# plot(new_data_ind_emil[,1],new_data_ind_emil[,7], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_ind_emil, col="black", lwd=2, lty=2)
# 
# plot(new_data_traff_emil[,1],new_data_traff_emil[,2], col=cols3[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Traffic (Emilia-Romagna)", ylab="PM2.5 level", xlab="Date")
# abline(h=tot_mean_traff_emil, col="black", lwd=2, lty=2)
# 
# 
# # Lombardia 
# 
# x11(width=14)
# par(mfrow=c(1,3))
# plot(new_data_fond_lomb[,1],new_data_fond_lomb[,2], col=cols4[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Background (Lombardia)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:20) {
#   par(new=TRUE)
#   plot(new_data_fond_lomb[,1],new_data_fond_lomb[,i], col=cols4[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
# }
# 
# par(new=TRUE)
# plot(new_data_fond_lomb[,1],new_data_fond_lomb[,21], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_fond_lomb, col="black", lwd=2, lty=2)
# 
# plot(new_data_ind_lomb[,1],new_data_ind_lomb[,2], col=cols5[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Industrial (Lombardia)", ylab="PM2.5 level", xlab="Date")
# par(new=TRUE)
# 
# plot(new_data_ind_lomb[,1],new_data_ind_lomb[,3], col=cols5[3], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
# 
# par(new=TRUE)
# plot(new_data_ind_lomb[,1],new_data_ind_lomb[,4], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_ind_lomb, col="black", lwd=2, lty=2)
# 
# plot(new_data_traff_lomb[,1],new_data_traff_lomb[,2], col=cols5[1], ylim=c(0,100), type='l', main="2018 PM2.5 level - Traffic (Lombardia)", ylab="PM2.5 level", xlab="Date")
# 
# for (i in 3:12) {
#   par(new=TRUE)
#   plot(new_data_traff_lomb[,1],new_data_traff_lomb[,i], col=cols5[i], ylim=c(0,100), type='l', ylab="PM2.5 level", xlab="Date",axes=FALSE)
#   
# }
# 
# par(new=TRUE)
# plot(new_data_traff_lomb[,1],new_data_traff_lomb[,13], col='black', ylim=c(0,100), lwd = '1.5', type='l', ylab="PM2.5 level", xlab="Date", axes=FALSE)
# abline(h=tot_mean_traff_lomb, col="black", lwd=2, lty=2)



