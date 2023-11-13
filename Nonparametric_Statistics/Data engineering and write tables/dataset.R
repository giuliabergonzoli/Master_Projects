#loading necessary libraries
library(stringr)

################################################################################
##################################### 2015 #####################################
################################################################################

####################
########ARIA########
####################

aria_2015 <- read.csv("sensori_aria_2015.csv", header=TRUE, dec = '.', sep = ',')
aria_2015[c('Date', 'Time')] <- str_split_fixed(aria_2015$Data, ' ', 2)
aria_2015 <- aria_2015[,-c(2,4,5,7)]

staz_aria_2015 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2015 <- staz_aria_2015[,-c(9,10,11,16)]
colnames(staz_aria_2015)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2015 <- aggregate(Valore ~ IdSensore + Date, aria_2015, mean)
t <- table(dati_giornalieri_2015$IdSensore)
mydata_aria_2015 <- merge(dati_giornalieri_2015, staz_aria_2015, by="IdSensore")

write.table(mydata_aria_2015, "aria_giornaliero_2015.csv", sep = ";", dec = ",", row.names=FALSE)

####################
#######METEO########
####################

meteo_2015 <- read.csv("sensori_meteo_2015.csv", header=TRUE, dec = '.', sep = ',')
meteo_2015[c('Date', 'Time')] <- str_split_fixed(meteo_2015$Data, ' ', 2)
meteo_2015 <- meteo_2015[,-c(2,4,5,7)]

staz_meteo_2015 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2015 <- staz_meteo_2015[,-c(8,9,10,15)]
colnames(staz_meteo_2015)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2015 <- aggregate(Valore ~ IdSensore + Date, meteo_2015, mean)
t <- table(dati_giornalieri_2015$IdSensore)
mydata_meteo_2015 <- merge(dati_giornalieri_2015, staz_meteo_2015, by="IdSensore")

write.table(mydata_meteo_2015, "meteo_2015.csv", sep = ";", dec = ",", row.names=FALSE)

#######################################################################
################ Merge based on nearest station #######################
#######################################################################

staz_aria_2015 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2015 <- staz_aria_2015[,-c(9,10,11,16)]
colnames(staz_aria_2015)[1] = c("IdSensore")

meteo_2015 <- read.csv("meteo_2015.csv", header=TRUE, dec = ',', sep = ';')
staz_meteo_2015_ids <- unique(meteo_2015$IdStazione)
staz_meteo_2015_ids <- as.data.frame(staz_meteo_2015_ids)
names(staz_meteo_2015_ids) <- "IdStazione"

staz_meteo_2015 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2015 <- staz_meteo_2015[,-c(8,9,10,15)]
colnames(staz_meteo_2015)[1] = c("IdSensore")

staz_meteo_2015 <- merge(staz_meteo_2015,staz_meteo_2015_ids,by="IdStazione")
tipologie <- unique(staz_meteo_2015$Tipologia) #velocità vento è il quinto valore

staz_PM10= staz_aria_2015[which(staz_aria_2015$NomeTipoSensore=="PM10 (SM2005)"),]

staz_PM10['id_temp'] <- 0  # add new column
staz_PM10['id_rain'] <- 0  # add new column
staz_PM10['id_wind'] <- 0  # add new column

# find nearest weather station for each pollution one and attach id

for (i in 1:dim(staz_PM10)[1]){
  
  dist_temp= 10000
  dist_rain= 10000
  dist_wind= 10000
  
  lat_i = staz_PM10[i,11]
  long_i = staz_PM10[i,12]
  
  id_temp_i = 1456
  id_rain_i = 1456
  id_wind_i = 1456
  
  for (j in 1:dim(staz_meteo_2015)[1]){
    
    lat_j = staz_meteo_2015[j,11]
    long_j = staz_meteo_2015[j,10]
    
    dist_new= sqrt((lat_i-lat_j)^2+(long_i-long_j)^2)
    
    if(dist_new < dist_temp & staz_meteo_2015[j,3]=="Temperatura" ){
      
      dist_temp = dist_new
      id_temp_i = staz_meteo_2015[j,1]
      
    }
    
    if(dist_new < dist_rain & staz_meteo_2015[j,3]=="Precipitazione" ){
      
      dist_rain = dist_new
      id_rain_i = staz_meteo_2015[j,1]
      
    }
    
    if(dist_new < dist_wind & staz_meteo_2015[j,3]==tipologie[5] ){
      
      dist_wind = dist_new
      id_wind_i = staz_meteo_2015[j,1]
      
    }
    
  }
  
  staz_PM10[i,13]= id_temp_i
  staz_PM10[i,14]= id_rain_i
  staz_PM10[i,15]= id_wind_i
  
}

# add daily values and ids to pollution measurements

aria_giornaliero <- read.csv("aria_giornaliero_2015.csv", header=TRUE, dec = ',', sep = ';')
PM10 <- aria_giornaliero[which(aria_giornaliero$NomeTipoSensore== 'PM10 (SM2005)'),]

ids_to_attach = staz_PM10[,c(4,13,14,15)]

PM10 = merge(PM10,ids_to_attach,by="Idstazione")

# temperature

temp = meteo_2015[which(meteo_2015$Tipologia=="Temperatura"),c(2,3,6)]
names(temp)[3]= "id_temp"
names(temp)[2]= "val_temp"

PM10 = merge(PM10,temp,by=c("Date","id_temp"), all.x = TRUE)

# rainfall

rain = meteo_2015[which(meteo_2015$Tipologia=="Precipitazione"),c(2,3,6)]
names(rain)[3]= "id_rain"
names(rain)[2]= "val_rain"

PM10 = merge(PM10,rain,by=c("Date","id_rain"), all.x = TRUE)

# wind

wind = meteo_2015[which(meteo_2015$Tipologia==tipologie[5]),c(2,3,6)]
names(wind)[3]= "id_wind"
names(wind)[2]= "val_wind"

PM10 = merge(PM10,wind,by=c("Date","id_wind"), all.x = TRUE)


write.table(PM10, "PM10_all_2015.csv", sep = ";", dec = ",", row.names=FALSE)


################################################################################
##################################### 2016 #####################################
################################################################################

####################
########ARIA########
####################

aria_2016 <- read.csv("sensori_aria_2016.csv", header=TRUE, dec = '.', sep = ',')
aria_2016[c('Date', 'Time')] <- str_split_fixed(aria_2016$Data, ' ', 2)
aria_2016 <- aria_2016[,-c(2,4,5,7)]

staz_aria_2016 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2016 <- staz_aria_2016[,-c(9,10,11,16)]
colnames(staz_aria_2016)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2016 <- aggregate(Valore ~ IdSensore + Date, aria_2016, mean)
t <- table(dati_giornalieri_2016$IdSensore)
mydata_aria_2016 <- merge(dati_giornalieri_2016, staz_aria_2016, by="IdSensore")

write.table(mydata_aria_2016, "aria_giornaliero_2016.csv", sep = ";", dec = ",", row.names=FALSE)

####################
#######METEO########
####################

meteo_2016 <- read.csv("sensori_meteo_2016.csv", header=TRUE, dec = '.', sep = ',')
meteo_2016[c('Date', 'Time')] <- str_split_fixed(meteo_2016$Data, ' ', 2)
meteo_2016 <- meteo_2016[,-c(2,4,5,7)]

staz_meteo_2016 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2016 <- staz_meteo_2016[,-c(8,9,10,15)]
colnames(staz_meteo_2016)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2016 <- aggregate(Valore ~ IdSensore + Date, meteo_2016, mean)
t <- table(dati_giornalieri_2016$IdSensore)
mydata_meteo_2016 <- merge(dati_giornalieri_2016, staz_meteo_2016, by="IdSensore")

write.table(mydata_meteo_2016, "meteo_2016.csv", sep = ";", dec = ",", row.names=FALSE)

#######################################################################
################ Merge based on nearest station #######################
#######################################################################

staz_aria_2016 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2016 <- staz_aria_2016[,-c(9,10,11,16)]
colnames(staz_aria_2016)[1] = c("IdSensore")

meteo_2016 <- read.csv("meteo_2016.csv", header=TRUE, dec = ',', sep = ';')
staz_meteo_2016_ids <- unique(meteo_2016$IdStazione)
staz_meteo_2016_ids <- as.data.frame(staz_meteo_2016_ids)
names(staz_meteo_2016_ids) <- "IdStazione"

staz_meteo_2016 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2016 <- staz_meteo_2016[,-c(8,9,10,15)]
colnames(staz_meteo_2016)[1] = c("IdSensore")

staz_meteo_2016 <- merge(staz_meteo_2016,staz_meteo_2016_ids,by="IdStazione")
tipologie <- unique(staz_meteo_2016$Tipologia) #velocità vento è il quinto valore

staz_PM10= staz_aria_2016[which(staz_aria_2016$NomeTipoSensore=="PM10 (SM2005)"),]

staz_PM10['id_temp'] <- 0  # add new column
staz_PM10['id_rain'] <- 0  # add new column
staz_PM10['id_wind'] <- 0  # add new column

# find nearest weather station for each pollution one and attach id

for (i in 1:dim(staz_PM10)[1]){
  
  dist_temp= 10000
  dist_rain= 10000
  dist_wind= 10000
  
  lat_i = staz_PM10[i,11]
  long_i = staz_PM10[i,12]
  
  id_temp_i = 1456
  id_rain_i = 1456
  id_wind_i = 1456
  
  for (j in 1:dim(staz_meteo_2016)[1]){
    
    lat_j = staz_meteo_2016[j,11]
    long_j = staz_meteo_2016[j,10]
    
    dist_new= sqrt((lat_i-lat_j)^2+(long_i-long_j)^2)
    
    if(dist_new < dist_temp & staz_meteo_2016[j,3]=="Temperatura" ){
      
      dist_temp = dist_new
      id_temp_i = staz_meteo_2016[j,1]
      
    }
    
    if(dist_new < dist_rain & staz_meteo_2016[j,3]=="Precipitazione" ){
      
      dist_rain = dist_new
      id_rain_i = staz_meteo_2016[j,1]
      
    }
    
    if(dist_new < dist_wind & staz_meteo_2016[j,3]==tipologie[5] ){
      
      dist_wind = dist_new
      id_wind_i = staz_meteo_2016[j,1]
      
    }
    
  }
  
  staz_PM10[i,13]= id_temp_i
  staz_PM10[i,14]= id_rain_i
  staz_PM10[i,15]= id_wind_i
  
}

# add daily values and ids to pollution measurements

aria_giornaliero <- read.csv("aria_giornaliero_2016.csv", header=TRUE, dec = ',', sep = ';')
PM10 <- aria_giornaliero[which(aria_giornaliero$NomeTipoSensore== 'PM10 (SM2005)'),]

ids_to_attach = staz_PM10[,c(4,13,14,15)]

PM10 = merge(PM10,ids_to_attach,by="Idstazione")

# temperature

temp = meteo_2016[which(meteo_2016$Tipologia=="Temperatura"),c(2,3,6)]
names(temp)[3]= "id_temp"
names(temp)[2]= "val_temp"

PM10 = merge(PM10,temp,by=c("Date","id_temp"), all.x = TRUE)

# rainfall

rain = meteo_2016[which(meteo_2016$Tipologia=="Precipitazione"),c(2,3,6)]
names(rain)[3]= "id_rain"
names(rain)[2]= "val_rain"

PM10 = merge(PM10,rain,by=c("Date","id_rain"), all.x = TRUE)

# wind

wind = meteo_2016[which(meteo_2016$Tipologia==tipologie[5]),c(2,3,6)]
names(wind)[3]= "id_wind"
names(wind)[2]= "val_wind"

PM10 = merge(PM10,wind,by=c("Date","id_wind"), all.x = TRUE)

PM10 = PM10[which(PM10$Date!="29/02/2016"),]  # 2016 è anno bisestile

write.table(PM10, "PM10_all_2016.csv", sep = ";", dec = ",", row.names=FALSE)


################################################################################
##################################### 2017 #####################################
################################################################################

####################
########ARIA########
####################

aria_2017 <- read.csv("sensori_aria_2017.csv", header=TRUE, dec = '.', sep = ',')
aria_2017[c('Date', 'Time')] <- str_split_fixed(aria_2017$Data, ' ', 2)
aria_2017 <- aria_2017[,-c(2,4,5,7)]

staz_aria_2017 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2017 <- staz_aria_2017[,-c(9,10,11,16)]
colnames(staz_aria_2017)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2017 <- aggregate(Valore ~ IdSensore + Date, aria_2017, mean)
t <- table(dati_giornalieri_2017$IdSensore)
mydata_aria_2017 <- merge(dati_giornalieri_2017, staz_aria_2017, by="IdSensore")

write.table(mydata_aria_2017, "aria_giornaliero_2017.csv", sep = ";", dec = ",", row.names=FALSE)

####################
#######METEO########
####################

meteo_2017 <- read.csv("sensori_meteo_2017.csv", header=TRUE, dec = '.', sep = ',')
meteo_2017[c('Date', 'Time')] <- str_split_fixed(meteo_2017$Data, ' ', 2)
meteo_2017 <- meteo_2017[,-c(2,4,5,7)]

staz_meteo_2017 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2017 <- staz_meteo_2017[,-c(8,9,10,15)]
colnames(staz_meteo_2017)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2017 <- aggregate(Valore ~ IdSensore + Date, meteo_2017, mean)
t <- table(dati_giornalieri_2017$IdSensore)
mydata_meteo_2017 <- merge(dati_giornalieri_2017, staz_meteo_2017, by="IdSensore")


write.table(mydata_meteo_2017, "meteo_2017.csv", sep = ";", dec = ",", row.names=FALSE)

#######################################################################
################ Merge based on nearest station #######################
#######################################################################

staz_aria_2017 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2017 <- staz_aria_2017[,-c(9,10,11,16)]
colnames(staz_aria_2017)[1] = c("IdSensore")

meteo_2017 <- read.csv("meteo_2017.csv", header=TRUE, dec = ',', sep = ';')
staz_meteo_2017_ids <- unique(meteo_2017$IdStazione)
staz_meteo_2017_ids <- as.data.frame(staz_meteo_2017_ids)
names(staz_meteo_2017_ids) <- "IdStazione"

staz_meteo_2017 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2017 <- staz_meteo_2017[,-c(8,9,10,15)]
colnames(staz_meteo_2017)[1] = c("IdSensore")

staz_meteo_2017 <- merge(staz_meteo_2017,staz_meteo_2017_ids,by="IdStazione")
tipologie <- unique(staz_meteo_2017$Tipologia) #velocità vento è il quinto valore

staz_PM10= staz_aria_2017[which(staz_aria_2017$NomeTipoSensore=="PM10 (SM2005)"),]

staz_PM10['id_temp'] <- 0  # add new column
staz_PM10['id_rain'] <- 0  # add new column
staz_PM10['id_wind'] <- 0  # add new column

# find nearest weather station for each pollution one and attach id

for (i in 1:dim(staz_PM10)[1]){
  
  dist_temp= 10000
  dist_rain= 10000
  dist_wind= 10000
  
  lat_i = staz_PM10[i,11]
  long_i = staz_PM10[i,12]
  
  id_temp_i = 1456
  id_rain_i = 1456
  id_wind_i = 1456
  
  for (j in 1:dim(staz_meteo_2017)[1]){
    
    lat_j = staz_meteo_2017[j,11]
    long_j = staz_meteo_2017[j,10]
    
    dist_new= sqrt((lat_i-lat_j)^2+(long_i-long_j)^2)
    
    if(dist_new < dist_temp & staz_meteo_2017[j,3]=="Temperatura" ){
      
      dist_temp = dist_new
      id_temp_i = staz_meteo_2017[j,1]
      
    }
    
    if(dist_new < dist_rain & staz_meteo_2017[j,3]=="Precipitazione" ){
      
      dist_rain = dist_new
      id_rain_i = staz_meteo_2017[j,1]
      
    }
    
    if(dist_new < dist_wind & staz_meteo_2017[j,3]==tipologie[5] ){
      
      dist_wind = dist_new
      id_wind_i = staz_meteo_2017[j,1]
      
    }
    
  }
  
  staz_PM10[i,13]= id_temp_i
  staz_PM10[i,14]= id_rain_i
  staz_PM10[i,15]= id_wind_i
  
}

# add daily values and ids to pollution measurements

aria_giornaliero <- read.csv("aria_giornaliero_2017.csv", header=TRUE, dec = ',', sep = ';')
PM10 <- aria_giornaliero[which(aria_giornaliero$NomeTipoSensore== 'PM10 (SM2005)'),]

ids_to_attach = staz_PM10[,c(4,13,14,15)]

PM10 = merge(PM10,ids_to_attach,by="Idstazione")

# temperature

temp = meteo_2017[which(meteo_2017$Tipologia=="Temperatura"),c(2,3,6)]
names(temp)[3]= "id_temp"
names(temp)[2]= "val_temp"

PM10 = merge(PM10,temp,by=c("Date","id_temp"), all.x = TRUE)

# rainfall

rain = meteo_2017[which(meteo_2017$Tipologia=="Precipitazione"),c(2,3,6)]
names(rain)[3]= "id_rain"
names(rain)[2]= "val_rain"

PM10 = merge(PM10,rain,by=c("Date","id_rain"), all.x = TRUE)

# wind

wind = meteo_2017[which(meteo_2017$Tipologia==tipologie[5]),c(2,3,6)]
names(wind)[3]= "id_wind"
names(wind)[2]= "val_wind"

PM10 = merge(PM10,wind,by=c("Date","id_wind"), all.x = TRUE)

write.table(PM10, "PM10_all_2017.csv", sep = ";", dec = ",", row.names=FALSE)


################################################################################
##################################### 2018 #####################################
################################################################################

####################
########ARIA########
####################

aria_2018 <- read.csv("sensori_aria_2018.csv", header=TRUE, dec = '.', sep = ',')
aria_2018[c('Date', 'Time')] <- str_split_fixed(aria_2018$Data, ' ', 2)
aria_2018 <- aria_2018[,-c(2,4,5,7)]

staz_aria_2018 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2018 <- staz_aria_2018[,-c(9,10,11,16)]
colnames(staz_aria_2018)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2018 <- aggregate(Valore ~ IdSensore + Date, aria_2018, mean)
t <- table(dati_giornalieri_2018$IdSensore)
mydata_aria_2018 <- merge(dati_giornalieri_2018, staz_aria_2018, by="IdSensore")

write.table(mydata_aria_2018, "aria_giornaliero_2018.csv", sep = ";", dec = ",", row.names=FALSE)

####################
#######METEO########
####################

meteo_2018 <- read.csv("sensori_meteo_2018.csv", header=TRUE, dec = '.', sep = ',')
meteo_2018[c('Date', 'Time')] <- str_split_fixed(meteo_2018$Data, ' ', 2)
meteo_2018 <- meteo_2018[,-c(2,4,5,7)]

staz_meteo_2018 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2018 <- staz_meteo_2018[,-c(8,9,10,15)]
colnames(staz_meteo_2018)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2018 <- aggregate(Valore ~ IdSensore + Date, meteo_2018, mean)
t <- table(dati_giornalieri_2018$IdSensore)
mydata_meteo_2018 <- merge(dati_giornalieri_2018, staz_meteo_2018, by="IdSensore")

write.table(mydata_meteo_2018, "meteo_2018.csv", sep = ";", dec = ",", row.names=FALSE)

#######################################################################
################ Merge based on nearest station #######################
#######################################################################

staz_aria_2018 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2018 <- staz_aria_2018[,-c(9,10,11,16)]
colnames(staz_aria_2018)[1] = c("IdSensore")

meteo_2018 <- read.csv("meteo_2018.csv", header=TRUE, dec = ',', sep = ';')
staz_meteo_2018_ids <- unique(meteo_2018$IdStazione)
staz_meteo_2018_ids <- as.data.frame(staz_meteo_2018_ids)
names(staz_meteo_2018_ids) <- "IdStazione"

staz_meteo_2018 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2018 <- staz_meteo_2018[,-c(8,9,10,15)]
colnames(staz_meteo_2018)[1] = c("IdSensore")

staz_meteo_2018 <- merge(staz_meteo_2018,staz_meteo_2018_ids,by="IdStazione")
tipologie <- unique(staz_meteo_2018$Tipologia) #velocità vento è il quinto valore

staz_PM10= staz_aria_2018[which(staz_aria_2018$NomeTipoSensore=="PM10 (SM2005)"),]

staz_PM10['id_temp'] <- 0  # add new column
staz_PM10['id_rain'] <- 0  # add new column
staz_PM10['id_wind'] <- 0  # add new column

# find nearest weather station for each pollution one and attach id

for (i in 1:dim(staz_PM10)[1]){
  
  dist_temp= 10000
  dist_rain= 10000
  dist_wind= 10000
  
  lat_i = staz_PM10[i,11]
  long_i = staz_PM10[i,12]
  
  id_temp_i = 1456
  id_rain_i = 1456
  id_wind_i = 1456
  
  for (j in 1:dim(staz_meteo_2018)[1]){
    
    lat_j = staz_meteo_2018[j,11]
    long_j = staz_meteo_2018[j,10]
    
    dist_new= sqrt((lat_i-lat_j)^2+(long_i-long_j)^2)
    
    if(dist_new < dist_temp & staz_meteo_2018[j,3]=="Temperatura" ){
      
      dist_temp = dist_new
      id_temp_i = staz_meteo_2018[j,1]
      
    }
    
    if(dist_new < dist_rain & staz_meteo_2018[j,3]=="Precipitazione" ){
      
      dist_rain = dist_new
      id_rain_i = staz_meteo_2018[j,1]
      
    }
    
    if(dist_new < dist_wind & staz_meteo_2018[j,3]==tipologie[5] ){
      
      dist_wind = dist_new
      id_wind_i = staz_meteo_2018[j,1]
      
    }
    
  }
  
  staz_PM10[i,13]= id_temp_i
  staz_PM10[i,14]= id_rain_i
  staz_PM10[i,15]= id_wind_i
  
}

# add daily values and ids to pollution measurements

aria_giornaliero <- read.csv("aria_giornaliero_2018.csv", header=TRUE, dec = ',', sep = ';')
PM10 <- aria_giornaliero[which(aria_giornaliero$NomeTipoSensore== 'PM10 (SM2005)'),]

ids_to_attach = staz_PM10[,c(4,13,14,15)]

PM10 = merge(PM10,ids_to_attach,by="Idstazione")

# temperature

temp = meteo_2018[which(meteo_2018$Tipologia=="Temperatura"),c(2,3,6)]
names(temp)[3]= "id_temp"
names(temp)[2]= "val_temp"

PM10 = merge(PM10,temp,by=c("Date","id_temp"), all.x = TRUE)

# rainfall

rain = meteo_2018[which(meteo_2018$Tipologia=="Precipitazione"),c(2,3,6)]
names(rain)[3]= "id_rain"
names(rain)[2]= "val_rain"

PM10 = merge(PM10,rain,by=c("Date","id_rain"), all.x = TRUE)

# wind

wind = meteo_2018[which(meteo_2018$Tipologia==tipologie[5]),c(2,3,6)]
names(wind)[3]= "id_wind"
names(wind)[2]= "val_wind"

PM10 = merge(PM10,wind,by=c("Date","id_wind"), all.x = TRUE)

write.table(PM10, "PM10_all_2018.csv", sep = ";", dec = ",", row.names=FALSE)


################################################################################
##################################### 2019 #####################################
################################################################################

####################
########ARIA########
####################

aria_2019 <- read.csv("sensori_aria_2019.csv", header=TRUE, dec = '.', sep = ',')
aria_2019[c('Date', 'Time')] <- str_split_fixed(aria_2019$Data, ' ', 2)
aria_2019 <- aria_2019[,-c(2,4,5,7)]

staz_aria_2019 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2019 <- staz_aria_2019[,-c(9,10,11,16)]
colnames(staz_aria_2019)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2019 <- aggregate(Valore ~ IdSensore + Date, aria_2019, mean)
t <- table(dati_giornalieri_2019$IdSensore)
mydata_aria_2019 <- merge(dati_giornalieri_2019, staz_aria_2019, by="IdSensore")

write.table(mydata_aria_2019, "aria_giornaliero_2019.csv", sep = ";", dec = ",", row.names=FALSE)

####################
#######METEO########
####################

meteo_2019 <- read.csv("sensori_meteo_2019.csv", header=TRUE, dec = '.', sep = ',')
meteo_2019[c('Date', 'Time')] <- str_split_fixed(meteo_2019$Data, ' ', 2)
meteo_2019 <- meteo_2019[,-c(2,4,5,7)]

staz_meteo_2019 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2019 <- staz_meteo_2019[,-c(8,9,10,15)]
colnames(staz_meteo_2019)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2019 <- aggregate(Valore ~ IdSensore + Date, meteo_2019, mean)
t <- table(dati_giornalieri_2019$IdSensore)
mydata_meteo_2019 <- merge(dati_giornalieri_2019, staz_meteo_2019, by="IdSensore")

write.table(mydata_meteo_2019, "meteo_2019.csv", sep = ";", dec = ",", row.names=FALSE)

#######################################################################
################ Merge based on nearest station #######################
#######################################################################

staz_aria_2019 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2019 <- staz_aria_2019[,-c(9,10,11,16)]
colnames(staz_aria_2019)[1] = c("IdSensore")

meteo_2019 <- read.csv("meteo_2019.csv", header=TRUE, dec = ',', sep = ';')
staz_meteo_2019_ids <- unique(meteo_2019$IdStazione)
staz_meteo_2019_ids <- as.data.frame(staz_meteo_2019_ids)
names(staz_meteo_2019_ids) <- "IdStazione"

staz_meteo_2019 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2019 <- staz_meteo_2019[,-c(8,9,10,15)]
colnames(staz_meteo_2019)[1] = c("IdSensore")

staz_meteo_2019 <- merge(staz_meteo_2019,staz_meteo_2019_ids,by="IdStazione")
tipologie <- unique(staz_meteo_2019$Tipologia) #velocità vento è il quinto valore

staz_PM10= staz_aria_2019[which(staz_aria_2019$NomeTipoSensore=="PM10 (SM2005)"),]

staz_PM10['id_temp'] <- 0  # add new column
staz_PM10['id_rain'] <- 0  # add new column
staz_PM10['id_wind'] <- 0  # add new column

# find nearest weather station for each pollution one and attach id

for (i in 1:dim(staz_PM10)[1]){
  
  dist_temp= 10000
  dist_rain= 10000
  dist_wind= 10000
  
  lat_i = staz_PM10[i,11]
  long_i = staz_PM10[i,12]
  
  id_temp_i = 1456
  id_rain_i = 1456
  id_wind_i = 1456
  
  for (j in 1:dim(staz_meteo_2019)[1]){
    
    lat_j = staz_meteo_2019[j,11]
    long_j = staz_meteo_2019[j,10]
    
    dist_new= sqrt((lat_i-lat_j)^2+(long_i-long_j)^2)
    
    if(dist_new < dist_temp & staz_meteo_2019[j,3]=="Temperatura" ){
      
      dist_temp = dist_new
      id_temp_i = staz_meteo_2019[j,1]
      
    }
    
    if(dist_new < dist_rain & staz_meteo_2019[j,3]=="Precipitazione" ){
      
      dist_rain = dist_new
      id_rain_i = staz_meteo_2019[j,1]
      
    }
    
    if(dist_new < dist_wind & staz_meteo_2019[j,3]==tipologie[5] ){
      
      dist_wind = dist_new
      id_wind_i = staz_meteo_2019[j,1]
      
    }
    
  }
  
  staz_PM10[i,13]= id_temp_i
  staz_PM10[i,14]= id_rain_i
  staz_PM10[i,15]= id_wind_i
  
}

# add daily values and ids to pollution measurements

aria_giornaliero <- read.csv("aria_giornaliero_2019.csv", header=TRUE, dec = ',', sep = ';')
PM10 <- aria_giornaliero[which(aria_giornaliero$NomeTipoSensore== 'PM10 (SM2005)'),]

ids_to_attach = staz_PM10[,c(4,13,14,15)]

PM10 = merge(PM10,ids_to_attach,by="Idstazione")

# temperature

temp = meteo_2019[which(meteo_2019$Tipologia=="Temperatura"),c(2,3,6)]
names(temp)[3]= "id_temp"
names(temp)[2]= "val_temp"

PM10 = merge(PM10,temp,by=c("Date","id_temp"), all.x = TRUE)

# rainfall

rain = meteo_2019[which(meteo_2019$Tipologia=="Precipitazione"),c(2,3,6)]
names(rain)[3]= "id_rain"
names(rain)[2]= "val_rain"

PM10 = merge(PM10,rain,by=c("Date","id_rain"), all.x = TRUE)

# wind

wind = meteo_2019[which(meteo_2019$Tipologia==tipologie[5]),c(2,3,6)]
names(wind)[3]= "id_wind"
names(wind)[2]= "val_wind"

PM10 = merge(PM10,wind,by=c("Date","id_wind"), all.x = TRUE)

write.table(PM10, "PM10_all_2019.csv", sep = ";", dec = ",", row.names=FALSE)


################################################################################
##################################### 2020 #####################################
################################################################################

####################
########ARIA########
####################

aria_2020 <- read.csv("sensori_aria_2020.csv", header=TRUE, dec = '.', sep = ',')
aria_2020[c('Date', 'Time')] <- str_split_fixed(aria_2020$Data, ' ', 2)
aria_2020 <- aria_2020[,-c(2,4,5,7)]

staz_aria_2020 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2020 <- staz_aria_2020[,-c(9,10,11,16)]
colnames(staz_aria_2020)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2020 <- aggregate(Valore ~ IdSensore + Date, aria_2020, mean)
t <- table(dati_giornalieri_2020$IdSensore)
mydata_aria_2020 <- merge(dati_giornalieri_2020, staz_aria_2020, by="IdSensore")

write.table(mydata_aria_2020, "aria_giornaliero_2020.csv", sep = ";", dec = ",", row.names=FALSE)

####################
#######METEO########
####################

meteo_2020 <- read.csv("sensori_meteo_2020.csv", header=TRUE, dec = '.', sep = ',')
meteo_2020[c('Date', 'Time')] <- str_split_fixed(meteo_2020$Data, ' ', 2)
meteo_2020 <- meteo_2020[,-c(2,4,5,7)]

staz_meteo_2020 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2020 <- staz_meteo_2020[,-c(8,9,10,15)]
colnames(staz_meteo_2020)[1] = c("IdSensore")

#daily mean
dati_giornalieri_2020 <- aggregate(Valore ~ IdSensore + Date, meteo_2020, mean)
t <- table(dati_giornalieri_2020$IdSensore)
mydata_meteo_2020 <- merge(dati_giornalieri_2020, staz_meteo_2020, by="IdSensore")

write.table(mydata_meteo_2020, "meteo_2020.csv", sep = ";", dec = ",", row.names=FALSE)

#######################################################################
################ Merge based on nearest station #######################
#######################################################################

staz_aria_2020 <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria_2020 <- staz_aria_2020[,-c(9,10,11,16)]
colnames(staz_aria_2020)[1] = c("IdSensore")

meteo_2020 <- read.csv("meteo_2020.csv", header=TRUE, dec = ',', sep = ';')
staz_meteo_2020_ids <- unique(meteo_2020$IdStazione)
staz_meteo_2020_ids <- as.data.frame(staz_meteo_2020_ids)
names(staz_meteo_2020_ids) <- "IdStazione"

staz_meteo_2020 <- read.csv("Stazioni_meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo_2020 <- staz_meteo_2020[,-c(8,9,10,15)]
colnames(staz_meteo_2020)[1] = c("IdSensore")

staz_meteo_2020 <- merge(staz_meteo_2020,staz_meteo_2020_ids,by="IdStazione")
tipologie <- unique(staz_meteo_2020$Tipologia) #velocità vento è il quinto valore

staz_PM10= staz_aria_2020[which(staz_aria_2020$NomeTipoSensore=="PM10 (SM2005)"),]

staz_PM10['id_temp'] <- 0  # add new column
staz_PM10['id_rain'] <- 0  # add new column
staz_PM10['id_wind'] <- 0  # add new column

# find nearest weather station for each pollution one and attach id

for (i in 1:dim(staz_PM10)[1]){
  
  dist_temp= 10000
  dist_rain= 10000
  dist_wind= 10000
  
  lat_i = staz_PM10[i,11]
  long_i = staz_PM10[i,12]
  
  id_temp_i = 1456
  id_rain_i = 1456
  id_wind_i = 1456
  
  for (j in 1:dim(staz_meteo_2020)[1]){
    
    lat_j = staz_meteo_2020[j,11]
    long_j = staz_meteo_2020[j,10]
    
    dist_new= sqrt((lat_i-lat_j)^2+(long_i-long_j)^2)
    
    if(dist_new < dist_temp & staz_meteo_2020[j,3]=="Temperatura" ){
      
      dist_temp = dist_new
      id_temp_i = staz_meteo_2020[j,1]
      
    }
    
    if(dist_new < dist_rain & staz_meteo_2020[j,3]=="Precipitazione" ){
      
      dist_rain = dist_new
      id_rain_i = staz_meteo_2020[j,1]
      
    }
    
    if(dist_new < dist_wind & staz_meteo_2020[j,3]==tipologie[5] ){
      
      dist_wind = dist_new
      id_wind_i = staz_meteo_2020[j,1]
      
    }
    
  }
  
  staz_PM10[i,13]= id_temp_i
  staz_PM10[i,14]= id_rain_i
  staz_PM10[i,15]= id_wind_i
  
}

# add daily values and ids to pollution measurements

aria_giornaliero <- read.csv("aria_giornaliero_2020.csv", header=TRUE, dec = ',', sep = ';')
PM10 <- aria_giornaliero[which(aria_giornaliero$NomeTipoSensore== 'PM10 (SM2005)'),]

ids_to_attach = staz_PM10[,c(4,13,14,15)]

PM10 = merge(PM10,ids_to_attach,by="Idstazione")

# temperature

temp = meteo_2020[which(meteo_2020$Tipologia=="Temperatura"),c(2,3,6)]
names(temp)[3]= "id_temp"
names(temp)[2]= "val_temp"

PM10 = merge(PM10,temp,by=c("Date","id_temp"), all.x = TRUE)

# rainfall

rain = meteo_2020[which(meteo_2020$Tipologia=="Precipitazione"),c(2,3,6)]
names(rain)[3]= "id_rain"
names(rain)[2]= "val_rain"

PM10 = merge(PM10,rain,by=c("Date","id_rain"), all.x = TRUE)

# wind

wind = meteo_2020[which(meteo_2020$Tipologia==tipologie[5]),c(2,3,6)]
names(wind)[3]= "id_wind"
names(wind)[2]= "val_wind"

PM10 = merge(PM10,wind,by=c("Date","id_wind"), all.x = TRUE)

write.table(PM10, "PM10_all_2020.csv", sep = ";", dec = ",", row.names=FALSE)


################################################################################
##################################### 2021 #####################################
################################################################################

####################
########ARIA########
####################

aria <- read.csv("2021_sensori_aria.csv", header=TRUE, dec = '.', sep = ',')
aria[c('Date', 'Time')] <- str_split_fixed(aria$Data, ' ', 2)
aria <- aria[,-c(2,4,5,7)]

staz_aria <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria <- staz_aria[,-c(9,10,11,16)]
colnames(staz_aria)[1] = c("IdSensore")

#daily mean
dati_giornalieri <- aggregate(Valore ~ IdSensore + Date, aria, mean)
t <- table(dati_giornalieri$IdSensore)
mydata_aria <- merge(dati_giornalieri, staz_aria, by="IdSensore")

write.table(mydata_aria, "aria_giornaliero.csv", sep = ";", dec = ",", row.names=FALSE)

####################
#######METEO########
####################

meteo <- read.csv("2021_sensori_meteo.csv", header=TRUE, dec = '.', sep = ',')
meteo[c('Date', 'Time')] <- str_split_fixed(meteo$Data, ' ', 2)
meteo <- meteo[,-c(2,4,5,7)]

staz_meteo <- read.csv("Stazioni_Meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo <- staz_meteo[,-c(8,9,10,15)]
colnames(staz_meteo)[1] = c("IdSensore")

#daily mean
dati_giornalieri <- aggregate(Valore ~ IdSensore + Date, meteo, mean)
t <- table(dati_giornalieri$IdSensore)
mydata_meteo <- merge(dati_giornalieri, staz_meteo, by="IdSensore")

write.table(mydata_meteo, "meteo.csv", sep = ";", dec = ",", row.names=FALSE)

#######################################################################
################ Merge based on nearest station #######################
#######################################################################

staz_aria <- read.csv("Stazioni_qualit__dell_aria.csv", header=TRUE, dec = '.', sep = ',')
staz_aria <- staz_aria[,-c(9,10,11,16)]
colnames(staz_aria)[1] = c("IdSensore")

meteo <- read.csv("meteo.csv", header=TRUE, dec = ',', sep = ';')
staz_meteo_ids <- unique(meteo$IdStazione)
staz_meteo_ids <- as.data.frame(staz_meteo_ids)
names(staz_meteo_ids) <- "IdStazione"

staz_meteo <- read.csv("Stazioni_Meteorologiche.csv", header=TRUE, dec = '.', sep = ',')
staz_meteo <- staz_meteo[,-c(8,9,10,15)]
colnames(staz_meteo)[1] = c("IdSensore")

staz_meteo <- merge(staz_meteo,staz_meteo_ids,by="IdStazione")
tipologie <- unique(staz_meteo$Tipologia) #velocità vento è il settimo valore

staz_PM10= staz_aria[which(staz_aria$NomeTipoSensore=="PM10 (SM2005)"),]

staz_PM10['id_temp'] <- 0  # add new column
staz_PM10['id_rain'] <- 0  # add new column
staz_PM10['id_wind'] <- 0  # add new column

# find nearest weather station for each pollution one and attach id

for (i in 1:dim(staz_PM10)[1]){
  
  dist_temp= 10000
  dist_rain= 10000
  dist_wind= 10000
  
  lat_i = staz_PM10[i,11]
  long_i = staz_PM10[i,12]
  
  id_temp_i = 1456
  id_rain_i = 1456
  id_wind_i = 1456
  
  for (j in 1:dim(staz_meteo)[1]){
    
    lat_j = staz_meteo[j,11]
    long_j = staz_meteo[j,10]
    
    dist_new= sqrt((lat_i-lat_j)^2+(long_i-long_j)^2)
    
    if(dist_new < dist_temp & staz_meteo[j,3]=="Temperatura" ){
      
      dist_temp = dist_new
      id_temp_i = staz_meteo[j,1]
      
    }
    
    if(dist_new < dist_rain & staz_meteo[j,3]=="Precipitazione" ){
      
      dist_rain = dist_new
      id_rain_i = staz_meteo[j,1]
      
    }
    
    if(dist_new < dist_wind & staz_meteo[j,3]==tipologie[7] ){
      
      dist_wind = dist_new
      id_wind_i = staz_meteo[j,1]
      
    }
    
  }
  
  staz_PM10[i,13]= id_temp_i
  staz_PM10[i,14]= id_rain_i
  staz_PM10[i,15]= id_wind_i
  
}

# add daily values and ids to pollution measurements

aria_giornaliero <- read.csv("aria_giornaliero.csv", header=TRUE, dec = ',', sep = ';')
PM10 <- aria_giornaliero[which(aria_giornaliero$NomeTipoSensore== 'PM10 (SM2005)'),]

ids_to_attach = staz_PM10[,c(4,13,14,15)]

PM10 = merge(PM10,ids_to_attach,by="Idstazione")

# temperature

temp = meteo[which(meteo$Tipologia=="Temperatura"),c(2,3,6)]
names(temp)[3]= "id_temp"
names(temp)[2]= "val_temp"

PM10 = merge(PM10,temp,by=c("Date","id_temp"), all.x = TRUE)

# rainfall

rain = meteo[which(meteo$Tipologia=="Precipitazione"),c(2,3,6)]
names(rain)[3]= "id_rain"
names(rain)[2]= "val_rain"

PM10 = merge(PM10,rain,by=c("Date","id_rain"), all.x = TRUE)

# wind

wind = meteo[which(meteo$Tipologia==tipologie[7]),c(2,3,6)]
names(wind)[3]= "id_wind"
names(wind)[2]= "val_wind"

PM10 = merge(PM10,wind,by=c("Date","id_wind"), all.x = TRUE)

write.table(PM10, "PM10_all.csv", sep = ";", dec = ",", row.names = FALSE)

