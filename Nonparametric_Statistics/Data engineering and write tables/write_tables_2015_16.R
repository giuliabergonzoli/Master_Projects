#loading necessary libraries
library(roahd)
library(DepthProc)
library(MASS)
library(rgl)
library(fda)

################################################################################
###################### CREATION MATRICES PM10 2015 - 2016 ######################
################################################################################

############################
########### 2015 ###########
############################

#to start: go to file dataset.R and run the section 2015 to create the necessary files

data2015 <- read.csv("PM10_all_2015.csv", header=TRUE, dec = ',', sep = ';')
data2015$Date <- as.character.Date(data2015$Date)

#LOMBARDIA: creating time series for each station
PM10_2015 = data2015
PM10_short_2015= PM10_2015[,c(1,7,10,5)]
PM10_short_2015$NomeStazione = as.factor(PM10_short_2015$NomeStazione)

stazioni_lomb_2015 = as.factor(levels(PM10_short_2015$NomeStazione))
PM10_short_2015$Date =as.Date(PM10_short_2015$Date, format="%d/%m/%Y")
Data_lomb_2015 = unique(PM10_short_2015$Date)

PM10_lomb_2015 = as.data.frame(Data_lomb_2015)
colnames(PM10_lomb_2015) = c("Date")

for(i in 2:(length(stazioni_lomb_2015)+1)){
  stazione_i_lomb_2015 = PM10_short_2015[which(PM10_short_2015$NomeStazione==stazioni_lomb_2015[i-1]),c(1,2)]
  PM10_lomb_2015 = merge(PM10_lomb_2015,stazione_i_lomb_2015, by = "Date", all.x = TRUE)
}

colnames(PM10_lomb_2015) = c("Date",as.character(stazioni_lomb_2015))

#replace the -9999 with NAs
for(j in 2:(length(stazioni_lomb_2015)+1)){
  stazione_j_lomb = PM10_lomb_2015[,j]
  row_na_lomb = which(stazione_j_lomb==-9999)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- NA
      else
        stazione_j_lomb[row_na_lomb[i]] <- NA
    }
  }
  PM10_lomb_2015[,j] = stazione_j_lomb
}

PM10_lomb_2015[,1] = as.Date(PM10_lomb_2015[,1], format="%d/%m/%Y")
PM10_lomb_2015 = PM10_lomb_2015[which(PM10_lomb_2015$Date>'2015-08-31'),]
PM10_lomb_2015[,80] = as.numeric(as.Date(PM10_lomb_2015[,1], format="%d/%m/%Y"))-16678

write.table(PM10_lomb_2015, "matrixPM10_2015_sett.csv", sep = ";", dec = ",", row.names=FALSE)

#remove the stations where there are too many NAs (>=75)
colSums(is.na(PM10_lomb_2015))
tokeep_cols = colSums(is.na(PM10_lomb_2015))[which(colSums(is.na(PM10_lomb_2015))<75)]
PM10_lomb_2015 <- PM10_lomb_2015[,names(tokeep_cols)]

#replacing the remaining NAs with the previous value (mean of the month in the case 01/01/2015)
for(j in 2:(length(PM10_lomb_2015[1,])-1)){
  stazione_j_lomb = PM10_lomb_2015[,j]
  row_na_lomb = which(is.na(stazione_j_lomb)==TRUE)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- mean(stazione_j_lomb[seq(1,30)],na.rm = TRUE)
      else
        stazione_j_lomb[row_na_lomb[i]] <- stazione_j_lomb[row_na_lomb[i]-1]
    }
  }
  PM10_lomb_2015[,j] = stazione_j_lomb
}

write.table(PM10_lomb_2015, "matrixPM10_2015_noNA_sett.csv", sep = ";", dec = ",", row.names=FALSE)

############################
########### 2016 ###########
############################

#to start: go to file dataset.R and run the section 2016 to create the necessary files

data2016 <- read.csv("PM10_all_2016.csv", header=TRUE, dec = ',', sep = ';')    # without 29/02/2016 
data2016$Date <- as.character.Date(data2016$Date)

#LOMBARDIA: creating time series for each station
PM10_2016 = data2016
PM10_short_2016= PM10_2016[,c(1,7,10,5)]
PM10_short_2016$NomeStazione = as.factor(PM10_short_2016$NomeStazione)

stazioni_lomb_2016 = as.factor(levels(PM10_short_2016$NomeStazione))
PM10_short_2016$Date =as.Date(PM10_short_2016$Date, format="%d/%m/%Y")
Data_lomb_2016 = unique(PM10_short_2016$Date)

PM10_lomb_2016 = as.data.frame(Data_lomb_2016)
colnames(PM10_lomb_2016) = c("Date")

for(i in 2:(length(stazioni_lomb_2016)+1)){
  stazione_i_lomb_2016 = PM10_short_2016[which(PM10_short_2016$NomeStazione==stazioni_lomb_2016[i-1]),c(1,2)]
  PM10_lomb_2016 = merge(PM10_lomb_2016,stazione_i_lomb_2016, by = "Date", all.x = TRUE)
}

colnames(PM10_lomb_2016) = c("Date",as.character(stazioni_lomb_2016))

#replace the -9999 with NAs
for(j in 2:(length(stazioni_lomb_2016)+1)){
  stazione_j_lomb = PM10_lomb_2016[,j]
  row_na_lomb = which(stazione_j_lomb==-9999)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- NA
      else
        stazione_j_lomb[row_na_lomb[i]] <- NA
    }
  }
  PM10_lomb_2016[,j] = stazione_j_lomb
}

PM10_lomb_2016[,1] = as.Date(PM10_lomb_2016[,1], format="%d/%m/%Y")
PM10_lomb_2016 = PM10_lomb_2016[which(PM10_lomb_2016$Date<'2016-09-01'),]
PM10_lomb_2016[,80] = as.numeric(as.Date(PM10_lomb_2016[,1], format="%d/%m/%Y"))-16800+122

write.table(PM10_lomb_2016, "matrixPM10_2016_sett.csv", sep = ";", dec = ",", row.names=FALSE)

#remove the stations where there are too many NAs (>=75)
colSums(is.na(PM10_lomb_2016))
tokeep_cols = colSums(is.na(PM10_lomb_2016))[which(colSums(is.na(PM10_lomb_2016))<75)]
PM10_lomb_2016 <- PM10_lomb_2016[,names(tokeep_cols)]

#replacing the remaining NAs with the previous value (mean of the month in the case 01/01/2016)
for(j in 2:(length(PM10_lomb_2016[1,]))){
  stazione_j_lomb = PM10_lomb_2016[,j]
  row_na_lomb = which(is.na(stazione_j_lomb)==TRUE)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- mean(stazione_j_lomb[seq(1,30)],na.rm = TRUE)
      else
        stazione_j_lomb[row_na_lomb[i]] <- stazione_j_lomb[row_na_lomb[i]-1]
    }
  }
  PM10_lomb_2016[,j] = stazione_j_lomb
}

write.table(PM10_lomb_2016, "matrixPM10_2016_noNA_sett.csv", sep = ";", dec = ",", row.names=FALSE)

