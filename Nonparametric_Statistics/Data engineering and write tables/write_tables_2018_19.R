#loading necessary libraries
library(roahd)
library(DepthProc)
library(MASS)
library(rgl)
library(fda)

################################################################################
###################### CREATION MATRICES PM10 2018 - 2019 ######################
################################################################################

############################
########### 2018 ###########
############################

#to start: go to file dataset.R and run the section 2018 to create the necessary files

data2018 <- read.csv("PM10_all_2018.csv", header=TRUE, dec = ',', sep = ';')
data2018$Date <- as.character.Date(data2018$Date)

#LOMBARDIA: creating time series for each station
PM10_2018 = data2018
PM10_short_2018= PM10_2018[,c(1,7,10,5)]
PM10_short_2018$NomeStazione = as.factor(PM10_short_2018$NomeStazione)

stazioni_lomb_2018 = as.factor(levels(PM10_short_2018$NomeStazione))
PM10_short_2018$Date =as.Date(PM10_short_2018$Date, format="%d/%m/%Y")
Data_lomb_2018 = unique(PM10_short_2018$Date)

PM10_lomb_2018 = as.data.frame(Data_lomb_2018)
colnames(PM10_lomb_2018) = c("Date")

for(i in 2:(length(stazioni_lomb_2018)+1)){
  stazione_i_lomb_2018 = PM10_short_2018[which(PM10_short_2018$NomeStazione==stazioni_lomb_2018[i-1]),c(1,2)]
  PM10_lomb_2018 = merge(PM10_lomb_2018,stazione_i_lomb_2018, by = "Date", all.x = TRUE)
}

colnames(PM10_lomb_2018) = c("Date",as.character(stazioni_lomb_2018))

#replace the -9999 with NAs
for(j in 2:(length(stazioni_lomb_2018)+1)){
  stazione_j_lomb = PM10_lomb_2018[,j]
  row_na_lomb = which(stazione_j_lomb==-9999)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- NA
      else
        stazione_j_lomb[row_na_lomb[i]] <- NA
    }
  }
  PM10_lomb_2018[,j] = stazione_j_lomb
}

PM10_lomb_2018[,1] = as.Date(PM10_lomb_2018[,1], format="%d/%m/%Y")
PM10_lomb_2018 = PM10_lomb_2018[which(PM10_lomb_2018$Date>'2018-08-31'),]
PM10_lomb_2018[,66] = as.numeric(as.Date(PM10_lomb_2018[,1], format="%d/%m/%Y"))-18261-244

write.table(PM10_lomb_2018, "matrixPM10_2018_sett.csv", sep = ";", dec = ",", row.names=FALSE)

#remove the stations where there are too many NAs (>=75)
colSums(is.na(PM10_lomb_2018))
tokeep_cols = colSums(is.na(PM10_lomb_2018))[which(colSums(is.na(PM10_lomb_2018))<75)]
PM10_lomb_2018 <- PM10_lomb_2018[,names(tokeep_cols)]

#replacing the remaining NAs with the previous value (mean of the month in the case 01/01/2018)
for(j in 2:(length(PM10_lomb_2018[1,]))){
  stazione_j_lomb = PM10_lomb_2018[,j]
  row_na_lomb = which(is.na(stazione_j_lomb)==TRUE)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- mean(stazione_j_lomb[seq(1,30)],na.rm = TRUE)
      else
        stazione_j_lomb[row_na_lomb[i]] <- stazione_j_lomb[row_na_lomb[i]-1]
    }
  }
  PM10_lomb_2018[,j] = stazione_j_lomb
}

write.table(PM10_lomb_2018, "matrixPM10_2018_noNa_sett.csv", sep = ";", dec = ",", row.names=FALSE)

############################
########### 2019 ###########
############################

#to start: go to file dataset.R and run the section 2019 to create the necessary files

data2019 <- read.csv("PM10_all_2019.csv", header=TRUE, dec = ',', sep = ';')
data2019$Date <- as.character.Date(data2019$Date)

#LOMBARDIA: creating time series for each station
PM10_2019 = data2019
PM10_short_2019= PM10_2019[,c(1,7,10,5)]
PM10_short_2019$NomeStazione = as.factor(PM10_short_2019$NomeStazione)

stazioni_lomb_2019 = as.factor(levels(PM10_short_2019$NomeStazione))
PM10_short_2019$Date =as.Date(PM10_short_2019$Date, format="%d/%m/%Y")
Data_lomb_2019 = unique(PM10_short_2019$Date)

PM10_lomb_2019 = as.data.frame(Data_lomb_2019)
colnames(PM10_lomb_2019) = c("Date")

for(i in 2:(length(stazioni_lomb_2019)+1)){
  stazione_i_lomb_2019 = PM10_short_2019[which(PM10_short_2019$NomeStazione==stazioni_lomb_2019[i-1]),c(1,2)]
  PM10_lomb_2019 = merge(PM10_lomb_2019,stazione_i_lomb_2019, by = "Date", all.x = TRUE)
}

colnames(PM10_lomb_2019) = c("Date",as.character(stazioni_lomb_2019))

#replace the -9999 with NAs
for(j in 2:(length(stazioni_lomb_2019)+1)){
  stazione_j_lomb = PM10_lomb_2019[,j]
  row_na_lomb = which(stazione_j_lomb==-9999)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- NA
      else
        stazione_j_lomb[row_na_lomb[i]] <- NA
    }
  }
  PM10_lomb_2019[,j] = stazione_j_lomb
}

PM10_lomb_2019[,1] = as.Date(PM10_lomb_2019[,1], format="%d/%m/%Y")
PM10_lomb_2019 = PM10_lomb_2019[which(PM10_lomb_2019$Date<'2019-09-01'),]
PM10_lomb_2019[,67] = as.numeric(as.Date(PM10_lomb_2019[,1], format="%d/%m/%Y"))-18627+122

write.table(PM10_lomb_2019, "matrixPM10_2019_sett.csv", sep = ";", dec = ",", row.names=FALSE)

#remove the stations where there are too many NAs (>=75)
colSums(is.na(PM10_lomb_2019))
tokeep_cols = colSums(is.na(PM10_lomb_2019))[which(colSums(is.na(PM10_lomb_2019))<75)]
PM10_lomb_2019 <- PM10_lomb_2019[,names(tokeep_cols)]

#replacing the remaining NAs with the previous value (mean of the month in the case 01/01/2019)
for(j in 2:(length(PM10_lomb_2019[1,])-1)){
  stazione_j_lomb = PM10_lomb_2019[,j]
  row_na_lomb = which(is.na(stazione_j_lomb)==TRUE)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- mean(stazione_j_lomb[seq(1,30)],na.rm = TRUE)
      else
        stazione_j_lomb[row_na_lomb[i]] <- stazione_j_lomb[row_na_lomb[i]-1]
    }
  }
  PM10_lomb_2019[,j] = stazione_j_lomb
}

write.table(PM10_lomb_2019, "matrixPM10_2019_noNa_sett.csv", sep = ";", dec = ",", row.names=FALSE)

