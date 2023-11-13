#loading necessary libraries
library(roahd)
library(DepthProc)
library(MASS)
library(rgl)
library(fda)

################################################################################
###################### CREATION MATRICES PM10 2020 - 2021 ######################
################################################################################

############################
########### 2020 ###########
############################

#to start: go to file dataset.R and run the section 2020 to create the necessary files

data2020 <- read.csv("PM10_all_2020.csv", header=TRUE, dec = ',', sep = ';')   # without 29/02/2020
data2020$Date <- as.character.Date(data2020$Date)

#LOMBARDIA: creating time series for each station
PM10_2020 = data2020
PM10_short_2020= PM10_2020[,c(1,7,10,5)]
PM10_short_2020$NomeStazione = as.factor(PM10_short_2020$NomeStazione)

stazioni_lomb_2020 = as.factor(levels(PM10_short_2020$NomeStazione))
PM10_short_2020$Date =as.Date(PM10_short_2020$Date, format="%d/%m/%Y")
Data_lomb_2020 = unique(PM10_short_2020$Date)

PM10_lomb_2020 = as.data.frame(Data_lomb_2020)
colnames(PM10_lomb_2020) = c("Date")

for(i in 2:(length(stazioni_lomb_2020)+1)){
  stazione_i_lomb_2020 = PM10_short_2020[which(PM10_short_2020$NomeStazione==stazioni_lomb_2020[i-1]),c(1,2)]
  PM10_lomb_2020 = merge(PM10_lomb_2020,stazione_i_lomb_2020, by = "Date", all.x = TRUE)
}

colnames(PM10_lomb_2020) = c("Date",as.character(stazioni_lomb_2020))

#replace the -9999 with NAs
for(j in 2:(length(stazioni_lomb_2020)+1)){
  stazione_j_lomb = PM10_lomb_2020[,j]
  row_na_lomb = which(stazione_j_lomb==-9999)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- NA
      else
        stazione_j_lomb[row_na_lomb[i]] <- NA
    }
  }
  PM10_lomb_2020[,j] = stazione_j_lomb
}

PM10_lomb_2020[,1] = as.Date(PM10_lomb_2020[,1], format="%d/%m/%Y")
PM10_lomb_2020 = PM10_lomb_2020[which(PM10_lomb_2020$Date>'2020-08-31'),]
PM10_lomb_2020[,66] = as.numeric(as.Date(PM10_lomb_2020[,1], format="%d/%m/%Y"))-18261-244

write.table(PM10_lomb_2020, "matrixPM10_2020_sett.csv", sep = ";", dec = ",", row.names=FALSE)

#remove the stations where there are too many NAs (>=75)
colSums(is.na(PM10_lomb_2020))
tokeep_cols = colSums(is.na(PM10_lomb_2020))[which(colSums(is.na(PM10_lomb_2020))<75)]
PM10_lomb_2020 <- PM10_lomb_2020[,names(tokeep_cols)]

#replacing the remaining NAs with the previous value (mean of the month in the case 01/01/2020)
for(j in 2:(length(PM10_lomb_2020[1,]))){
  stazione_j_lomb = PM10_lomb_2020[,j]
  row_na_lomb = which(is.na(stazione_j_lomb)==TRUE)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- mean(stazione_j_lomb[seq(1,30)],na.rm = TRUE)
      else
        stazione_j_lomb[row_na_lomb[i]] <- stazione_j_lomb[row_na_lomb[i]-1]
    }
  }
  PM10_lomb_2020[,j] = stazione_j_lomb
}

write.table(PM10_lomb_2020, "matrixPM10_2020_noNA_sett.csv", sep = ";", dec = ",", row.names=FALSE)

############################
########### 2021 ###########
############################

#to start: go to file dataset.R and run the section 2021 to create the necessary files

data2021 <- read.csv("PM10_all.csv", header=TRUE, dec = ',', sep = ';')
data2021$Date <- as.character.Date(data2021$Date)

#LOMBARDIA: creating time series for each station
PM10_2021 = data2021
PM10_short_2021= PM10_2021[,c(1,7,10,5)]
PM10_short_2021$NomeStazione = as.factor(PM10_short_2021$NomeStazione)

stazioni_lomb_2021 = as.factor(levels(PM10_short_2021$NomeStazione))
PM10_short_2021$Date =as.Date(PM10_short_2021$Date, format="%d/%m/%Y")
Data_lomb_2021 = unique(PM10_short_2021$Date)

PM10_lomb_2021 = as.data.frame(Data_lomb_2021)
colnames(PM10_lomb_2021) = c("Date")

for(i in 2:(length(stazioni_lomb_2021)+1)){
  stazione_i_lomb_2021 = PM10_short_2021[which(PM10_short_2021$NomeStazione==stazioni_lomb_2021[i-1]),c(1,2)]
  PM10_lomb_2021 = merge(PM10_lomb_2021,stazione_i_lomb_2021, by = "Date", all.x = TRUE)
}

colnames(PM10_lomb_2021) = c("Date",as.character(stazioni_lomb_2021))

#replace the -9999 with NAs
for(j in 2:(length(stazioni_lomb_2021)+1)){
  stazione_j_lomb = PM10_lomb_2021[,j]
  row_na_lomb = which(stazione_j_lomb==-9999)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- NA
      else
        stazione_j_lomb[row_na_lomb[i]] <- NA
    }
  }
  PM10_lomb_2021[,j] = stazione_j_lomb
}

PM10_lomb_2021[,1] = as.Date(PM10_lomb_2021[,1], format="%d/%m/%Y")
PM10_lomb_2021 = PM10_lomb_2021[which(PM10_lomb_2021$Date<'2021-09-01'),]
PM10_lomb_2021[,67] = as.numeric(as.Date(PM10_lomb_2021[,1], format="%d/%m/%Y"))-18627+122

write.table(PM10_lomb_2021, "matrixPM10_2021_sett.csv", sep = ";", dec = ",", row.names=FALSE)

#remove the stations where there are too many NAs (>=75)
colSums(is.na(PM10_lomb_2021))
tokeep_cols = colSums(is.na(PM10_lomb_2021))[which(colSums(is.na(PM10_lomb_2021))<75)]
PM10_lomb_2021 <- PM10_lomb_2021[,names(tokeep_cols)]

#replacing the remaining NAs with the previous value (mean of the month in the case 01/01/2021)
for(j in 2:(length(PM10_lomb_2021[1,])-1)){
  stazione_j_lomb = PM10_lomb_2021[,j]
  row_na_lomb = which(is.na(stazione_j_lomb)==TRUE)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- mean(stazione_j_lomb[seq(1,30)],na.rm = TRUE)
      else
        stazione_j_lomb[row_na_lomb[i]] <- stazione_j_lomb[row_na_lomb[i]-1]
    }
  }
  PM10_lomb_2021[,j] = stazione_j_lomb
}

write.table(PM10_lomb_2021, "matrixPM10_2021_noNA_sett.csv", sep = ";", dec = ",", row.names=FALSE)

