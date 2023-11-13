#loading necessary libraries
library(roahd)
library(DepthProc)
library(MASS)
library(rgl)
library(fda)

################################################################################
###################### CREATION MATRICES RAIN 2020 & 2021 ######################
################################################################################

############################
########### 2020 ###########
############################

#to start: go to file dataset.R and run the section 2020 to create the necessary files

data2020 <- read.csv("PM10_all_2020.csv", header=TRUE, dec = ',', sep = ';')
data2020$Date <- as.character.Date(data2020$Date)

#LOMBARDIA: creating time series for each station
rain_2020 = data2020
rain_short_2020= rain_2020[,c(1,19,10,5)]
rain_short_2020$NomeStazione = as.factor(rain_short_2020$NomeStazione)

stazioni_lomb_2020 = as.factor(levels(rain_short_2020$NomeStazione))
rain_short_2020$Date =as.Date(rain_short_2020$Date, format="%d/%m/%Y")
Data_lomb_2020 = unique(rain_short_2020$Date)

rain_lomb_2020 = as.data.frame(Data_lomb_2020)
colnames(rain_lomb_2020) = c("Date")

for(i in 2:(length(stazioni_lomb_2020)+1)){
  stazione_i_lomb_2020 = rain_short_2020[which(rain_short_2020$NomeStazione==stazioni_lomb_2020[i-1]),c(1,2)]
  rain_lomb_2020 = merge(rain_lomb_2020,stazione_i_lomb_2020, by = "Date", all.x = TRUE)
}

colnames(rain_lomb_2020) = c("Date",as.character(stazioni_lomb_2020))

rain_lomb_2020 = rain_lomb_2020[which(rain_lomb_2020$Date!="2020-02-29"),] # 2020 is a leap year

#replace wrong values (<0) with NAs
for(j in 2:(length(stazioni_lomb_2020)+1)){
  stazione_j_lomb = rain_lomb_2020[,j]
  row_na_lomb = which(stazione_j_lomb<0)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- NA
      else
        stazione_j_lomb[row_na_lomb[i]] <- NA
    }
  }
  rain_lomb_2020[,j] = stazione_j_lomb
}

rain_lomb_2020[,1] = as.Date(rain_lomb_2020[,1], format="%d/%m/%Y")
rain_lomb_2020 = rain_lomb_2020[which(rain_lomb_2020$Date>'2020-08-31'),]
rain_lomb_2020[,66] = as.numeric(as.Date(rain_lomb_2020[,1], format="%d/%m/%Y"))-18261-244

write.table(rain_lomb_2020, "matrixrain_2020_sett.csv", sep = ";", dec = ",", row.names=FALSE)

#remove the stations where there are too many NAs (>=75)
colSums(is.na(rain_lomb_2020))
tokeep_cols = colSums(is.na(rain_lomb_2020))[which(colSums(is.na(rain_lomb_2020))<75)]
rain_lomb_2020 <- rain_lomb_2020[,names(tokeep_cols)]

rm(stazione_j_lomb)
rm(row_na_lomb)
for(j in 2:(length(rain_lomb_2020[1,])-1)){
  stazione_j_lomb = rain_lomb_2020[,j]
  row_na_lomb = which(is.na(stazione_j_lomb)==TRUE)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- 0
      else
        stazione_j_lomb[row_na_lomb[i]] <- 0
    }
  }
  rain_lomb_2020[,j] = stazione_j_lomb
}

write.table(rain_lomb_2020, "matrixrain_2020_noNa_sett.csv", sep = ";", dec = ",", row.names=FALSE)

############################
########### 2021 ###########
############################

#to start: go to file dataset.R and run the section 2021 to create the necessary files

data2021 <- read.csv("PM10_all.csv", header=TRUE, dec = ',', sep = ';')
data2021$Date <- as.character.Date(data2021$Date)

#LOMBARDIA: creating time series for each station
rain_2021 = data2021
rain_short_2021= rain_2021[,c(1,19,10,5)]
rain_short_2021$NomeStazione = as.factor(rain_short_2021$NomeStazione)

stazioni_lomb_2021 = as.factor(levels(rain_short_2021$NomeStazione))
rain_short_2021$Date =as.Date(rain_short_2021$Date, format="%d/%m/%Y")
Data_lomb_2021 = unique(rain_short_2021$Date)

rain_lomb_2021 = as.data.frame(Data_lomb_2021)
colnames(rain_lomb_2021) = c("Date")

for(i in 2:(length(stazioni_lomb_2021)+1)){
  stazione_i_lomb_2021 = rain_short_2021[which(rain_short_2021$NomeStazione==stazioni_lomb_2021[i-1]),c(1,2)]
  rain_lomb_2021 = merge(rain_lomb_2021,stazione_i_lomb_2021, by = "Date", all.x = TRUE)
}

colnames(rain_lomb_2021) = c("Date",as.character(stazioni_lomb_2021))

#replace wrong values (<0) with NAs
for(j in 2:(length(stazioni_lomb_2021)+1)){
  stazione_j_lomb = rain_lomb_2021[,j]
  row_na_lomb = which(stazione_j_lomb<0)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- NA
      else
        stazione_j_lomb[row_na_lomb[i]] <- NA
    }
  }
  rain_lomb_2021[,j] = stazione_j_lomb
}

rain_lomb_2021[,1] = as.Date(rain_lomb_2021[,1], format="%d/%m/%Y")
rain_lomb_2021 = rain_lomb_2021[which(rain_lomb_2021$Date<'2021-09-01'),]
rain_lomb_2021[,67] = as.numeric(as.Date(rain_lomb_2021[,1], format="%d/%m/%Y"))-18627+122

write.table(rain_lomb_2021, "matrixrain_2021_sett.csv", sep = ";", dec = ",", row.names=FALSE)

#remove the stations where there are too many NAs (>=75)
colSums(is.na(rain_lomb_2021))
tokeep_cols = colSums(is.na(rain_lomb_2021))[which(colSums(is.na(rain_lomb_2021))<75)]
rain_lomb_2021 <- rain_lomb_2021[,names(tokeep_cols)]

rm(stazione_j_lomb)
rm(row_na_lomb)
for(j in 2:(length(rain_lomb_2021[1,])-1)){
  stazione_j_lomb = rain_lomb_2021[,j]
  row_na_lomb = which(is.na(stazione_j_lomb)==TRUE)
  if (length(row_na_lomb) >=1){
    for(i in 1:length(row_na_lomb)) {
      if(row_na_lomb[i]==1)
        stazione_j_lomb[row_na_lomb[i]] <- 0
      else
        stazione_j_lomb[row_na_lomb[i]] <- 0
    }
  }
  rain_lomb_2021[,j] = stazione_j_lomb
}

write.table(rain_lomb_2021, "matrixrain_2021_noNa_sett.csv", sep = ";", dec = ",", row.names=FALSE)

