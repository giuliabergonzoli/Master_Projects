emil <- read.csv("PM2.5Emilia.csv", header=TRUE, dec = ',', sep = ';')
emil_2018 = emil[which(emil$Anno==2018),c(5,2,6)]
colnames(emil_2018) = c("Data","NS","Valore")
emil_2018$NS = as.factor(emil_2018$NS)
emil_2018$Data = as.factor(emil_2018$Data)

stazioni_emil = as.factor(levels(emil_2018$NS))
Data = as.factor(levels(emil_2018$Data))

new_data_emil = as.data.frame(Data)

for(i in 2:(length(stazioni_emil)+1)){
  stazione_i_emil = emil_2018[which(emil_2018$NS==stazioni_emil[i-1]),c(1,3)]
  new_data_emil = merge(new_data_emil,stazione_i_emil, by = "Data", all.x = TRUE)
}

colnames(new_data_emil) = c("Date",as.character(stazioni_emil))

#fix dates
count <- seq(1:364)
new_data_emil <- cbind(new_data_emil,count)

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:363)
new_col <- cbind(new_col,count)
colnames(new_col)[1] = c("Data")

new_data_emil = merge(new_data_emil,new_col, by = "count", all.x = TRUE)
new_data_emil <- new_data_emil[,-1]

data_iniziale <- as.Date("2018-01-01")
new_col <- as.data.frame(data_iniziale + 0:364)
colnames(new_col)[1] = c("Data")

new_data_emil = merge(new_data_emil,new_col, by = "Data", all = TRUE)
new_data_emil <- new_data_emil[,-2]

mean_stations_emil = colMeans(as.matrix(new_data_emil[,-1]), na.rm = TRUE)

#replace the NAs
for(j in 2:(length(stazioni_emil)+1)){
  stazione_j = new_data_emil[,j]
  rows_na = which(is.na(stazione_j)==TRUE)
  for (i in 1:length(rows_na)) {
    if(rows_na[i] == 1){
      stazione_j[rows_na[i]] <- mean_stations_emil[j-1]
    }
    else
      stazione_j[rows_na[i]] <- stazione_j[rows_na[i]-1]
  }
  new_data_emil[,j] = stazione_j
}


#daily average for each row
means_emil = rowMeans(as.matrix(new_data_emil[,-1]), na.rm = TRUE)
new_data_emil <- data.frame(new_data_emil, means_emil)

for(i in 2:32){
  
  for(j in 1:365){
    
    if (new_data_emil[j,i]<0)
      new_data_emil[j,i]=1
  }
}

new_data_emil[,2:32] <- log(new_data_emil[,2:32])

library(fda)

#data exploration with frequentist approach using only the mean values
NT <- dim(new_data_emil)[1] #numero giorni
abscissa <- 1:365
Xobs0 <- new_data_emil$means_emil

plot(abscissa,Xobs0, type = "l")

# generalized cross-validation
nbasis <- seq(2,50,by=2) 
gcv <- numeric(length(nbasis))
dfs <- numeric(length(nbasis))
SSEs <- numeric(length(nbasis))

for (i in 1:length(nbasis)){
  basis <- create.fourier.basis(range(abscissa), nbasis[i]) 
  dfs[i] <- smooth.basis(abscissa, Xobs0, basis)$df
  SSEs[i] <- smooth.basis(abscissa, Xobs0, basis)$SSE
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv #gcv= 365*SSE/(365-df)^2
}

par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v=nbasis[which.min(gcv)],col='red')

basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis[which.min(gcv)])
plot(basis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis_gcv <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis_gcv ,type="l",col="red",lwd=2) #clear overfitting

#choose a lower number of basis
#6
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=6)
plot(basis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis_6 <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis_6 ,type="l",col="red",lwd=2) #oversmoothing

#12
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=12)
plot(basis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis_12 <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis_12 ,type="l",col="red",lwd=2)
