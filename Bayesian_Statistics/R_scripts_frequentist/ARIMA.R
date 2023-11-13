####################
### STAN ###
###################
### TIME SERIES ###
###################

library(tseries)
library(forecast)
library(rstan)
library(bmstdr)

### EMILIA ###

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

### LOMBARDIA ###
lomb <- read.csv("PM2.5Lombardia_new.csv", header=TRUE, dec = ',', sep = ';')
lomb_2018 = lomb[which(lomb$Anno==2018),c(2,3,13,14)]
colnames(lomb_2018) = c("Data","Valore","NS","Stazione")
lomb_2018$NS = as.factor(lomb_2018$NS)

stazioni_lomb = as.factor(levels(lomb_2018$NS))
Data_lomb = as.factor(levels(as.factor(lomb_2018$Data)))

new_data_lomb = as.data.frame(Data_lomb)
colnames(new_data_lomb) = c("Data")

for(i in 2:(length(stazioni_lomb)+1)){
  stazione_i_lomb = lomb_2018[which(lomb_2018$NS==stazioni_lomb[i-1]),c(1,2)]
  new_data_lomb = merge(new_data_lomb,stazione_i_lomb, by = "Data", all.x = TRUE)
}

colnames(new_data_lomb) = c("Date",as.character(stazioni_lomb))


# ARIMA SULLA MEDIA -> ARIMA(3,1,2)
data <- cbind(new_data_emil[-1],new_data_lomb[,-1])
mean_emil_lomb = rowMeans(as.matrix(data[,-1]), na.rm = TRUE)

library("BBmisc")
normalize(mean_emil_lomb, method="standardize")
serie_storica_emil_lomb <- ts(mean_emil_lomb)
# pulire ts
serie_storica_emil_lomb <- tsclean(serie_storica_emil_lomb)
for(j in 1:length(serie_storica_emil_lomb)){
  if(serie_storica_emil_lomb[j]==0){
    serie_storica_lomb[j]=1
  }
}
serie_storica_emil_lomb_log <- log(serie_storica_emil_lomb)
x11()
par(mfrow=c(2,1))
plot(serie_storica_emil_lomb, main="Mean", col="red2")
plot(serie_storica_emil_lomb_log, main= "Log(Mean)", col="blue")

acf(serie_storica_emil_lomb_log)
#stima stationarietà (H0=non stazionaria vs H1=stazionaria)
adf.test(serie_storica_emil_lomb_log, alternative = 'stationary')$p.value
# stimare parametri ARIMA
fit_emil_lomb <- auto.arima(serie_storica_emil_lomb_log, seasonal = TRUE)  # non serve rendere la serie stazionaria
summary(fit_emil_lomb)
autoplot(fit_emil_lomb) #plot of the inverse roots, they should all lie within the unit circle
acf(fit_emil_lomb$residuals)
pacf(fit_emil_lomb$residuals)
hist(fit_emil_lomb$residuals)
qqnorm(fit_emil_lomb$residuals)
qqline(fit_emil_lomb$residuals)
shapiro.test(fit_emil_lomb$residuals)
tsdiag(fit_emil_lomb)
checkresiduals(fit_emil_lomb)


## STAN
log_mean_emil_lomb=log(mean_emil_lomb)
dati <- list(
  y = log_mean_emil_lomb,
  num_giorni = 365,
  num_param_ar = 3,
  num_param_ma = 2
)

fit1 <- stan(
  file = "TS.stan",
  data = dati,
  chains = 4,
  warmup = 10000, 
  iter = 20000,
  cores = 4, 
  refresh = 0,
  control=list(max_treedepth=15) #to put when you have small dev. std im param
)

summary(fit1)
stan_dens(fit1, pars = c("auto_reg_1"), col="black", show_density=TRUE) #densità
stan_trace(fit1, pars = c("auto_reg_1")) #plot of 4 chains
stan_hist(fit1, pars = c("auto_reg_1")) #istogramma

stan_hist(fit1, pars = c("auto_reg_2"))
stan_hist(fit1, pars = c("auto_reg_3"))
stan_hist(fit1, pars = c("mov_avg_1"))
stan_hist(fit1, pars = c("mov_avg_2"))
stan_hist(fit1, pars = c("sigma"))

# con dev.stan = 2 ---->bene uguale--->cambiare max_treedepth
# con dev.stan = 10 ---->ok
# con dev.stan = 100 -----> bene

