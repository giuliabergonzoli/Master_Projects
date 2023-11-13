library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(fda)

####### Conformal ####### CITTA' STUDI

library(conformalInference)

train_gam=function(x,y,out=NULL){
  colnames(x)=c('val_temp','Date','Quota','rain_binary','val_wind','long','lat')
  train_data=data.frame(y,x)
  train_data[,5]=as.factor(train_data[,5])
  model_gam=gam(y ~ s(val_temp) + s(Date,bs='cc') + 
                  Quota + rain_binary + val_wind + s(long,lat), data=train_data)
}

predict_gam=function(obj, new_x){
  new_x=data.frame(new_x)
  colnames(new_x)=c('val_temp','Date','Quota','rain_binary','val_wind','long','lat')
  new_x[,4]=as.factor(new_x[,4])
  predict.gam(obj,new_x)
}

data_pred <- read.csv("data_pred2021.csv", header=TRUE, dec = ',', sep = ';')

Y_PM102021 = data_pred[,7]  # extract vector of all Pm10 values ordered by date
Y_PM102021 = replace(Y_PM102021, Y_PM102021==0, 1)
Y_PM102021_log = log(Y_PM102021)

rain2021 = as.numeric(data_pred[,19])
#rain as a binary variable: 1 rain, 0 no rain
rain_binary2021=rain2021
rain_binary2021[which(rain_binary2021>0)]=1

data_try = as.data.frame(cbind(Y_PM102021_log,data_pred[,18],as.numeric(data_pred[,23]),as.numeric(data_pred[,11]),
                               rain_binary2021,as.numeric(data_pred[,20]),data_pred[,17],data_pred[,16],data_pred[,5]))
names(data_try) = c('Y_PM10_log','val_temp','Date','Quota','rain_binary','val_wind','long','lat','idstaz')


data_train=data_try[which(data_try$idstaz!=705),-9]
data_new = data_try[which(data_try$idstaz==705),-9] #città studi
data_conf=as.matrix(data_new[,-1])

c_preds=conformal.pred(data_train[,-1],data_train[,1],
                       data_conf,alpha=0.05,
                       verbose=T,
                       train.fun = train_gam ,
                       predict.fun = predict_gam)



c_preds
save(c_preds, file="c_preds_poli.rda")
load('c_preds_poli.rda')

plot(data_new[,3],data_new[,1],type='l',lty=3,ylim = c(1,5.5), col =" black ", main='Conformal prediction',xlab='Days',ylab='PM10 level')
polygon(c(data_conf[,2],rev(data_conf[,2])),c(c_preds$up,rev(c_preds$lo)), col = "lightcyan", border = 4)
lines(data_new[,3],data_new[,1],lty=3,ylim = c(1,5.5), col =" black ", main='GAM')
lines(data_conf[,2],c_preds$pred,type='l',ylim = c(1,5.5), col ="red")
#matlines(data_conf[,2] ,cbind(c_preds$up,c_preds$lo),ylim = c(1,5.5),lwd =1, col ="lightblue",lty =3)
legend(x='topright',legend = c('True values','Pointwise prediction','Conformal bands'), col = c('black','red','dodgerblue'), cex=.5, lwd='2')
