#loading necessary libraries
library(roahd)
library(DepthProc)
library(MASS)
library(rgl)
library(fda)

################################################################################
##### Intersection between 2015-16 and 2020-21 to have the same stations #######
################################################################################

PM10_lomb_20_21 <- read.csv("matrixPM10_20_21_noNa_sett.csv", header=TRUE, dec = ',', sep = ';')

PM10_lomb_2015 <- read.csv("matrixPM10_2015_noNA_sett.csv", header=TRUE, dec = ',', sep = ';')
PM10_lomb_2015 <- PM10_lomb_2015[,-43]   # elimino la stazione in più
colnames(PM10_lomb_2015)[75] = c("Numeric Date")
PM10_lomb_2016 <- read.csv("matrixPM10_2016_noNA_sett.csv", header=TRUE, dec = ',', sep = ';')
PM10_lomb_2016 <- PM10_lomb_2016[,-c(32,39,45,70)]   # elimino le stazioni in più
colnames(PM10_lomb_2016)[75] = c("Numeric Date")
PM10_lomb_2016 <- PM10_lomb_2016[which(PM10_lomb_2016$Date!="2016-02-29"),]
PM10_lomb_15_16 <- rbind(PM10_lomb_2015,PM10_lomb_2016)
write.table(PM10_lomb_15_16, "matrixPM10_15_16_noNa_sett.csv", sep = ";", dec = ",", row.names=FALSE)

#keep the same stations for 2015-16 and 2020-21 (intersection)
vec_15_16=names(PM10_lomb_15_16)
vec_20_21=names(PM10_lomb_20_21)

inters=intersect(vec_15_16, vec_20_21)

PM10_lomb_15_16_inters=PM10_lomb_15_16[, inters]
PM10_lomb_20_21_inters=PM10_lomb_20_21[, inters]

################################################################################
############################## Permutational test ##############################
################################################################################

seed=2781991
B=1000 #how many times do I permute
library(fda)
library(roahd)

#smooth the curve 2015-16
nbasis=2
basis <- create.fourier.basis(rangeval=range(1:365), nbasis=nbasis)
x11()
quartz()
plot(basis)

num_staz_inters=length(PM10_lomb_15_16_inters[1,]) #60=59+colonna data
smooth_PM10_lomb_15_16_inters=PM10_lomb_15_16_inters

for (i in 2:num_staz_inters) {
    Xsp <- smooth.basis(argvals=1:365, y=PM10_lomb_15_16_inters[,i], fdParobj=basis)
    smooth_PM10_lomb_15_16_inters[,i] <- eval.fd(1:365, Xsp$fd) #  the curve smoothing the data
}
matplot(1:365,smooth_PM10_lomb_15_16_inters[,-1], type='l',col='blue') 

#smooth the curve 2020-21
basis <- create.fourier.basis(rangeval=range(1:365), nbasis=nbasis)
plot(basis)

num_staz_inters=length(PM10_lomb_20_21_inters[1,]) #60=59+colonna data
smooth_PM10_lomb_20_21_inters=PM10_lomb_20_21_inters

for (i in 2:num_staz_inters) {
  Xsp <- smooth.basis(argvals=1:365, y=PM10_lomb_20_21_inters[,i], fdParobj=basis)
  smooth_PM10_lomb_20_21_inters[,i] <- eval.fd(1:365, Xsp$fd) #  the curve smoothing the data
}
matplot(1:365,smooth_PM10_lomb_20_21_inters[,-1], type='l',col='blue') 

#plot of the curves
x11()
quartz()
matplot(1:365,PM10_lomb_15_16_inters[,-1], type='l',col='olivedrab2', lwd = 1, main="PM10 level in 2015-16 and 2020-21", xlab='Days', ylab='PM10 level')
matlines(1:365,PM10_lomb_20_21_inters[,-1], type='l',col='darkolivegreen', lwd=1)
legend(x='topright',legend = c('2015-16', '2020-21'), col = c('olivedrab2', 'darkolivegreen'), lwd='2')

#plot of the smooth curves
x11()
quartz()
matplot(1:365,smooth_PM10_lomb_15_16_inters[,-1], type='l',col='olivedrab2', lwd = 1, main="Smoothed PM10 level in 2015-16 and 2020-21", xlab='Days', ylab='PM10 level')
matlines(1:365,smooth_PM10_lomb_20_21_inters[,-1], type='l',col='darkolivegreen', lwd=1)
legend(x='topright',legend = c('2015-16', '2020-21'), col = c('olivedrab2', 'darkolivegreen'), lwd='2')

############################### Smoothed curves ################################
###### test: H0: the 2 curves have same distr, H1: the 2 curves have not #######

smooth_PM10_bind=rbind(t(smooth_PM10_lomb_15_16_inters[,-1]),t(smooth_PM10_lomb_20_21_inters[, -1]))
n=nrow(smooth_PM10_bind)
n_15_16=nrow(t(smooth_PM10_lomb_15_16_inters[,-1]))
n_20_21=nrow(t(smooth_PM10_lomb_20_21_inters[,-1]))

meandiff=(colMeans(t(smooth_PM10_lomb_20_21_inters[,-1]))-colMeans(t(smooth_PM10_lomb_15_16_inters[,-1]))) 

x11()
quartz()
matplot(1:365, colMeans(t(smooth_PM10_lomb_20_21_inters[,-1])) , type='l',col='blue') 
matlines(1:365,colMeans(t(smooth_PM10_lomb_15_16_inters[,-1])), type='l',col='red')

x11()
quartz()
plot(meandiff,type = 'l')

#different test statistics
T0_norm2=sum(meandiff^2) #norm2
T0_norm1= sum(abs(meandiff)) #norm1

T0_perm_norm1=numeric(B)
T0_perm_norm2=numeric(B)

for(perm in 1:B){
  permutazione <- sample(n) #vector of elements from 1 to n, permuted each time
  smooth_PM10_bind_perm=smooth_PM10_bind[permutazione,] #recreate the matrix with the permuted rows (= stations)
  perm_15_16 = smooth_PM10_bind_perm[1:n_15_16,] #first nm rows
  perm_20_21 = smooth_PM10_bind_perm[(n_15_16+1):n,] #last nf rows
  T0_perm_norm2[perm]=sum(((colMeans(perm_15_16)-colMeans(perm_20_21)))^2) #vector containing all   the new B T0
  T0_perm_norm1[perm]=sum(abs((colMeans(perm_15_16)-colMeans(perm_20_21))))
}

#norm 2
x11()
quartz()
hist(T0_perm_norm2, xlim = c(0, 10000), ylim = c(0,400), main = "Histogram norm 2", xlab= "T0=norm2", breaks=30)
abline(v=T0_norm2,col='olivedrab2', lw=2)
sum(T0_perm_norm2 >= T0_norm2)/B #p-value

#norm 1
x11()
quartz()
hist(T0_perm_norm1, xlim = c(0, 2000), ylim = c(0,140), main = "Histogram norm 1", xlab= "T0=norm1", breaks=30)
abline(v=T0_norm1,col='olivedrab2', lw=2)
sum(T0_perm_norm1 >= T0_norm1)/B #p-value

#pointwise p-value: 

library(devtools)
#devtools::install_github("alessiapini/fdatest")
library(fdatest)
tst2=IWT2(t(smooth_PM10_lomb_15_16_inters[,-1]),t(smooth_PM10_lomb_20_21_inters[, -1]))
x11()
quartz()
plot(tst2, col=c("olivedrab2", "darkolivegreen"), main="Piecewise significance 2015-16 and 2020-21")


#############################  Non smoothed curves #############################
###### test: H0: the 2 curves have same distr, H1: the 2 curves have not #######

PM10_bind=rbind(t(PM10_lomb_15_16_inters[,-1]),t(PM10_lomb_20_21_inters[, -1]))
n=nrow(PM10_bind)
n_15_16=nrow(t(PM10_lomb_15_16_inters[,-1]))
n_20_21=nrow(t(PM10_lomb_20_21_inters[,-1]))

meandiff=(colMeans(t(PM10_lomb_20_21_inters[,-1]))-colMeans(t(PM10_lomb_15_16_inters[,-1]))) 

x11()
quartz()
matplot(1:365, colMeans(t(PM10_lomb_20_21_inters[,-1])) , type='l',col='blue') 
matlines(1:365,colMeans(t(PM10_lomb_15_16_inters[,-1])), type='l',col='red')

x11()
quartz()
plot(meandiff,type = 'l')

#different test statistics
T0_norm2=sum(meandiff^2) #norm2
T0_norm1= sum(abs(meandiff)) #norm1

T0_perm_norm1=numeric(B)
T0_perm_norm2=numeric(B)

for(perm in 1:B){
  permutazione <- sample(n) #vector of elements from 1 to n, permuted each time
  PM10_bind_perm=PM10_bind[permutazione,] #recreate the matrix with the permuted rows (= stations)
  perm_15_16 = PM10_bind_perm[1:n_15_16,] #first nm rows
  perm_20_21 = PM10_bind_perm[(n_15_16+1):n,] #last nf rows
  T0_perm_norm2[perm]=sum(((colMeans(perm_15_16)-colMeans(perm_20_21)))^2) #vector containing all   the new B T0
  T0_perm_norm1[perm]=sum(abs((colMeans(perm_15_16)-colMeans(perm_20_21))))
}

#norm 2
x11()
quartz()
hist(T0_perm_norm2, xlim = c(0, 150000), ylim = c(0,500), main = "Histogram norm 2", xlab= "T0=norm2", breaks=10)
abline(v=T0_norm2,col='olivedrab2', lw=2)
sum(T0_perm_norm2 >= T0_norm2)/B #p-value

#norm 1
x11()
quartz()
hist(T0_perm_norm1, xlim = c(0, 6000), main = "Histogram norm 1", xlab= "T0=norm1", breaks=10)
abline(v=T0_norm1,col='olivedrab2', lw=2)
sum(T0_perm_norm1 >= T0_norm1)/B #p-value

#pointwise p-value: 

library(devtools)
#devtools::install_github("alessiapini/fdatest")
library(fdatest)
tst2=IWT2(t(PM10_lomb_15_16_inters[,-1]),t(PM10_lomb_20_21_inters[, -1]))
x11()
quartz()
plot(tst2, col=c("olivedrab2", "darkolivegreen"), main="Piecewise significance 2015-16 and 2020-21")


########################### Functional bootstrap ######################################

library(fda.usc)

#make the data functional
PM10_15_16_func = fData( seq(1,365),t(PM10_lomb_15_16_inters[,-1]))
PM10_20_21_func = fData( seq(1,365),t(PM10_lomb_20_21_inters[,-1]))

#functional boostrap
#how it works: for each istant, take the data of the 32 stations, draw a distribution, then samples again from that distribuition nb=200 times, 
#compute the means of those 200 values. does this for each time

#output: boot_2016$statistics: time series computed on the resampled values, foe each t
boot_15_16=fdata.bootstrap(PM10_15_16_func$values, statistic=func.mean, alpha=0.05, nb=200, smo=0)
boot_20_21=fdata.bootstrap(PM10_20_21_func$values, statistic=func.mean, alpha=0.05, nb=200, smo=0)

#plot of the 2 test statistics (means)
x11()
quartz()
plot(boot_15_16$statistic, ylim=c(0,120), col="blue")
par(new=TRUE)
plot(boot_20_21$statistic, ylim=c(0,120), col="red")

#mean of the 2 curves "statistics means"
mean_15_16=rowMeans(boot_15_16$statistic$data)
mean_20_21=rowMeans(boot_20_21$statistic$data)

#hypothesis test
B=10000
p.value <- numeric(B)
#pb=progress_bar$new(total=B)
#pb$tick(0)
set.seed(seed)
for(j in 1:B){
  x1 = rowMeans(boot_15_16$statistic$data) # mean_15_16
  x2 = rowMeans(boot_20_21$statistic$data) # mean_20_21
  p.value[j] <- t.test(x1,y=x2)$p.value
  #pb$tick()
}

#t-test: H0=: mu_15_16-mu_20_21=0, vs H1:...
t.test(boot_15_16$statistic$data-boot_20_21$statistic$data, mu=0)
#It returns a pvalue: we do not refuse H0 for tests with alpha<pvalue%





