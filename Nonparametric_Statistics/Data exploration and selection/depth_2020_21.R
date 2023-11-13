#loading necessary libraries
library(roahd)
library(DepthProc)
library(MASS)
library(rgl)
library(fda)

################################################################################
################################ DEPTH MEASURES ################################
################################################################################

PM10_lomb_20_21 <- read.csv("matrixPM10_20_21_noNA_sett.csv", header=TRUE, dec = ',', sep = ';')

PM10_mat = data.matrix(PM10_lomb_20_21)
PM10_mat = PM10_mat[,-c(1,66)]
PM10_func = fData( seq(1,365),t(PM10_mat))
x11()
quartz()
plot(PM10_func,lwd=1)

# finding the median

band_depth = BD(PM10_func)
modified_band_depth = MBD(PM10_func)

median_PM10 = median_fData(fData = PM10_func, type="MBD")

x11()
quartz()
plot(PM10_func,main="PM10 stations")
lines(seq(1,365),median_PM10$values,lwd=1)
legend(x='topright',legend = c('Median'), col=c("black"), lwd=1)

# finding outliers

x11()
quartz()
plot_mag = roahd::fbplot(PM10_func, main= "Magnitude outliers") # this gives 8 magnitude outliers
outliergram(PM10_func) #gives 1 shape outlier

id_out = plot_mag$ID_outliers

par(mfrow=c(2,4))
#plot(PM10_func[-id_out])
for (i in 1:length(id_out)){
  plot(PM10_func, xlim=c(0,365), ylim=c(0,150))
  par(new=TRUE)
  plot(PM10_func[id_out[i]],col="black", xlim=c(0,365), ylim=c(0,150),
       lwd=1, main=names(PM10_lomb_20_21)[id_out[i]+1], xlab="Days", ylab="PM10 level")
}




