library(mvnormtest)
library(MASS)
load('mcshapiro.test.RData')

#Import datasets
setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features")
ITA <- read.table(file = "student_ita.txt", header = T)
AUT <- read.table(file = "student_aut.txt", header = T)
BEL <- read.table(file = "student_bel.txt", header = T)
DNK <- read.table(file = "student_dnk.txt", header = T)
DEU <- read.table(file = "student_deu.txt", header = T)
LUX <- read.table(file = "student_lux.txt", header = T)
ESP <- read.table(file = "student_esp.txt", header = T)
SWE <- read.table(file = "student_swe.txt", header = T)
CHE <- read.table(file = "student_che.txt", header = T)
GBR <- read.table(file = "student_gbr.txt", header = T)
EUR <- read.table(file = "student_eur.txt", header = T)

countries <- list(AUT, BEL, CHE, DEU, DNK, ESP, GBR, ITA, LUX, SWE, EUR)
name_countries <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "GBR", "ITA", "LUX", "SWE", "EUR")
n_countries <- length(countries) 

variables <- c("CULTURAL POSSESSIONS", "FAMILY WEALTH", "ESCS STATUS", 
               "EDUCATIONAL RESOURCES", "HOME POSSESSIONS")

m_scores <- rep(0, n_countries)
s_scores <- rep(0, n_countries)
r_scores <- rep(0, n_countries)

mean_matrix <- as.data.frame(matrix(ncol = 11, nrow = 4),row.names = c("math_native","math_immig",
                                                                       "read_native","read_immig"))
names(mean_matrix) <- name_countries

for (i in 1:n_countries) {
  
  # BUILD DATAFRAMES ----
  
  X = data.frame(countries[[i]]$math, countries[[i]]$scie,  countries[[i]]$read)
  names(X) = c("math","scie", "read")
  immStatus = countries[[i]]$immigration
  
  ## Extract Indeces
  nativeIndex = which(immStatus==1)
  n_native = length(nativeIndex)
  
  immIndex= which(immStatus==2 + I(immStatus ==3))
  nImm = length(immIndex)
  
  ## Build dataframe
  X_native = X[nativeIndex,]
  X_imm = X[-nativeIndex,]
  
  print("________________________________________________________________________________________________")
  print(paste("                                            ", name_countries[i]))
  
  
  # HYPOTHESIS CHECK ---- 
  # p_native = mcshapiro.test(X_native[sample(min(300,n_native)),])$p
  # p_imm1 = mcshapiro.test(X_imm1)$p
  # p_imm2 = mcshapiro.test(X_imm2)$p
  # 
  # if(p_native<0.05 || p_imm1<0.05 || p_imm2<0.05)
  #   print("normality not guarenteed")
  # #check homogeneity assumptions
  
  # MANOVA ----
  n1 <- dim(X_native)[1]
  n2 <- dim(X_imm)[1]
  n  <- n1+n2
  
  g  <- 2
  p  <- dim(X)[2]
  
  countries[[i]]$immigration <- ifelse(countries[[i]]$immigration == 1, 1, 2)
  immig.status <- factor(countries[[i]]$immigration, labels=c('native','immig'))
  
  fit <- manova(as.matrix(X) ~ immig.status)
  p_val = summary.manova(fit)$stats[1,6]
  print(paste("Manova pvalue = ", p_val))
  
  
  alpha <- 0.05
  k <- p*g*(g-1)/2
  qT <- qt(1-alpha/(2*k), n-g)
  
  W <- summary.manova(fit)$SS$Residuals
  m  <- sapply(X,mean)         # estimates mu
  m1 <- sapply(X[nativeIndex,],mean)    # estimates mu.1=mu+tau.1 
  m2 <- sapply(X_imm, mean)
  mean_matrix[1,i] <- m1[1]
  mean_matrix[2,i] <- m2[1]
  mean_matrix[3,i] <- m1[3]
  mean_matrix[4,i] <- m2[3]
  
  #Intervals for native vs immig
  inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
  sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
  pval12 <- (1- pt(abs(m1-m2)/sqrt( diag(W)/(n-g) * (1/n1+1/n2) ),n-g))*(2*k)

  
  # PRINT RESULTS ----
  
  
  ############## MATH
  print("MATH")
  if(inf12[1]>0 || sup12[1]<0){
    m_scores[i] = (m1[1] - m2[1])/m1[1]
    print("Native - 1� Generation Immigrants")
    print(paste("Native mean = ", m1[1]))
    print(paste("Immig1 mean = ", m2[1]))
    print(paste("p-value: ", pval12[1]))
  }
  

  ############## SCIENCE
  print("SCIENCE")
  if(inf12[2]>0 || sup12[2]<0){
    s_scores[i] = (m1[2] - m2[2])/m1[2]
    print("Native - 1� Generation Immigrants")
    print(paste("Native mean = ", m1[2]))
    print(paste("Immig1 mean = ", m2[2]))
    print(paste("p-value: ", pval12[2]))
  }
  
  
  ############## READING
  
  print("READING")
  if(inf12[3]>0 || sup12[3]<0){
    r_scores[i] = (m1[3] - m2[3])/m1[3]
    print("Native - 1� Generation Immigrants")
    print(paste("Native mean = ", m1[3]))
    print(paste("Immig1 mean = ", m2[3]))
    print(paste("p-value: ", pval12[3]))
  }
  
}

m_order = order(m_scores) # dovremmo normalizzare in modo da avere una differenza percentuale?
s_order = order(s_scores)
r_order = order(r_scores)


m_rank = name_countries[m_order]
s_rank = name_countries[s_order]
r_rank = name_countries[r_order]

# GRAPHICAL VISUALIZATION ----
#Plot math:
x11(width = 18, height = 15)
par(mfrow = c(1,1),mar = c(5, 4, 4, 4) + 0.3)
#Barplot
bp <- barplot(sort(m_scores)[-11], names.arg = m_rank[-11], xlab ="Countries",
        ylab = "Native vs IMM", main = "MATH", col = "firebrick3")
yl <- 0.10
barplot(sort(m_scores)[-11], names.arg = m_rank[-11], col = "coral1",main = "Math - Native and Immigrant", 
        border = "NA",ylim = c(0,0.12), ylab = "Mean % decrease in score of immig students", cex.names = 0.9)
#Points
math_native <- t(mean_matrix[1,m_order])[-11]
math_native_adapted <- (math_native-450)/100*yl
math_immig <- t(mean_matrix[2,m_order])[-11]
math_immig_adapted <- (math_immig-450)/100*yl

points(bp,math_native_adapted,  pch=15, cex= 1.5, col="red3")
points(bp,math_native_adapted,  pch=0, cex= 1.5)
axis(side=4, at = seq(0,100, by=20)/100*yl, labels =seq(450,550,by=20), las=1)
mtext("Scores", side=4, line=3)
points(bp,math_immig_adapted, pch=16, cex =1.5, col="yellow")
points(bp,math_immig_adapted, pch=1, cex =1.5)
legend("top", 
       pch = c(15,16),
       legend = c("Native", "Immigrant"), 
       col = c("red3", "yellow"))
segments(bp, math_native_adapted, x1 = bp, y1 = math_immig_adapted, lty="dotted", col="gray32")

#READING
#Plot read:
x11(width = 18, height = 15)
par(mfrow = c(1,1),mar = c(5, 4, 4, 4) + 0.3)
#Barplot
bp <- barplot(sort(r_scores)[-11])
yl <- 0.10
barplot(sort(r_scores)[-11], names.arg = r_rank[-11], col = "lightskyblue",main = "Reading - Native and Immigrant", 
        border = "NA", ylab = "Mean % decrease in score of immig students", cex.names = 0.9)
#Points
read_native <- t(mean_matrix[3,r_order])[-11]
read_native_adapted <- (read_native-450)/100*yl
read_immig <- t(mean_matrix[4,r_order])[-11]
read_immig_adapted <- (read_immig-450)/100*yl

points(bp,read_native_adapted,  pch=15, cex= 1.5, col="dodgerblue3")
points(bp,read_native_adapted,  pch=0, cex= 1.5)
axis(side=4, at = seq(0,100, by=20)/100*yl, labels =seq(450,550,by=20), las=1)
mtext("Scores", side=4, line=3)
points(bp,read_immig_adapted, pch=16, cex =1.5, col="aliceblue")
points(bp,read_immig_adapted, pch=1, cex =1.5)
legend("top", 
       pch = c(15,16),
       legend = c("Native", "Immigrant"), 
       col = c("dodgerblue3", "aliceblue"))
segments(bp, read_native_adapted, x1 = bp, y1 = read_immig_adapted, lty="dotted", col="gray32")



ymax <- max(m_scores) * 1.05
par(mar=c(4.1,5.1,2.1,5.1))
bp <- barplot(sort(m_scores), col = "red", border = "NA", ylab = "Native - Immig scores")

barplot(sort(m_scores), col = "red", border = "NA", ylab = "Native - Immig scores")

abline(v = bp, col = "red", lwd = 2.5)
points(bp, t(mean_matrix[1,m_order])/10000, col = "blue", lwd = 2)
points(bp, df$counts/ymax*100, pch = 19, cex = 1.5)
axis(4,at=c(0,20,40,60,80,100), labels=c("0","100","200","300","400","500"))
mtext("Number of switching operations", side = 4, line = 3, font = 2)
axis(1, at=bp, labels=df$date)


# barplot(sort(s_scores), names.arg = s_rank, xlab ="Countries",
#         ylab = "Native vs IMM", main = "SCIENCE", col = "blue")
barplot(sort(r_scores), names.arg = r_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "READING", col = "blue")

x11(width = 18, height = 15)
par(mfrow = c(2,1))
points(t(mean_matrix[1,]), pch=0, col="red", ylim=c(450,550), xaxt="n")
axis(1,                         # Define x-axis manually
     at = 1:11,
     labels = name_countries)
points(t(mean_matrix[2,]),pch=1)
barplot(sort(m_scores), names.arg = m_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "MATH", col = "red")
# barplot(sort(s_scores), names.arg = s_rank, xlab ="Countries",
#         ylab = "Native vs IMM", main = "SCIENCE", col = "blue")
barplot(sort(r_scores), names.arg = r_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "READING", col = "blue")

## GRAPH WAS SAVED AS : barplot_performances_countries



  
 #  print("Math:")
 #  if(inf12[1]<0 && sup12[1]>0)
 #    print(paste("Native vs immig1 NO significative difference, pval = ",pval12[1]))
 #  if(inf13[1]<0 && sup13[1]>0)
 #    print(paste("Native vs immig2 NO significative difference, pval = ",pval13[1]))
 #  if(inf23[1]<0 && sup23[1]>0)
 #    print(paste("Immig1 vs immig2 NO significative difference, pval = ",pval23[1]))
 #  
 #  print("Science:")
 #  if(inf12[2]<0 && sup12[2]>0)
 #    print(paste("Native vs immig1 not significative, pval = ",pval12[2]))
 #  if(inf13[2]<0 && sup13[2]>0)
 #    print(paste("Native vs immig2 not significative, pval = ",pval13[2]))
 #  if(inf23[2]<0 && sup23[2]>0)
 #    print(paste("Immig1 vs immig2 not significative, pval = ",pval23[2]))
 #  
 #  print("Reading:")
 #  if(inf12[3]<0 && sup12[3]>0)
 #    print(paste("Native vs immig1 not significative, pval = ",pval12[3]))
 #  if(inf13[3]<0 && sup13[3]>0)
 #    print(paste("Native vs immig2 not significative, pval = ",pval13[3]))
 #  if(inf23[3]<0 && sup23[3]>0)
 #    print(paste("Immig1 vs immig2 not significative, pval = ",pval23[3]))
 #  
 # }
















