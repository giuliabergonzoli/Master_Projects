library(MASS)
library(car)
library(rgl)


library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4) #main package but only w/ resduals indep and homosk
library(insight)

library(ggplot2)

###############
### COUNTRY ###
###############

studentsData= read.table(file = "student_eur.txt", header = T)
studentsData=na.omit(studentsData)

studentsData$immigration[which(studentsData$immigration==1)] = 0;
studentsData$immigration[which(studentsData$immigration==2 + I(studentsData$immigration==3))] = 1;
table(studentsData$immigration)
studentsData$country= as.factor(studentsData$country)

#math
x11()
ggplot(data=studentsData, aes(x=as.factor(country), y=math, fill=as.factor(country))) +
  geom_boxplot() +
  labs(x='Country', y='Math Achievement') +
  ggtitle('Boxplot of math achievements among countries') +
  theme_minimal() +
  theme(axis.text=element_text(size=rel(1.15)),axis.title=element_text(size=rel(1.5)),
        plot.title = element_text(face="bold", size=rel(1.75)), legend.text = element_text(size=rel(1.15)),
        legend.position = 'none')


##--------------##
## Linear Model ##
##--------------##
# We start with a standard linear regression model, neglecting the dependence structure
lm1 <- lm(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + 
            + immigration:emo_sup, data = studentsData)
summary(lm1)

plot(lm1$residuals)

boxplot(lm1$residuals ~ studentsData$country, col='orange', xlab='country', ylab='Residuals')
# residuals don't differ a lot across countries

#-----------------------------#
# Linear Mixed Effects Models #
#-----------------------------#
# We now take into account the clustering at country --> dependency among students within the same country
lmm1 = lmer(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
              + ESCS_status + teacher_support + emo_sup + 
              + learn_time_math + immigration:language + 
              + immigration:grade_rep + immigration:fear_failure + 
              + immigration:emo_sup + (1|country), 
            data = studentsData)
summary(lmm1)


# Fixed Effects and 95% CIs
#-------------------------------
confint(lmm1, oldNames=TRUE)
fixef(lmm1)

# Variance components
print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b
#Percentage of Variance explained by the Random Effect (PVRE).
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE #0.02313535

# Random effects: b_0i
ranef(lmm1)

# dotplot
x11()
dotplot(ranef(lmm1))

# Random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2)
coef(lmm1)

## visualization of the coefficients
x11()
par(mfrow=c(1,3))
plot(unlist(coef(lmm1)$country[1]),
     xlab='country i', ylab=expression(beta[0]+b['0i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random intercepts + fixed intercepts')
abline(h=fixef(lmm1)[1], lty=2, col='red', lwd=2)
legend(30, 14, legend=expression(paste('Fixed intercept ',beta[0])), lwd=2, lty=2, col='red', x.intersp=0.5)
plot(unlist(coef(lmm1)$country[2]),
     xlab='country i', ylab=expression(beta[1]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slopes for gender')
abline(h=fixef(lmm1)[2], lty=2, col='red', lwd=2)
legend(30, -0.6, legend=expression(paste('Fixed slope ',beta[1])), lwd=2, lty=2, col='red', x.intersp=0.5)
plot(unlist(coef(lmm1)$country[3]),
     xlab='country i', ylab=expression(beta[2]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slopes for ESCS_status')
abline(h=fixef(lmm1)[3], lty=2, col='red', lwd=2)
legend(30, 2.35, legend=expression(paste('Fixed slope ',beta[2])), lwd=2, lty=2, col='red', x.intersp=0.5)

# Diagnostic plots 
#------------------
# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm1)

x11()
qqnorm(resid(lmm1))
qqline(resid(lmm1), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(lmm1)$country), main='Normal Q-Q Plot - Random Effects for Primary studentsData')
qqline(unlist(ranef(lmm1)$country), col='red', lwd=2)

#--------------------------------------------------#
# Linear Mixed Model with Random Intercept & Slope #
#--------------------------------------------------#
graphics.off()

lmm2 = lmer(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
              + ESCS_status + teacher_support + emo_sup + 
              + learn_time_math + immigration:language + 
              + immigration:grade_rep + immigration:fear_failure + 
              + immigration:emo_sup + (1 + immigration|country), 
            data = studentsData)
summary(lmm2)

confint(lmm2, oldNames=TRUE) #non � significativo sig02 che si riferisce alla correlazione, provo il modello sotto
fixef(lmm2)

# Yet another point of interest is the correlation of the intercepts and slopes. In this case it's -0.22. 
# That's pretty small, but the interpretation is the same as with any correlation. 

# Variance components
print(vc <- VarCorr(lmm2), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(lmm2))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm2))  ## it automatically computes Var(b0,b1)
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE #0.02574573

# Estimates of fixed and random effects

# Fixed effects: (beta_0, beta_1, beta_2)
fixef(lmm2)

# Random effects: (b_0i, b_1i) for i=1,...,200
ranef(lmm2)

x11()
dotplot(ranef(lmm2))

# Random intercepts and slopes: (beta_0+b_0i, beta_1, beta_2+b_2i)
coef(lmm2)

## Visualization of random effects 
x11()
par(mfrow=c(1,3))
plot(unlist(coef(lmm2)$country[1]),
     xlab='country i', ylab=expression(beta[0]+b['0i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random intercepts')
abline(h=fixef(lmm2)[1], lty=2, col='red', lwd=2)
legend(30, 13.5, legend=expression(paste('Fixed intercept ',beta[0])), lwd=2, lty=2, col='red', x.intersp=0.5)

plot(unlist(coef(lmm2)$country[2]),
     xlab='country i', ylab=expression(beta[1]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slope for gender')
abline(h=fixef(lmm2)[2], lty=2, col='red', lwd=2)
legend(30,-0.6, legend=expression(paste('Fixed slope ',beta[1])), lwd=2, lty=2, col='red', x.intersp=0.5)

plot(unlist(coef(lmm2)$country[3]),
     xlab='Student i', ylab=expression(beta[2]+b['1i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random slopes for ESCS_status')
abline(h=fixef(lmm2)[3], lty=2, col='red', lwd=2)
legend(30, 5, legend=expression(paste('Fixed slope ',beta[2])), lwd=2, lty=2, col='red', x.intersp=0.5)

# Diagnostic plots 
#--------------------
# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm2)

x11()
qqnorm(resid(lmm2))
qqline(resid(lmm2), col='red', lwd=2)


# 2) Assessing Assumption on the Random Effects
x11()
par(mfrow=c(1,2))
qqnorm(unlist(ranef(lmm2)$country[1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(lmm2)$country[1]), col='red', lwd=2)
qqnorm(unlist(ranef(lmm2)$country[2]), main='Normal Q-Q Plot - Random Effects on ESCS_status')
qqline(unlist(ranef(lmm2)$country[2]), col='red', lwd=2)

x11()
plot(unlist(ranef(lmm2)$country[2]),unlist(ranef(lmm2)$country[1]),
     ylab=expression(paste('Intercept  ', b['0i'])),
     xlab=expression(paste('ESCS_status  ', b['1i'])), col='dodgerblue2',
     main='Scatterplot of estimated random effects')
abline(v=0,h=0)
# Alternative plot(ranef(lmm2))


# Comparing models
anova(lmm1, lmm2)
# The p-value for the test is essentially zero -> we prefer lmm2 (which has also lower AIC)


## We observe that the correlation between d_11 and d_22 id very low, 
## we fit a new model with a diagonal D matrix
lmm3 <- lmer(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
               + ESCS_status + teacher_support + emo_sup + 
               + learn_time_math + immigration:language + 
               + immigration:grade_rep + immigration:fear_failure + 
               + immigration:emo_sup + (1|country) + (0 + immigration|country),
                   data = studentsData, control=lmerControl(optimizer="bobyqa",
                                                    optCtrl=list(maxfun=2e5)))

summary(lmm3)
confint(lmm3,oldNames=TRUE)
fixef(lmm3)

# PVRE
sigma2_eps <- as.numeric(get_variance_residual(lmm3))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm3)) + as.numeric(get_variance_slope(lmm3))
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE #0.04398689

## visualization of the random intercepts with their 95% confidence intervals
dotplot(ranef(lmm3, condVar=T))
ranef(lmm3, condVar=T)

# Comparing models
#------------------
# The anova function, when given two or more arguments representing fitted models,
# produces likelihood ratio tests comparing the models.
anova(lmm2, lmm3)

#according to the p-values the two models are essentially the same, so we choose the one with lower AIC (lmm3)
