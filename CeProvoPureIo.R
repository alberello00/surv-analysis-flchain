library(survival)
data("flchain")
attach(flchain)

### QUESTION 1 - UNIVARIATE ANALYSIS ON ALL 

univariate_km = function(var) {
  surv_object <- Surv(futime, death)
  result_km <- survfit(surv_object ~ var, conf.type = "log-log")
  
  return(result_km)
}

#  we will not include variables: chapter (too many level)

# age
age_quantile <- cut(age, quantile(age), include.lowest =  T)
age_death = univariate_km(age_quantile)
plot(age_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of age quantiles')
legend("bottomleft", legend = c("[50,55]", "(55,63]", "(63, 72]", "(72, 101]"), lty = 1:2, col = c('red', 'blue', 'black', 'green'), horiz=T, cex = 0.8)


# sex
sex_death = univariate_km(sex)
plot(sex_death, col = c('red', 'blue'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of sex', ylim=c(0.65, 1))
legend("bottomleft", legend = c('M', 'F'), col = c('red', 'blue'), lty = 1:2, horiz = T)


# kappa
kappa_quantile = cut(kappa, quantile(kappa), include.lowest =  T)
kappa_death = univariate_km(kappa_quantile)
plot(kappa_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of kappa', ylim=c(0.3, 1))
legend("bottomleft", legend = c('[0.01,0.96]', '(0.96,1.27]', '(1.27,1.68]', '(1.68,20.5]'), col = c('red', 'blue', 'black', 'green'), lty = 1:2, horiz = T, cex = 0.8 )


# lambda
lambda_quantile = cut(lambda, quantile(lambda), include.lowest =  T)
lambda_death = univariate_km(lambda_quantile)
plot(lambda_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of lambda', ylim=c(0.3, 1))
legend("bottomleft", legend = c('[0.04,1.2]', '(1.2,1.51]', '(1.51,1.92]', '(1.92,26.6]'), col = c('red', 'blue', 'black', 'green'), lty = 1:2, horiz = T, cex = 0.8 )


# flc.gpr
flc.grp_death = univariate_km(flc.grp) # log-rang diviso in prime 5 seconde 5
plot(flc.grp_death, col = c('red', 'blue', 'black', 'green', 'purple', 'red', 'blue', 'black', 'green', 'purple'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of flc.grp', ylim=c(0.2, 1))
legend("bottom", legend = c(1,2,3,4,5,6,7,8,9,10), col = c('red', 'blue', 'black', 'green', 'purple', 'red', 'blue', 'black', 'green', 'purple'), lty = 1:2, horiz = T, cex = 0.5)


# creatinine
creatinine_quanitle = cut(creatinine, quantile(creatinine, na.rm = T), include.lowest =  T)
creatinine_death = univariate_km(creatinine_quanitle)
plot(creatinine_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of creatinine', ylim=c(0.4, 1))
legend("bottomleft", legend = c("[0.4,0.9]", "(0.9,1]", "(1,1.2]", "(1.2,10.8]"), col = c('red', 'blue', 'black', 'green'), lty = 1:2, horiz = T, cex = 0.8)


# mgus
mgus_death = univariate_km(mgus)
plot(mgus_death, col = c('red', 'blue'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of mgus', ylim=c(0.6, 1))
legend("bottomleft", legend = c(0,1), col = c('red', 'blue'), lty = 1:2, horiz = T, cex = 1)


### QUESTION 2 
surv_object = Surv(futime, death)

# complete model
cox_multi <- coxph(surv_object ~ age_quantile + sex + kappa_quantile + lambda_quantile + flc.grp + creatinine_quanitle + mgus)
cox_multi
extractAIC(cox_multi) # 31337.85

# here we aggregate kappa
cox_multi <- coxph(surv_object ~ age_quantile + sex + kappa + lambda_quantile + flc.grp + creatinine_quanitle + mgus)
cox_multi
extractAIC(cox_multi) # 31288.1

# here we aggregate lambda
cox_multi <- coxph(surv_object ~ age_quantile + sex + kappa + lambda + flc.grp + creatinine_quanitle + mgus)
cox_multi
extractAIC(cox_multi) # 31268.46

# here we remove mgus
cox_multi <- coxph(surv_object ~ age_quantile + sex + kappa + lambda + flc.grp + creatinine_quanitle)
cox_multi
extractAIC(cox_multi) # 31267.86

# here we aggregate 1st and 2nd quantile of creatinine
creatinine_quanitle_1 = cut(creatinine, quantile(creatinine, na.rm=T)[c(-2,-1)], include.lowest = T)
cox_multi <- coxph(surv_object ~ age_quantile + sex + kappa + lambda + flc.grp + creatinine_quanitle_1)
cox_multi
extractAIC(cox_multi) # 21471.79

# here we remove sex and kappa
creatinine_quanitle_1 = cut(creatinine, quantile(creatinine, na.rm=T)[c(-2,-1)], include.lowest = T)
cox_multi <- coxph(surv_object ~ age_quantile + lambda + flc.grp + creatinine_quanitle_1)
cox_multi
extractAIC(cox_multi) # 21468.69



install.packages("mfp")
library(mfp)

mfp=mfp(surv_object ~ fp(age) + ethn + qual1+qual2+voc+marr+child, family=cox, 
        method="breslow",verbose=T, select=1, alpha=0.05, data=DB)




# changing the baseline (continuous covariate)
cox2b <- coxph(surv_object ~ I(age - 35))
cox2b

# age classes
table(age_quantile)

cox3 <- coxph(surv_object ~ age_quantile)
cox3

plot(survfit(surv_object ~ age_quantile), col=1:4,lty=1:4, xlab="Months", ylab="Estimated S(t)")
legend("topright", legend = levels(age_quantile), lty = 1:4,col=1:4,title = "Age")




















































# Fleming-Harringon (Nelson-Aalen) estimator
?survfit
result.na <- survfit(g ~ 1, conf.type="log-log", stype=2)
summary(result.na)

# cumulative hazard and corresponding standard error
round(result.na$cumhaz,4)
round(result.na$std.chaz,4)

plot(result.na)
abline(h=0.5, col="red",lty=3)

tt_full<-c(tt,1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
cens_full<- c(cens, rep(1,21))
control <- rep(c(0,1), each=21)

results <- survfit(Surv(tt_full, cens_full) ~ control, conf.type="log-log")
results

summary(results)
plot(results,col=c("black", "red"),lty = 1:2,xlab = "Weeks", ylab = "estimated S(t)")
legend("topright", legend = c("treat", "control"), lty = c(1, 2),col=c("black","red"))

quantile(results)


