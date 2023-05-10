library(survival)
data("flchain")

# La gammopatia monoclonale è una patologia rara, caratterizzata dall'accumulo nel midollo osseo
# e nel sangue di una proteina anomala (paraproteina, proteina monoclonale o M proteina). --> Patologia cancerogena

# Analisi fatta su campioni di sangue
#L'elettroforesi delle sieroproteine (proteine sieriche, cioè del siero, che circolano attraverso il sistema circolatorio)
# è un esame di screening che permette di separare le proteine del siero in base alla loro mobilità in campo elettrico.

# Le molecole delle Immunoglobuline (gli anticorpi) sono proteine composte da due catene peptidiche "pesanti"
# identiche unite a due catene "leggere" identiche.--> catene leggere kappa e lambda --> la sovrapproduzione indica malfunzionamenti
# Il rapporto kappa/lambda per avere valori non anomali è: 0.26-1.65


#La causa principale di questa malattia è l’alterazione di alcune cellule del midollo osseo e la loro conseguente produzione eccessiva
#di proteina monoclonale. L’accumulo di questa proteina riduce lo spazio delle cellule sane e si concentra anche nel sangue. 
#Nonostante non si sappiano precisamente le cause di questa alterazione, recenti ricerche sembrano
#individuarne le cause in malattie autoimmuni (es. artrite, remautide).

# Let's try something
#### Lab 1 (leukemia) ####
#Change into a survival variable

flchain$age = as.integer(flchain$age)
flchain$sex = as.integer(flchain$sex) 
flchain$sample.yr = as.integer(flchain$sample.yr)
flchain$kappa = as.integer(flchain$kappa)
flchain$lambda = as.integer(flchain$lambda)
flchain$flc.grp = as.integer(flchain$flc.grp)
flchain$creatinine = as.integer(flchain$creatinine)
flchain$mgus = as.integer(flchain$mgus)

univariate_km = function(var) {
  surv_object <- Surv(var, death)
  result_km <- survfit(surv_object ~ 1, conf.type = "log-log")
  
  return(result_km)
}

#  we will not include variables: chapter, kappa, lambda (last two condensed into flc.grp)
# variables_of_interest = c(age, sex, sample.yr, flc.grp, creatinine, mgus, futime, chapter)

age_death = univariate_km(age)
sex_death = univariate_km(sex)
sample.yr_death = univariate_km(sample.yr)
flc.grp_death = univariate_km(flc.grp)
creatinine_death = univariate_km(creatinine)
mgus_death = univariate_km(mgus)
futime_death = univariate_km(futime)
chapter_death = univariate_km(chapter)

plot(age_death,col='black', lty = 1, xlab = "AGE", ylab = "estimated S(t)", main = 'Survival function as a function of AGE')
plot(sex_death,col='black', lty = 1, xlab = "SEX", ylab = "estimated S(t)", main = 'Survival function as a function of SEX')
plot(sample.yr_death,col='black', lty = 1, xlab = "SEX", ylab = "estimated S(t)", main = 'Survival function as a function of SEX')
plot(sample.yr_death,col='black', lty = 1, xlab = "SEX", ylab = "estimated S(t)", main = 'Survival function as a function of SEX')

# legend("topright", legend = c("treat", "control"), lty = c(1, 2),col=c("black","red"))


for (i in 1:8) {
  
}






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


