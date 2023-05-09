library(survival)
data("flchain")
flchain
summary(flchain)

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
g<-Surv(flchain$age,flchain$death)
class(g)
#Kaplan-Meier est
result.km<- survfit(g~ 1, conf.type = "log-log")
result.km
summary(result.km)

# Standard errors
d_j <- result.km$n.event[result.km$n.event!=0]  #
r_j <- result.km$n.risk[result.km$n.event!=0]
S_t <- result.km$surv[result.km$n.event!=0]
to.sum <- d_j/(r_j*(r_j-d_j))
se <- sqrt(S_t[1]^2*to.sum[1])
for(i in 2:length(r_j)){
  se[i] <- sqrt(S_t[i]^2*sum(to.sum[1:i]))
}
round(se,4)
summary(result.km)

# log-log confidence interval   
se_cloglog <- sqrt(1/log(S_t[1])^2*to.sum[1])
for(i in 2:length(r_j)){
  se_cloglog[i] <- sqrt(1/log(S_t[i])^2*sum(to.sum[1:i]))
}
A_t <- qnorm(0.975)*se_cloglog
round(S_t^exp(A_t),3)
round(S_t^exp(-A_t),3)
summary(result.km)

# median 
plot(result.km)
abline(h = 0.5, col = "red", lty = 3)

quantile(result.km )
abline(v = c(quantile(result.km)$quantile[2],
             quantile(result.km)$lower[2],
             quantile(result.km)$upper[2]), col = "red",lty = 3)

# 1st quartile
plot(result.km)
abline(h=0.75, col="red",lty=3)
abline(v=c(quantile(result.km)$quantile[1],
           quantile(result.km)$lower[1],
           quantile(result.km)$upper[1]), col="red",lty=3)

# Fleming-Harringon (Nelson-Aalen) estimator
?survfit
result.na <- survfit(g ~ 1, conf.type="log-log",stype=2)
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


