
getwd()
setwd("~/Documents/AD")
cure <- read.table("cure.txt",header = T)
cure
mean(cure$pav)
mean(cure$pap)
nrow(cure[cure$pav <= 75,])
nrow(cure[cure$pap <= 75,])

par(mfrow=c(2,2))
hist(cure$age, main = "Histogramme de l'age", col = 12)
repSexe <- table(cure$sexe)
repSexe
pie(repSexe, main="repartion des sexes",col=c("pink","lightblue"),labels=paste(c("femmes","hommes"),"  ",round(repSexe*100/66,1), "%"))

hist(cure$pav, main = "Histogramme des poids avant cure", col = 10, xlab = " poids en Kg", ylab = "")
hist(cure$pav, main = "Histogramme des poids après cure", col = 2, xlab = " poids en Kg", ylab = "")

par(mfrow=c(2,2))
(boxplot(cure$pav,main="poids avant cure"))
(boxplot(cure$pap,main="poids aprés cure"))
boxplot(cure$pav~cure$sexe,main="poids avant cure selon le sexe",col=c("pink","lightblue"),names=c("femmes","hommes"))
boxplot(cure$pap~cure$sexe,main="poids aprés cure selon le sexe",col=c("pink","lightblue"),names=c("femmes","hommes"))

reg <-  lm(age~pap,data=cure)

plot(cure$pap~cure$pav)
cor.test(cure$pav,cure$pap)

#reg linéaire 
reg <- lm(pap~pav,data = cure)
#droite de reg 
abline(reg, col="red")
reg$coeff[1]
reg$coeff[2]

legend ("topleft", "Droite de régression : pap = 35.62 + 0.44 * pub", text.col = "blue", cex = 0.5)

reg$coeff[1]+reg$coeff[2]*104

lnPap <- log(cure$pap)
lnPav <- log(cure$pav)
reg <- lm(lnPap~lnPav, data = cure)
reg$coef[1]
reg$coef[2]
cor(lnPap,lnPav)

pap <-  reg$coeff[1]+cure$pav^reg$coeff[2]
pap


moyenne <- 120
sigma <- 12
n <- 1
seuil <- c((moyenne - qnorm(.95)*sigma/sqrt(n)))
seuil


moyenne <- 120
sigma <- 12
n <- 10
seuil <- c((moyenne - qnorm(.95)*sigma/sqrt(n)))
seuil

moyenne <- 129
sigma <- 12
n <- 10
seuil <- c((moyenne - qnorm(.95)*sigma/sqrt(n)))
seuil





