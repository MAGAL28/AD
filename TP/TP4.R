#SECK Serigne 
getwd()
setwd("~/downloads/TP3-AD")
getwd()
ventes <- read.table("ventes.txt", header=T)

#Traçons le nuage de point 
plot(ventes$pub, ventes$CA , main="nuage de point", xlab = "Budget pub", ylab = "Chiffres d'affaires")
#les variables semblent bien corrélées car ils ont tendences à suivrent le même chemin. Une droitre passera parfaitement entre les variables. 
ventes
#Q3 Coef cor
cor(ventes$pub,ventes$CA)
cor.test(ventes$pub,ventes$CA)
# on a p-value = 0.0179 ainsi p-value <= 0.05 donc on conclu que la corrélations est signifivative
#Q4 Régression linéaire  de "CA" en fonction de "pub"
reg <- lm(CA~pub, data = ventes)
# droite de régression
reg$coef
reg$coef[1]
reg$coef[2]
abline(reg,col="red")
legend ("topleft", "Droite de régression : CA = 127.9703 + 3.466479 * pub", text.col = "blue")
ventes

#Q5 Déterminons le vecteurs chiffres d'affaires théorique 
CAtheo <- reg$coef[1] + reg$coef[2] * ventes$pub
CAtheo
lines(ventes$pub, CAtheo, col = "black")
#Q6 coefficiant de detrmination 
cd <- var(CAtheo)/var(ventes$CA)
cd
# justifions que le carré du coefficient de corrélation est ègale au coéfficient de détermination
r <- cor(ventes$pub, ventes$CA)
r^2
#On a bien le carré du coefficient de corrélation égale au coéfficiant de détermination. 

#Q7
CA = reg$coef[1]+reg$coef[2]*198
CA

#**********************************************************************************************#
#Régression non linéaire 
freinage <- read.table("freinage.txt",header = T)
freinage

#Q1  traçons le nuage de points
plot(freinage$V, freinage$C, main = "nuange de point",xlab="Vitesse du véhicule", ylab = "Chemin de freinage", col="blue")
cor(freinage$C, freinage$V)
#Q2 les variables sont-elles corrélées ? 
cor.test(freinage$V , freinage$C)
#les variables sont bien corrélées car les variables ont tendences à suivre le même chemin. 
#on a p-value <= 0.05 donc les variables sont bien corrélées. 
#Graphiquement on peut dire aussi que les variables sont bien corrélées car ils ont tendences à suivrent le même chemin


#Q3 
#Q4 Régression linéaire "V" en fonction de "C"

reg <- lm(C~V, data = freinage)

#Le modèle linéaire semble bien adapté car un peu vaoir un ajustement linéaire c'est à dire une droite qui passe équitablement entr les variables. 
lnV <- log(freinage$V)
lnC <- log(freinage$C)

c <- freinage$C
v <- freinage$V
#Caculons b:
b <- (cov(lnV,lnC))/var(lnV)
b
#Calculons a :
a <- exp(mean(lnC)-b*mean(lnV))
a

#Le modèle obetu sera : C = a*V^b
Ctheo <- a*freinage$V^b
Ctheo

#droite de régression 
lines(freinage$V, Ctheo, col = "red")
#le Chemin de freinage sera exprimé par C =0.00549303 V^2.0000684

#Q6 déterminer le coef de détermination 
cd <- var(Ctheo)/var(freinage$C)
cor(lnC,lnV)^2
# On a que le carré du coéfficient de corrélation égale au coef de détermination

#Q7 A quoi sera  égal le chemin de freinage si le v ́ehicule roule `a 160 km/h ?
chemin <- a*160^b
chemin 

#*********************************************************************************#
#Série Chronologique
morinfantile <- read.table("morinfantile.txt",header = T)
morinfantile


#Q1 nuange de point 
plot(morinfantile$t, morinfantile$M, main = "nuange de point",xlab="Temps", ylab = "Taux de mortalité infantile")


#eg <- lm(C~V, data = freinage)

#Le modèle linéaire semble bien adapté car on peu avoir un ajustement linéaire c'est à dire une droite qui passe équitablement entr les variables. 
lnt <- log(morinfantile$t)
lnM <- log(morinfantile$M)
 lnt
 lnM
t <- morinfantile$t
m <- morinfantile$M
#Caculons b:
t <- (cov(lnt,lnM))/var(lnt)
t
#Calculons a :
m <- exp(mean(lnt)-b*mean(lnM))
m
cor.test(morinfantile$t, morinfantile$M)
#trés proche de zéro donc 
#Le modèle obetu sera : C = a*V^b
Cotheo <- t*morinfantile$t^m
Cotheo

#droite de régression 
lines(morinfantile$t, Cotheo, col = "red")
#le Chemin de freinage sera exprimé par C =0.00549303 V^2.0000684

#Q6 déterminer le coef de détermination 
cd <- var(Cotheo)/var(morinfantile$M)
cd
cor(lnt,lnM)^2
#Q7 
prevoir <- t*2010^m
