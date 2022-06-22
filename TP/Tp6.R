
getwd()
setwd("~/Documents/AD")
getwd()
#SECK SERIGNE         SECK SERIGNE      SECK SERIGNE  
#********************* LOI SYMETRIQUE ***************
par(mfrow=c(3,2))
de1 <-floor(runif(10000, 1,7))
mean(de1)
var(de1)
sd(de1)
hist(de1, main = "10000 lancers de 1 dé", prob=T , breaks = seq(0.5,6.5,1),xlab = paste("moy",round(mean(de1),2), " ; sd =",round(sd(de1),2)), ylab = "histogramme des fréquences")

de2 <- floor(runif(10000, 1,7))
sommeD1D2 <- de1+de2
sommeD1D2
mean(sommeD1D2)
var(sommeD1D2)

hist(sommeD1D2, main = "10000 lancers de 2 dé", prob=T,breaks = seq(0.5,12.5,1) ,xlab = paste("moy",round(mean(sommeD1D2),2), " ; sd =",round(sd(sommeD1D2),2)), ylab = "histogramme des fréquences")


de3 <- floor(runif(10000,1,7))
sommeD1D2D3 <- sommeD1D2 + de3
sommeD1D2D3
mean(sommeD1D2D3)
var(sommeD1D2D3)
hist(sommeD1D2D3, main = "10000 lancers de 3 dés", prob=T,breaks = seq(0.5,19.5,1) ,xlab = paste("moy",round(mean(sommeD1D2D3),2), " ; sd =",round(sd(sommeD1D2D3),2)), ylab = "histogramme des fréquences")

de4 <- floor(runif(10000,1,7))
de5 <- floor(runif(10000,1,7))
sommeD1D5 <- sommeD1D2D3 + de4 + de5
hist(sommeD1D5, main = "10000 lancers de 5 dés", prob=T,breaks = seq(0.5,30.5,1) ,xlab = paste("moy",round(mean(sommeD1D5),2), " ; sd =",round(sd(sommeD1D5),2)), ylab = "histogramme des fréquences")
mu = mean(sommeD1D5)
sigma=sd(sommeD1D5)

xth <- seq(5,30,0.1)
xth
yth <- dnorm(xth,mu , sigma)
yth
lines(xth,yth,col = "red",lwd=2)


#Oui on peut dire qu'on a une bonne approximation par la loi normale

#*********************    LOI NON SYMETRIQUE    

lamda <- 1
s1 <- rexp(10000 , rate = lamda)
m1=mean(s1)
m1
var1 = var(s1)
var1
hist(s1, main = "10000 tirages de X1", prob=T,xlab = paste("moy",round(mean(s1),2), " ; var =",round(var(s1),2)), ylab = "histogramme des fréquences")

s2 <- rexp(10000 , rate = lamda)
m2=mean(s2)
m2
var2 = var(s2)
var2
sommeS1S2 <- s2+s1
hist(sommeS1S2, main = "10000 tirages de X2", prob=T,xlab = paste("moy",round(mean(sommeS1S2),2), " ; var =",round(var(sommeS1S2),2)), ylab = "histogramme des fréquences")


s5 <- rexp(10000,rate = lamda)
for (i in 2:5) s5 <- s5 + rexp(10000,rate = lamda)
m5=mean(s5)
m5
var5 = var(s5)
var5
hist(s5, main = "10000 tirages de X5", prob=T,xlab = paste("moy",round(mean(s5),2), " ; var =",round(var(s5),2)), ylab = "histogramme des fréquences")


s10 <- rexp(10000,rate = lamda) 
for (i in 2:10) s10 <- s10 + rexp(10000, rate = lamda)
m10=mean(s10)
m10
var10 = var(s10)
var10
hist(s10, main = "10000 tirages de X10", prob=T,xlab = paste("moy",round(mean(s10),2), " ; var =",round(var(s10),2)), ylab = "histogramme des fréquences")


s50 <- rexp(10000,rate = lamda) 
for (i in 2:50) s50 <- s50 + rexp(10000, rate = lamda)
m50=mean(s50)
m50
var50 = var(s50)
var50
hist(s50, main = "10000 tirages de X50", prob=T,xlab = paste("moy",round(mean(s50),2), " ; var =",round(var(s50),2)), ylab = "histogramme des fréquences")


s100 <- rexp(10000,rate = lamda) 
for (i in 2:100) s100 <- s100 + rexp(10000, rate = lamda)
m100=mean(s100)
m100
var100 = var(s100)
var100
hist(s100, main = "10000 tirages de X100", prob=T,xlab = paste("moy",round(mean(s100),2), " ; var =",round(var(s100),2)), ylab = "histogramme des fréquences")


#Commentaire 
#On remarque que plus n augmente plus on s'approche à une loi normale
n = 10000
r = rexp(n,1)
x = seq(min(s100),max(s100),0.1)
y = dnorm(x,mean = m100,sd=sqrt(var100))
lines(x,y,col="red",lwd=2)

#**************$     EX0 3 LOI EXPONENTIELLE

n = 100
lambda <- 2
e <- rexp(n,rate=lambda)
e
mean(e)
var(e)

moy = numeric(10000)
for(i in 1:10000){moy[i] <- mean(rexp(100,lambda))}
moy


#L'Histogramme 
hist(moy, main = "10000 moyennes obtenus", prob=T,xlab = paste("moy",round(mean(moy),2), " ; sd =",round(sd(moy),2)), ylab = "histogramme des fréquences")
#il s'approche de la loi normale , preuve avec la droite densité ci dessous : 
v100 <- sd(moy)
x = seq(min(moy),max(moy),0.01)
x
y = dnorm(x,mean(moy),sd=v100)
lines(x,y,col="red",lwd=2)


#************************ TEST PARAMETRIQUES : test bilatéral

moyenne <- 80
sigma <- 0.5
n <- 100
seuil <- c((moyenne - qnorm(.975)*sigma/sqrt(n)),(moyenne + qnorm(.975)*sigma/sqrt(n)))
seuil
#quelle devrait être la taille de l'échantillon si on divise par 2 la longueur de l'intervalle 
#Soit m égale à l'échantillon cherché - 
m<-((sigma*qnorm(.975))^2)/(80.098-79.902)^2
m
#Hyposthése < Ho : µ = 80 ; H1 : µ # 80 >
#Le poids moyen observé par le controleur est inclu dans l'intervalle de confiance donc oui la production est conforme. 




#***********************     TEST UNILATERAL 

getwd()
tomate <- read.table("tomates.txt", header = T)
tomate

mean(tomate$diam)
var(tomate$diam)

n = 100
moyenne = mean(tomate$diam)
moyenne
sCarre = 0
for( i in 1 : 100)sCarre = sCarre + ((tomate$diam[i]-moyenne)^2)
sCarre
sCarre = sCarre/(n-1)
s = sqrt(sCarre)
s

#Déterminons l'intervalle de confiance 
qt(0.975,df=n-1) #valeur table de Student
seuil <- c(moyenne - qt(.975,n-1)*s/sqrt(n), moyenne + qt(.975,n-1)*s/sqrt(n))
seuil
t.test(tomate$diam) #vérification des valeurs 


# < la moyenne de la commande est plus grand que la moyenne attendu, on rejette Ho le fournisseur n'a pas respecté le calibre demandé >

#**************************     LOI DE STUDENT ET LOI NORMALE 

qnorm(.975)
qnorm(.95)
#réciproque 
pnorm(1.96)
pnorm(1.645)

#comparaison avec la loi de Student 
x <- seq(-3.5,3.5,0.1)
yStu1 <- dt(x,df=1)
plot(x,yStu1, type="l",col="red",ylim=c(0,0.5),main="Comparaison Student/Gauss")
yStu2 <- dt(x,df=2)
lines(x,yStu2,col="orange")
yStu5 <- dt(x,df=5)
lines(x,yStu5,col="blue")
yStu100 <- dt(x,df=100)
lines(x,yStu100,col="black")

dnorm(x,0,1)
x<-seq(-3.5,3.5,0.1)
lines(x,dnorm(x,0,1),col="green4")

