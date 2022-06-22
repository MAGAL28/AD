getwd()
setwd("~/Documents/TP3")
help(setwd)
getwd()
setwd("~/Documents")
getwd()
setwd("~/Documents/AD/TP3")
getwd()
iris<-read.table("iris.txt", header=T, dec=",")
iris
x11()
par(mfrow=c(2,3))
#Calculons les corrélations 
cor(iris$SeLo,iris$SeLa)
#matrice de corrélations 
cor(iris[,-5])
#les variables les mieux corrélées sont les variables PeLa, PeLo car il est 0.96
#2 - Représentation (Camembert)

eff = table(iris$Espece)
pie(eff)
#3 - Représentation des 4 histogramme
par(mfrow=c(2,3))
hist(iris$SeLo, main = "Selo", col="green", xlab="Selo", ylab="Effectif")
hist(iris$SeLa, main = "Sela", col="red", xlab="Sela", ylab="Effectif")
hist(iris$PeLo, main = "Pelo", col="yellow", xlab="Pelo", ylab="Effectif")
hist(iris$PeLa, main = "Pela", col="orange", xlab="Pela", ylab="Effectif")

# 4 - Représentation des 16 histogrammes 
par(mfrow=c(4,4))
seto = iris[iris$Espece=="setosa", ]
versi = iris[iris$Espece=="versicolor", ]
virgi = iris[iris$Espece=="virginica", ]
hist(iris$SeLo, main = "Selo", col="green", xlab="Selo", ylab="Effectif", xlim = c(4,8))
hist(iris$SeLa, main = "Sela", col="red", xlab="Sela", ylab="Effectif", xlim = c(2,4))
hist(iris$PeLo, main = "Pelo", col="yellow", xlab="Pelo", ylab="Effectif", xlim = c(1,7))
hist(iris$PeLa, main = "Pela", col="orange", xlab="Pela", ylab="Effectif", xlim = c(0,2))
hist(seto$SeLo, main = "Selo", col="green", xlab="Selo-Seto", ylab="Effectif", xlim = c(4,8))
hist(seto$SeLa, main = "Sela", col="red", xlab="Sela-Seto", ylab="Effectif", xlim = c(2,4))
hist(seto$PeLo, main = "Pelo", col="yellow", xlab="Pelo-Seto", ylab="Effectif", xlim = c(1,7))
hist(seto$PeLa, main = "Pela", col="orange", xlab="Pela-Seto", ylab="Effectif", xlim = c(0,2))
hist(versi$SeLo, main = "Selo", col="green", xlab="Selo-Versi", ylab="Effectif", xlim = c(4,8))
hist(versi$SeLa, main = "Sela", col="red", xlab="Sela-Versi", ylab="Effectif", xlim = c(2,4))
hist(versi$PeLo, main = "Pelo", col="yellow", xlab="Pelo-Versi", ylab="Effectif", xlim = c(1,7))
hist(versi$PeLa, main = "Pela", col="orange", xlab="Pela-Versi", ylab="Effectif", xlim = c(0,2))
hist(virgi$SeLo, main = "Selo", col="green", xlab="Selo-Virgi", ylab="Effectif", xlim = c(4,8))
hist(virgi$SeLa, main = "Sela", col="red", xlab="Sela-Virgi", ylab="Effectif", xlim = c(2,4))
hist(virgi$PeLo, main = "Pelo", col="yellow", xlab="Pelo-Virgi", ylab="Effectif", xlim = c(1,7))
hist(virgi$PeLa, main = "Pela", col="orange", xlab="Pela-Virgi", ylab="Effectif", xlim = c(0,2))
#boîte à moustache 
boxplot(iris$SeLo~iris$Espece)
boxplot(iris$PeLo~iris$Espece)
iris
boxplot(iris$SeLa~iris$Espece , main = "SeLa",col="red")
boxplot(iris$PeLa~iris$Espece, col="green")
plot(iris$SeLo, iris$Sela, main="SeLo, SeLa", col=as.numeric(iris$Espece))
boxplot(iris$PeLo~iris$Espece,main="Pelo", col="blue")
boxplot(iris$SeLo~iris$Espece, xlab = "SeLo",ylab="Espece", col = c("pink","blue")[as.numeric(iris$SeLo)] ,pch = c(17,20)[as.numeric(iris$SeLo)],cex = 1.5, las = 1)
iris
help(xlim)
sum(iris$SeLo)


