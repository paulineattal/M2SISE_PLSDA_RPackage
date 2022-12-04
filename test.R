library(ggplot2)

setwd("C:/Users/pauli/Documents/M2/R/projet/code/PLSDA_R_Package")
library(readxl)
library(readr)
data = read.table("zoo.csv", sep=";", header=TRUE)
formula = classe~.

#install.packages("https://github.com/paulineattal/PLSDA_R_Package/plslda5_0.1.0.tar.gz")


source("code/fit.R")
source("code/dummies.R")
source("code/cv.r")
source("code/nipals.r")
source("code/scale.R")
source("code/scale.R")
source("code/predict.r")
source("code/split_sample.r")
source("code/sel_forward.r")
source("code/metrics.r")
source("code/plots.r")


#selection de variables
print("Noms des variables avant séléction : ")
print(colnames(data))
print("Formule en entrée de la fonction : ")
print(formula)
sel.data = sel.forward(formula=formula, data=data)
print("Noms des variables avant séléction : ")
print(colnames(sel.data))


#split datas
print("Nombre de ligne total du jeu de données : ")
print(nrow(sel.data))
data_split = plslda.split_sample(formula=formula, data=sel.data)
print("Nombre de ligne du jeu d'entrainement : ")
print(nrow(data_split$train))
print("Nombre de lignes du jeu de test : ")
print(nrow(data_split$Xtest))

#fit
object = plslda.fit(formula=formula, data=data_split$train)
print("classe de l'object : ")
print(class(object))

#predict
ypred = plslda.predict(object=object,newdata=data_split$Xtest)
print(data.frame(ypred=as.factor(ypred), y=data_split$ytest))


#metrics
metrics <- plslda.metrics(y=data_split$ytest, ypred=ypred)
print(metrics)

#fonctions surchargées
source("code/print.r")
source("code/summary.r")
print(object)
summary(object)
