
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
library(ggplot2)
source("code/plots.r")


#selection de variables
print(past("Noms des variables avant séléction : ",colnames(data)))
print(past("Formule en entrée de la fonction : ",formula))
sel.data = sel.forward(formula=formula, data=data)
print(past("Noms des variables avant séléction : ",colnames(sel.data)))


#split datas
print(past("Nombre de ligne total du jeu de données : ",nrow(sel.data)))
data_split = plsda.split_sample(formula=formula, data=sel.data)
nrow(past("Nombre de ligne du jeu d'entrainement : ",data_split$train))
nrow(past("Nombre de lignes du jeu de test : ",data_split$Xtest))

#fit
object = plslda.fit(formula=formula, data=data_split$train)
print(class(object))

#predict
ypred = plslda.predict(object=object,newdata=data_split$Xtest)
print(data.frame(ypred=as.factor(ypred), y=data_split$ytest))


#metrics
metrics <- report.plslda(y=data_split$ytest, ypred=ypred)
print(metrics)
