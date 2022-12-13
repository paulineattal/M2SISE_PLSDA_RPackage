

install.packages("devtools")
library(devtools)

devtools::install_github('paulineattal/PLSDA_R_Package', subdir='/plslda')
library(plslda)


setwd("C:/Users/pauli/Documents/M2/R/projet/code/PLSDA_R_Package/")


library(readr)
data = read.table("zoo.csv", sep=";", header=TRUE)
formula = classe~.

print(head(data))

#selection de variables
print("Noms des variables avant séléction : ")
print(colnames(data))
sel.data = sel.forward(formula=formula, data=data)
print("Noms des variables apres séléction : ")
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
object =plslda.fit(formula=formula, data=data_split$train, ncomp="CV")
print("classe de l'object : ")
print(class(object))

#predict
ypred = plslda.predict(object=object, newdata=data_split$Xtest)
print(data.frame(ypred=as.factor(ypred), y=data_split$ytest))


#metrics
metrics <- plslda.metrics(y=data_split$ytest, ypred=ypred)
print(metrics)

#fonctions surchargées
print(object)
summary(object)


cercle_correlation.PLSDA(object=object, "PC1", "PC2")
plan_factoriel.PLSDA(object=object, "PC1", "PC2")
correlationplot.PLSDA(object=object,"PC1")
propz.PLSDA(object)

