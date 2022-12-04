
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
colnames(data)
formula
sel.data = sel.forward(formula=formula, data=data)
colnames(sel.data)


#split datas
nrow(sel.data)
data_split = plsda.split_sample(formula=formula, data=sel.data)
nrow(data_split$train)
nrow(data_split$Xtest)

#fit
object = plslda.fit(formula=formula, data=data_split$train)
object

#predict
ypred = plslda.predict(object=object,newdata=data_split$Xtest)
ypred
data_split$ytest


#metrics
metrics <- report.plslda(y=data_split$ytest, ypred=ypred)
metrics
