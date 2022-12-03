
setwd("C:/Users/pauli/Documents/M2/R/projet/code/PLSDA_R_Package")
data = read_excel("Data_LDA_Python.xlsx")
newdata = read_excel("Data_LDA_Python.xlsx", sheet="DATA_PREDICT")
formula=TYPE~.

#install.packages("https://github.com/paulineattal/PLSDA_R_Package/plslda5_0.1.0.tar.gz")


source("code/fit.R")
source("code/dummies.R")
source("code/cv.r")
source("code/nipals.r")
source("code/scale.R")
source("code/scale.R")
source("code/predict.r")
source("code/split_sample.r")

data = iris
formula = Species~.
data_split = plsda.split_sample(formula, data)
data = data_split$train
newdata = data_split$Xtest


object = plslda.fit(formula, data=data)
object

pred = plslda.predict(object,newdata)
pred
data_split$ytest
