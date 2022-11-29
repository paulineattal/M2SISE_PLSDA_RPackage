
setwd("C:/Users/pauli/Documents/M2/R/projet/code/PLSDA_R_Package")
data = read_excel("Data_LDA_Python.xlsx")

source("code/fit.R")
source("code/dummies.R")
source("code/cv.r")
source("code/nipals.r")
source("code/scale.R")
source("code/scale.R")
source("code/predict.r")

res_fit = fit(TYPE~., data=data)
res_fit

newdata = read_excel("Data_LDA_Python.xlsx", sheet="DATA_PREDICT")
pred = plsda.predict(res_fit,newdata)
