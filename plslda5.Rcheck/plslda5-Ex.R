pkgname <- "plslda5"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('plslda5')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("cercle_correlation.PLSDA")
### * cercle_correlation.PLSDA

flush(stderr()); flush(stdout())

### Name: cercle_correlation.PLSDA
### Title: Plots
### Aliases: cercle_correlation.PLSDA

### ** Examples

data(iris)
formule = Species~.
object = plslda.fit(formula=formule, data=iris)

cercle_correlation.PLSDA(object=object, "PC1", "PC2")
plan_factoriel.PLSDA(object=object, "PC1", "PC2")
correlationplot.PLSDA(object=object,"PC1")
propz.PLSDA(object)




cleanEx()
nameEx("plslda.cv")
### * plslda.cv

flush(stderr()); flush(stdout())

### Name: plslda.cv
### Title: plslda.cv, Cross validation for PLSLDA
### Aliases: plslda.cv

### ** Examples

data(iris)
plslda.cv(Species~., data = iris)
plslda.cv(Species~.,data=iris, nfold = 50)



cleanEx()
nameEx("plslda.dummies")
### * plslda.dummies

flush(stderr()); flush(stdout())

### Name: plslda.dummies
### Title: plslda.dummies, Converting factor to dummy
### Aliases: plslda.dummies

### ** Examples

data(iris)
formule = Species~.

y <- as.factor(model.response(model.frame(formula=formule, data = iris)))

dummy <- plslda.dummies(iris$Species, mod=NA)
dummy_mod <-plslda.dummies(iris$Species, mod= y)



cleanEx()
nameEx("plslda.fit")
### * plslda.fit

flush(stderr()); flush(stdout())

### Name: plslda.fit
### Title: plslda.fit, Function used to fit the PLS-LDA Regression model
### Aliases: plslda.fit

### ** Examples

1st example : where all the variables are used
fit(Species ~ ., data = iris, ncomp = 2,max.iter = 100,tol = 1e-06)
2nd example : only 2 variables are selected
fit(Species ~ Sepal.Length + Petal.Length, data = iris, ncomp = 2, max.iter = 100,tol = 1e-06)
3rd example : using "CV" in ncomp parameter
fit(Species ~ Sepal.Length + Petal.Length, data = iris, ncomp = "CV", max.iter = 100,tol = 1e-06)





cleanEx()
nameEx("plslda.metrics")
### * plslda.metrics

flush(stderr()); flush(stdout())

### Name: plslda.metrics
### Title: report.plslda, function to calculate the different metrics of a
###   prediction, to define the quality of the prediction based on the
###   confusion matrix components, and the accuracy score
### Aliases: plslda.metrics

### ** Examples

data(iris)
formula = Species~.

data_split = plslda.split_sample(formula=formule, data=iris)

object = plslda.fit(formula=formula, data=data_split$train)

ypred = plslda.predict(object=object,newdata=data_split$Xtest)

metrics <- plslda.metrics(y=data_split$ytest, ypred=ypred)
print(metrics$summary)
print(metrics$accuracy)




cleanEx()
nameEx("plslda.nipals")
### * plslda.nipals

flush(stderr()); flush(stdout())

### Name: plslda.nipals
### Title: plslda.nipals, Nipals
### Aliases: plslda.nipals

### ** Examples

data(iris)
formula = Species~.
X <- as.matrix(model.matrix(formula=formule, data = iris)[,-1])
y <- as.factor(model.response(model.frame(formula=formule, data = iris)))
ydum <- plslda.dummies(y)

plslda.nipals(X=X, y=ydum, ncomp = 2, max.iter = 100, tol = 1e-06)




cleanEx()
nameEx("plslda.predict")
### * plslda.predict

flush(stderr()); flush(stdout())

### Name: plslda.predict
### Title: plslda.predict, The function used to predict in our model
### Aliases: plslda.predict

### ** Examples


#' data(iris)
formula = Species~.

data_split = plslda.split_sample(formula=formule, data=iris)

object = plslda.fit(formula=formule, data=data_split$train)

ypred = plslda.predict(object=object,newdata=data_split$Xtest)




cleanEx()
nameEx("plslda.scale")
### * plslda.scale

flush(stderr()); flush(stdout())

### Name: plslda.scale
### Title: plslda.scale, Function to standardize vectors and matrixes
###   (center and reduce)
### Aliases: plslda.scale

### ** Examples

data(iris)
scale.t1<-plslda.scale(iris[,-5],center=TRUE, scale=TRUE)
scale.t2<-plslda.scale(iris[,-5],center=TRUE, scale=FALSE)



cleanEx()
nameEx("print.PLSDA")
### * print.PLSDA

flush(stderr()); flush(stdout())

### Name: print.PLSDA
### Title: print.PLSDA, Function used to print classification function and
###   latent vectors
### Aliases: print.PLSDA

### ** Examples

data(iris)
formule = Species~.

PLSDA_object = fit(formula=formule, data=iris)
print(PLSDA_object)




cleanEx()
nameEx("sel.forward")
### * sel.forward

flush(stderr()); flush(stdout())

### Name: sel.forward
### Title: sel.forward, The function used for variables selection,
###   following the forward method
### Aliases: sel.forward

### ** Examples

data(iris)
formule = Species~
sel <- sel.forward(formula=formule, data=iris, slentry = 0.01, verbose=FALSE)





cleanEx()
nameEx("summary.PLSDA")
### * summary.PLSDA

flush(stderr()); flush(stdout())

### Name: summary.PLSDA
### Title: summary.PLSDA, This function prints the standardized projection
###   coefficients of matrix X
### Aliases: summary.PLSDA

### ** Examples

data(iris)
formule = Species~.

PLSDA_object = fit(formula=formule, data=iris)
summary(PLSDA_object)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
