# <center>**Installation of the package**</center>

----

## Installaton with devtools

### Install devtools package for R
```r
install.package('devtools')
```
### Download the PLSLDA package from github
```r
devtools::install_github('paulineattal/PLSDA_R_Package', subdir='/plslda')
```
or
```r
install_github('paulineattal/PLSDA_R_Package', subdir='/plslda')
```

## Installation from the tar.gz

### Download the tar.gz on your computer

First you need to download the file tar.gz on the repos then you can install it.
To install the package on Rstudio from a local file :
```r
install.packages('your/path/to/the/tar', repos=NULL, type="source)
```
### Or download the tar.gz directly on Rstudio

```r
install.packaages('https://github.com/paulineattal/PLSDA_R_Package/blob/main/plslda5_0.1.0.tar.gz', repos=NULL, method='libcurl')
```

-----

# Description of dataset

we will test this package with dataset zoo, available here https://www.kaggle.com/datasets/uciml/zoo-animal-classification

This dataset consists of 101 animals from a zoo.
There are 16 variables with various traits to describe the animals.
The 7 Class Types are: Mammal, Bird, Reptile, Fish, Amphibian, Bug and Invertebrate

The purpose for this dataset is to be able to predict the classification of the animals, based upon the variables.
It is the perfect dataset for those who are new to learning Machine Learning.

```r
data = read.table("zoo.csv", sep=";", header=TRUE)
print(head(data))

```

    ##   hair airborne aquatic predator toothed breathes venomous fins legs tail domestic catsize   classe
    ##   0        1       1        0       0        1        0    0    2    1        0       0     bird
    ##   1        0       0        0       1        1        0    0    4    1        0       0     mammal
    ##   0        1       0        0       0        1        0    0    2    1        0       0     bird
    ##   0        1       0        0       0        1        0    0    2    1        0       1     bird
    ##   0        0       0        0       0        1        0    0    0    0        0       0     invertebrate
  

# <center>**How use the package**</center>

________

## Load the package

First you need to load the package
```r
library(plslda5)
```
## Use the help function

You can acces to the function help to see all the documentation about your function.
```r
help('the_name_of_your_function')
```
or
```r
?the_name_of_your_function
```
## sel.forward()
This function will select only usefull variable en amont de l'application de la methode plsda 
```r
print("Noms des variables avant séléction : ")
print(colnames(data))
sel.data = sel.forward(formula=formula, data=data)
print("Noms des variables avant séléction : ")
print(colnames(sel.data))
```

    ##  Noms des variables avant séléction : 
    ##  "hair"     "airborne" "aquatic"  "predator" "toothed"  "breathes" "venomous" "fins"    "legs"     "tail"     "domestic" "catsize"  "classe"  
    ##  Noms des variables après sélection :
    ##  "classe"   "toothed"  "hair"     "fins"     "tail"     "breathes" "airborne" "aquatic"  "legs"

## plslda.split_sample()

cette fonction sert à séparer votre jeu de donnée pour l'apprentissage et le test de la methode 
```r
print("Nombre de ligne total du jeu de données : ")
print(nrow(sel.data))
data_split = plslda.split_sample(formula=formula, data=sel.data)
print("Nombre de ligne du jeu d'entrainement : ")
print(nrow(data_split$train))
print("Nombre de lignes du jeu de test : ")
print(nrow(data_split$Xtest))
```
    ##  Nombre de ligne total du jeu de données :
    ##  53
    ##  Nombre de ligne du jeu d'entrainement : 
    ##  37
    ##  Nombre de lignes du jeu de test : 
    ##  16
    
## plslda.fit()

Cette fonction sert a lancer l'entrainement du modele de a plslda

```r
object = plslda.fit(formula=formula, data=data_split$train)
print("classe de l'object : ")
print(class(object))
```
    ##  classe de l'object : 
    ##  PLSDA
```r
summary("summary de l'object :")
print(summary(object))
```



(comment le télécharger, comment le faire fonction et les fonctions incluses dedans ..)
#comment utiliser l'application
