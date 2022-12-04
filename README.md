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





(comment le télécharger, comment le faire fonction et les fonctions incluses dedans ..)
#comment utiliser l'application