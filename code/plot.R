
#representation des y sur 2 axes
cercle_correlation.PLSDA <- function(object, PC1, PC2){
  Ypls = cbind(object$comp_X, y=as.matrix(object$y))
  ggplot(Ypls, aes(x=PC1, y=PC2, col = y, fill = y)) +
    stat_ellipse(geom = "polygon", col= "black", alpha =0.5)+
    geom_point(shape=21, col="black") +
    labs(title="Projection des individus sur les 2 axes factoriel",
         x ="Dim 1", y = "Dim 2", fill = "Modalitées")
}
cercle_correlation.PLSDA(object, "PC1", "PC2")



#projection des variables 
plan_factoriel.PLSDA <- function(object, X1, X2){
  Xpls <- as.data.frame(object$poid_X)
  ggplot() +  
    geom_text(data=Xpls, aes(x = X1, y = X2, label = rownames(Xpls)), col = 'red') +
    geom_segment(data=Xpls, aes(x = 0, y = 0, xend = X1, yend = X2), arrow=arrow(length=unit(0.2,"cm")),alpha = 0.75, color = 'darkred') + 
    labs(title="Projection des variables sur les 2 axes factoriel",
       x ="Dim 1", y = "Dim 2")
}
plan_factoriel.PLSDA(object, "PC1", "PC2")


#matrice de correlation 
correlationplot.PLSDA <- function(object, usedComp){
  ordre <- order(object$poid_X[,usedComp])
  X <- as.data.frame(object$X.init)
  mat.corr <- cor(X[ordre])
  corrplot::corrplot(mat.corr)
}
correlationplot.PLSDA(object,"PC1")



#proportion des variances expliquée des Z
propz.PLSDA <- function(object){
  prop <- as.data.frame(object$quality)
  p <- ggplot(prop, aes(x=rownames(prop),y = R2Ycum))
  p +
    geom_line(color="lightblue") +
    geom_point(color="violetred") +
    labs(title="Proportion des variances expliquée des Z",
         x ="Composantes", y = "% explication")
  #+ 
}

propz.PLSDA(object)




