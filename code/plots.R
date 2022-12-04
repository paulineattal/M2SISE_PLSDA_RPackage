
#representation des y sur 2 axes
cercle_correlation.PLSDA <- function(object, PC1, PC2){
  Ypls = cbind(object$comp_X, y=as.matrix(object$y))
  ggplot(Ypls, aes(x=PC1, y=PC2, col = y, fill = y)) +
    stat_ellipse(geom = "polygon", col= "black", alpha =0.5)+
    geom_point(shape=21, col="black") +
    labs(title="Projection des individus sur les 2 axes factoriel",
         x ="Dim 1", y = "Dim 2", fill = "Modalitées")
}
#cercle_correlation.PLSDA(object, "PC1", "PC2")


#projection des variables 
plan_factoriel.PLSDA <- function(object, PC1, PC2){
  Xpls <- as.data.frame(object$poid_X)
  ggplot() +  
    geom_text(data=Xpls, aes(x = PC1, y = PC2, label = rownames(Xpls)), col = 'red') +
    geom_segment(data=Xpls, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow=arrow(length=unit(0.2,"cm")),alpha = 0.75, color = 'darkred') + 
    labs(title="Projection des variables sur les 2 axes factoriel",
       x ="Dim 1", y = "Dim 2")
}
#plan_factoriel.PLSDA(object, "PC1", "PC2")


#matrice de correlation 
correlationplot.PLSDA <- function(object, usedComp){
  ordre <- order(object$poid_X[,usedComp])
  X <- as.data.frame(object$X.init)
  mat.corr <- cor(X[ordre])
  corrplot::corrplot(mat.corr)
}
<<<<<<< HEAD:code/plot.R
# <<<<<<< HEAD
# #correlationplot.PLSDA(object,"PC1")
# =======
# #correlationplot.PLSDA(object,"PC1")
# >>>>>>> 05ca2b5634deed9b5fcffedccb6a4577b6bd7abf

=======
#correlationplot.PLSDA(object,"PC1")
>>>>>>> 8ceb1905494d405d41bd2155245204dd4ee2a530:code/plots.R


#proportion des variances expliquée des Z
propz.PLSDA <- function(object){
  prop <- as.data.frame(object$quality)
  p <- ggplot(prop, aes(x=rownames(prop),y = R2Ycum))
  p +
    geom_line(aes(group=1),color="lightblue") +
    geom_point(color="violetred") +
    labs(title="Proportion des variances expliquée des Z",
         x ="Composantes", y = "% explication")
  #+ 
}

#propz.PLSDA(object)




