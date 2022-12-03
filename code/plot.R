
#representation des y sur les 2 premiers axes

Ypls = cbind(object$comp_X, y=as.matrix(object$y))
PC1 = PC1
PC2 = PC2

ggplot(Ypls, aes(x=PC1, y=PC2, col = y, fill = y)) +
  stat_ellipse(geom = "polygon", col= "black", alpha =0.5)+
  geom_point(shape=21, col="black")



#projection des variables 

Xpls <- as.data.frame(object$poid_X)

ggplot() +  
  geom_text(data=Xpls, aes(x = X1, y = X2, label = rownames(Xpls)), col = 'red') +
  geom_segment(data=Xpls, aes(x = 0, y = 0, xend = X1, yend = X2), arrow=arrow(length=unit(0.2,"cm")),alpha = 0.75, color = 'darkred')



#matrice de correlation 

mat.corr <- cor(as.matrix(object$y), object$comp_X)^2
usedComp <- "X1"
ordre <- order(object$poid_X[,usedComp])
X <- as.data.frame(X)
mat.corr <- cor(X[ordre])

corrplot::corrplot(mat.corr)


#proportion des variances expliquée des Z
prop <- as.data.frame(object$quality)

p <- ggplot(prop, aes(x=rownames(prop),y = R2Ycum))
p + geom_line(color="lightblue") + geom_point(color="violetred")




