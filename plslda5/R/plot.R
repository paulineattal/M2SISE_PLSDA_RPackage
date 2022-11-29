
#pouettey = cbind(nipals.res$comp_X, y=as.matrix(y))

#ggplot(pouettey, aes(x=PC1, y=PC2, col = y, fill = y)) +
 # stat_ellipse(geom = "polygon", col= "black", alpha =0.5)+
 # geom_point(shape=21, col="black")


#pouettex = as.data.frame(nipals.res$poid_Y)

#ggplot() +  
 # geom_text(data=pouettex, aes(x = X1, y = X2, label = rownames(pouette)), col = 'red') +
 # geom_segment(data=pouettex, aes(x = 0, y = 0, xend = X1, yend = X2), arrow=arrow(length=unit(0.2,"cm")),alpha = 0.75, color = 'darkred')




