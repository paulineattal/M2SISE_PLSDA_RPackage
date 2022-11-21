data


### faire une fonction de stat desc du jeu de donnees ???

ggplot(test, aes(x = R2)) +
  geom_point()
#+theme()
#+stat_smooth()
#+

ggplot(data, aes(parameter , fill = seed)) +
  geom_bar(position = "dodge2") +
  coord_flip()








ggplot(rescol, aes(x=X1, y=X2, col = y, fill = y)) +
  stat_ellipse(geom = "polygon", col= "black", alpha =0.5)+
  geom_point(shape=21, col="black")


pouette = as.data.frame(test$Xloading.weights)
ggplot() +  
  geom_text(data=pouette, aes(x = X1, y = X2, label = rownames(pouette)), col = 'red') +
  geom_segment(data=pouette, aes(x = 0, y = 0, xend = X1, yend = X2), arrow=arrow(length=unit(0.2,"cm")),alpha = 0.75, color = 'darkred')




