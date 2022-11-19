#plotting graphs with ggplot, with a different dataset


#dans notre cas, on utilisera la matrice des coefficients pour X
testdat <- lol.sims.cigar(n=400, d=30)
testdat

View(testdat)
X <- testdat$X
Y <- testdat$Y
X


View(X)
View(Y)
View(X)
data <- data.frame(x1=X[,1], x2=X[,2], y=Y)
data$y <- factor(data$y)
ggplot(data, aes(x=x1, y=x2, color=y)) +
  +     geom_point() +
  +     xlab("x1") +
  +     ylab("x2") +
  +     ggtitle("Simulated Data")
result <- lol.project.pls(X, Y, r)


data$y <- factor(data$y)
ggplot(data, aes(x=x1, y=x2, color=y)) +
  +     geom_point() +
  +     xlab("x1") +
  +     ylab("x2") +
  +     ggtitle("Projected Data using PLS")

