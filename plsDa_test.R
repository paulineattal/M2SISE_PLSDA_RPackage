infection <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
CRP <- c(40.0, 11.1, 30.0, 21.4, 10.7, 3.4, 42.0, 31.1, 50.0, 60.4, 45.7, 17.3)
Temp <- c(36.0, 37.2, 36.5, 39.4, 39.6, 40.7, 37.6, 42.2, 38.5, 39.4, 38.6, 42.7)



df <- data.frame(infection, CRP, Temp)
dfcenter <- df
dfcenter[,2:3] <- scale(dfcenter[,2:3], scale=F)
alphas <- eigen(cov(dfcenter[,2:3]))$vectors[,1]
df['LV1'] <- df[,2]*alphas[1] + df[,3]*alphas[2]


coef <- lm(infection~LV1, data=df)$coefficients
coef[1]
df['pred'] <- coef[1] + df['LV1']*coef[2]
df
