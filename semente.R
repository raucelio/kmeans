
dados <- read.csv("semente.csv",sep=";",dec=",")
especie <- dados[, 9]
dados <- na.omit(dados[,-c(1,9)])   
dados <- scale(dados)              



wss <- (nrow(dados)-1)*sum(apply(dados,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(dados, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de Cluster",
     ylab="Soma dos Quadrados Dentros dos grupos")

fit <- kmeans(dados, 3) 

aggregate(dados,by=list(fit$cluster),FUN=mean)



library(gmodels)
CrossTable(especie,fit$cluster,prop.chisq = F)

cor1 <- rgb(1,0,0,0.5)
cor2 <- rgb(0,0,1,0.5)
cor3 <- rgb(0,1,0,0.5)
cores <- c(cor1,cor2,cor3)

fit2 <- prcomp(dados)$x
plot(fit2[,1:2],col=cores[fit$cluster],pch=16,cex=0.8)
#text(fit2[,1], fit2[,2], especie, col=fit$cluster, cex=0.9)


