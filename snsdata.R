
dados1 <- read.csv("snsdata.csv",sep=",",dec=".")
dados  <- dados1 
dados  <- na.omit(dados[,-c(1,2,3,4)])   
dados  <- scale(dados)              



wss <- (nrow(dados)-1)*sum(apply(dados,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(dados, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de Cluster",
     ylab="Soma dos Quadrados Dentros dos grupos")

fit <- kmeans(dados, 4) 


dados1$classe <- fit$cluster



sort(colSums(dados1[dados1$classe==1, 5:40]),decreasing =  T)
sort(colSums(dados1[dados1$classe==2, 5:40]),decreasing =  T)
sort(colSums(dados1[dados1$classe==3, 5:40]),decreasing =  T)
sort(colSums(dados1[dados1$classe==4, 5:40]),decreasing =  T)


barplot(sort(colSums(dados1[dados1$classe==1, 5:40]), decreasing =  T )[1:10], horiz=T,las=2, cex.names = 0.6)
barplot(sort(colSums(dados1[dados1$classe==2, 5:40]), decreasing =  T )[1:10], horiz=T,las=2,cex.names = 0.6)
barplot(sort(colSums(dados1[dados1$classe==3, 5:40]), decreasing =  T )[1:10], horiz=T,las=2,cex.names = 0.6)
barplot(sort(colSums(dados1[dados1$classe==4, 5:40]), decreasing =  T )[1:10], horiz=T,las=2,cex.names = 0.6)


fit2 <- prcomp(dados)$x
plot(fit2[,1:2],col=fit$cluster,pch=16)

