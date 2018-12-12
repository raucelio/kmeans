dados <- iris
cor(dados[,-5])
set.seed(12345)
f <- kmeans(dados[,-5],3)
table(dados$Species, f$cluster)
wss <- NULL
for (i in 1:15) wss[i] <- sum(kmeans(dados[,-5], centers=i)$withinss)
plot(1:15, wss, type="l", xlab="Numero de Cluster",
     ylab="Soma dos Quadrados Dentro", axes=F)
points(1:15, wss, pch=16, col="red")
axis(1, 1:15, las=2)
axis(2,seq(0,700,by=100))
fit <- prcomp(dados[,-5])$x
plot(fit,col=f$cluster,pch='.')
text(fit[,1], fit[,2], substr(dados[,5],1,2), col=f$cluster, cex=0.9)