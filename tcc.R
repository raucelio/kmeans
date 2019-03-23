dados <- readRDS("enem_2017_df_media.rds")

tipo_escola <- dados[, 2]

dados <- dados[,-c(1,2)] 

wss <- (nrow(dados)-1)*sum(apply(dados,2,var))

for (i in 1:15) wss[i] <- sum(kmeans(dados, 
                                     
                                     centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Número de Cluster",
     
     ylab="Soma dos Quadrados Dentros dos grupos")



fit <- kmeans(dados, 2) 



aggregate(dados,by=list(fit$cluster),FUN=mean)







library(gmodels)

CrossTable(tipo_escola,fit$cluster,prop.chisq = F)

table(tipo_escola,fit$cluster)

cor1 <- rgb(1,0,0,0.5)

cor2 <- rgb(0,0,1,0.5)

cor3 <- rgb(0,1,0,0.5)

cores <- c(cor1,cor2,cor3)



fit2 <- prcomp(dados)$x

plot(fit2[,1:2],col=cores[fit$cluster],pch=tipo_escola,cex=0.8)

text(fit2[,1], fit2[,2], labels=tipo_escola, col=fit$cluster, cex=0.9)


library(ggplot2)
library(ggfortify)

x <- kmeans(dados, 3)

autoplot(x, 
         data = dados, 
         frame = TRUE, label.size = 3)

table(x$cluster,tipo_escola)
