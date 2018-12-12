# Trabalho Estatística - Análise Multivariada 09 - 08/12/2018

# Alexandre Aguiar - RA 1859446-8
# Elizoneide Noia  - RA 1864791-0
# Marcelo Noia     - RA 1865093-7



# Análise de Cluster
#  R tem uma incrível variedade de funções para análise de cluster . Nesta seção, descreverei três das 
#  muitas abordagens: aglomerativa hierárquica, particionamento e modelo baseado. Embora não haja melhores 
#  soluções para o problema de determinar o número de clusters a serem extraídos, várias abordagens são 
#  fornecidas abaixo.
#  Preparação de dados antes de agrupar os dados, você pode querer remover ou estimar dados ausentes e 
#  redimensionar as variáveis para fins de comparação.



### INICIO DO EXEMPLO

mydata <- mtcars
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

### FIM DO EXEMPLO


# Particionamento

# K-means clustering é o método de particionamento mais popular. 
# Isso requer que o analista especifique o número de clusters a serem extraídos. 
# Um gráfico da soma dos quadrados dentro dos grupos pelo número de clusters extraídos pode 
# ajudar a determinar o número apropriado de clusters. O analista procura uma curva na trama 
# semelhante a um teste de scree na análise fatorial. Veja Everitt & Hothorn (pg. 251) .

# K-Means Cluster Analysis

### INICIO DO EXEMPLO
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)
### FIM DO EXEMPLO


# Uma versão robusta de K-means baseada em mediods pode ser invocada usando pam ()
# em vez de kmeans () . A função pamk () no pacote fpc é um wrapper para pam que também 
# imprime o número sugerido de clusters com base na largura média ideal da silhueta.

### INICIO DO EXEMPLO
install.packages("fpc")
library(fpc)

fit <- pamk(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)
### FIM DO EXEMPLO


# Aglomerativa Hierárquica
# Existe uma ampla gama de abordagens de clustering hierárquico. Eu tive boa
# sorte com o método de Ward descrito abaixo.


### INICIO DO EXEMPLO
# Ward Hierarchical Clustering

d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
### FIM DO EXEMPLO


# A função pvclust () no pacote pvclust fornece valores p para o agrupamento
#hierárquico baseado em reamostragem de bootstrap multiescala. Os clusters
#altamente suportados pelos dados terão grandes valores p. Interpretação detalhes
#são fornecidos Suzuki . Esteja ciente de que pvclust clusters colunas, não linhas.
#Transponha seus dados antes de usar.


### INICIO DO EXEMPLO
# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)
### FIM DO EXEMPLO

#Baseado em modelo
#Abordagens baseadas em modelos assumem uma variedade de modelos de dados e aplicam estimativas de máxima verossimilhança e critérios de Bayes para identificar o modelo e o número de clusters mais prováveis. Especificamente, a função Mclust () no pacote mclust seleciona o modelo ideal de acordo com o BIC para EM inicializado por clustering hierárquico para modelos de mistura gaussiana parametrizada. (ufa!) Escolhe-se o modelo e o número de clusters com o maior BIC. Veja help (mclustModelNames) para detalhes sobre o modelo escolhido da melhor forma.


### INICIO DO EXEMPLO
# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model
### FIM DO EXEMPLO


#agrupamento baseado em modelo gráficos de dispersão de cluster clique para ver

#Plotando Soluções de Cluster
#É sempre uma boa ideia observar os resultados do cluster.

### INICIO DO EXEMPLO
# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)
### FIM DO EXEMPLO


#clusplot parcela discriminante clique para ver

# Validando soluções de cluster
# A função cluster.stats () no pacote fpc fornece um mecanismo para comparar
# a similaridade de duas soluções de cluster usando uma variedade de critérios 
# de validação (o coeficiente de gama de Hubert, o índice de Dunn e o índice de rand corrigido)

# comparing 2 cluster solutions


### INICIO DO EXEMPLO
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)
### FIM DO EXEMPLO


# onde d é uma matriz de distância entre objetos e fit1 $ cluster e fit $ cluste r são
# vetores inteiros contendo resultados de classificação de dois agrupamentos diferentes
# dos mesmos dados
