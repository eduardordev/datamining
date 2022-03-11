library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering
library(hopkins) #Para revisar si vale la pena hacer agrupamiento
library(GGally) #Para hacer el conjunto de graficos
library(FeatureImpCluster) #Para revisar la importancia de las variables en los grupos.
library(pheatmap) #Para hacer mapa de calor

datos<-read.csv("movies.csv")
datos<-datos[complete.cases(read.csv("movies.csv")),]
popular<-datos[,'popularity']
budget<-datos[,'budget']
revenue<-datos[,'revenue']
runtime<-datos[,'runtime']
votes<-datos[,'voteCount']
cl<-data.frame(popular,budget,revenue,runtime,votes)
clustering<-scale(cl)
hopkins(clustering)

datos<-dist(datos)
fviz_dist(datos, show_labels = F)
