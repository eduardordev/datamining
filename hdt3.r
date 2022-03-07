library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(ggpubr)

library(cluster) 
library(e1071)
library(mclust) 
library(fpc) 
library(NbClust) 
library(factoextra)

library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)

datosTrain <- read.csv("C:/Users/Mustella 3D/Desktop/train.csv")
theme_set(theme_pubr())
# Ejercicio 3

wss <- (nrow(datosTrain[,c(1,2,44:53,81)])-1)*sum(apply(datosTrain[,c(1,2,44:53,81)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosTrain[,c(1,2,44:53,81)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")# Grafica para concluir la cantidad de clusters

#se concluye que son 4 clusters, el metodo a utilizar es k means

conjunto<-datosTrain
datosCompleto<-datosTrain[complete.cases(conjunto),]
km<-kmeans(datosTrain[,c(1,2,44:53,81)],4,iter.max =100)
conjunto$grupo<-km$cluster

g1<- conjunto[conjunto$grupo==1,]
prop.table(table(g1$SalePrice))*100
nrow(g1)
summary(g1)

g2<- conjunto[conjunto$grupo==2,]
prop.table(table(g2$SalePrice))*100
g3<- conjunto[conjunto$grupo==3,]
prop.table(table(g3$SalePrice))*100
g4<- conjunto[conjunto$grupo==4,]
prop.table(table(g4$SalePrice))*100

plotcluster(datosTrain[,c(1,2,44:53,81)],km$cluster) #grafica de k means




#resumen de cada grupo del cluster
summary(g1)
summary(g2)
summary(g3)
summary(g4)

# resumen del dataset
summary(datosTrain$SalePrice)

#arboles de decision
theme_set(theme_pubr())
porciento <- 70/100

set.seed(123)

trainRowsNumber<-sample(1:nrow(datosTrain),porciento*nrow(datosTrain))
train<-datosTrain[trainRowsNumber,]
test<-datosTrain[-trainRowsNumber,]
arbolModeloRegresion<-rpart(SalePrice~.,train,method = "anova")
rpart.plot(arbolModeloRegresion)
#Se crea el test y se clasifica
head(test)
#Se crea la variable respuesta en base a la clasificacion hipotetica
datosTrain$Clasificacion <- ifelse(datosTrain$SalePrice <=152000, "Economica",
                                   ifelse(datosTrain$SalePrice <=229000, "Intermedia", "Cara"))



trainRowsNumber<-sample(1:nrow(datosTrain),porciento*nrow(datosTrain))
#conjuntos de entramiento y prueba
train<-datosTrain[trainRowsNumber,]
test<-datosTrain[-trainRowsNumber,]
arbolModeloClasificacion<-rpart(Clasificacion~.,train,method = "class")
#se muestra graficamente el modelo de clasificacion
rpart.plot(arbolModeloClasificacion)

save(train,test,arbolModeloClasificacion, file = "Variables.RData")
load("Variables.RData")
train2<- data.frame(train$SalePrice,train$Clasificacion)
colnames(train2)[2]<-"Clasificacion"
colnames(train2)[1]<-"SalePrice"
dt_model<-rpart(Clasificacion~.,train2,method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)
#Se crea el test y se clasifica
head(test)
aa <- data.frame(test$SalePrice)
colnames(aa)[1]<-"SalePrice"
aa2 <-  subset(test, select=-c(Condition1))

prediccion <- predict(dt_model,aa, se.fit=FALSE)


columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])

test$prediccion<-columnaMasAlta 

cfm<-confusionMatrix(as.factor(test$prediccion),as.factor(test$Clasificacion))

#Forest
library(haven)
library(dplyr)

datosTrain[sapply(datosTrain, is.character)] <- lapply(datosTrain[sapply(datosTrain, is.character)], as.factor)

porciento <- 70/100
set.seed(123)
trainRowsNumber<-sample(1:nrow(datosTrain),porciento*nrow(datosTrain))
train<-datosTrain[trainRowsNumber,]
test<-datosTrain[-trainRowsNumber,]

train <- train[, colSums(is.na(train)) == 0]
test <- test[, colSums(is.na(test)) == 0]
modeloRF1<-randomForest(as.factor(Clasificacion)~., data=train)

prediccionRF1<-predict(modeloRF1,newdata = test[,1:63])
testCompleto<-test
testCompleto$predRF<-prediccionRF1

cfmRandomForest <- confusionMatrix(testCompleto$predRF, as.factor(testCompleto$Clasificacion))

