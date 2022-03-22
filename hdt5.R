library(e1071)
library(caret)
porcentaje<-0.7
datos <- read.csv("C:/Users/Mustella 3D/Desktop/train.csv")
datos$Clasificacion <- ifelse(datos$SalePrice <=152000, "Barata", ifelse(datos$SalePrice <=229000, "Media", "Cara"))
datos$Clasificacion <- as.factor(datos$Clasificacion)
datos$SalePrice<- NULL
datos$Id<-NULL

datos$GarageYrBlt <- datos$YearBuilt
datos$LotFrontage[is.na(datos$LotFrontage)] <- 69
datos$MasVnrArea[is.na(datos$MasVnrArea)]<-0

set.seed(123)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]
modelo<-naiveBayes(train$Clasificacion~., data=train)
modelo 

summary(modelo)

predBayes<-predict(modelo, newdata = test[,1:80])
cm<-caret::confusionMatrix(predBayes,test$Clasificacion)
cm 
library(rpart)
library(caret)
library(tree)
library(rpart.plot)

dt_model<-rpart(Clasificacion~.,train,method = "class")

plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)

datos<-subset(datos, select = -c(6,25,30:33,35,42,57,58,60,63,64,72,73,74) )

set.seed(123)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]
modelo<-naiveBayes(train$Clasificacion~., data=train)
predBayes<-predict(modelo, newdata = test[,1:64])
cm<-caret::confusionMatrix(predBayes,test$Clasificacion)
cm 
