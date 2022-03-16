library(rpart.plot)
library(ModelMetrics)
library(ggplot2)
library(rpart)
library(randomForest)

#ejercicio2
percent<-70/100
set.seed(123)
datos<-read.csv('train.csv')
percentil <- quantile(datos$SalePrice)
View(datos)

estado<-c('Estado')
datos$Estado<-estado

datos <- within(datos, Estado[SalePrice<=129975] <- 0)
datos$Estado[(datos$SalePrice>129975 & datos$SalePrice<=163000)] <- 1
datos$Estado[datos$SalePrice>163000] <- 2
corte <- sample(nrow(datos),nrow(datos)*percent)

#datos entrenados
train<-datos[corte,]
test<-datos[-corte,]

#Regresion lineal de los datos
fitLMPW<-lm(SalePrice~ ., data = train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])
predL<-predict(fitLMPW, newdata = test)

#prediccion
resultados<-data.frame(test$SalePrice,predL)
ej2<-head(resultados, n=5)
graf1<-ggplot(datos=train,mapping = aes(x=SalePrice,y=GrLivArea ))+
  geom_point(color='red',size=2)+
  geom_smooth(method = 'lm',se=TRUE,color='black')+
  labs(title = 'Precio de venta ~ Pies cuadrados de vivienda',x="Precio de venta",y='Pies cuadrados de venta')+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))

#ejercicio3
pairs(datos$SalePrice ~ datos$GrLivArea)
pairs(datos$SalePrice ~ datos$YearBuilt)
pairs(datos$SalePrice ~ datos$BsmtUnfSF)
pairs(datos$SalePrice ~ datos$TotalBsmtSF)
pairs(datos$SalePrice ~ datos$GarageArea)
pairs(datos$SalePrice ~ datos$YearRemodAdd)
pairs(datos$SalePrice ~ datos$LotArea)

ggplot(datos=train,mapping = aes(x=SalePrice,y=GrLivArea ))+
  geom_point(color='red',size=2)+
  geom_smooth(method = 'lm',se=TRUE,color='black')+
  labs(title = 'Precio de venta ~ Pies cuadrados de vivienda',x="Precio de venta",y='Pies cuadrados de venta')+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))

predL<-predict(fitLMPW, newdatos = test)

summary(predL)
summary(test$SalePrice)

plot(fitLMPW)

boxplot(fitLMPW$residuals)

fitLMPW2 <- replace_outliers(fitLMPW$residuals)
par(mfrow = c(1,2))

boxplot(fitLMPW2, main = "Sin datos atipicos",col=6)
hist(fitLMPW2)


plot(test$SalePrice, test$LotArea)
points(predL, test$LotArea, col="red",pch=15)

plot(test$SalePrice, test$LotArea)
points(predL, test$LotArea, col="red",pch=15)
 
cor(datos$SalePrice,datos[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd","LotArea")], method = "spearman")


cor(datos$SalePrice,datos$YearBuilt, method = "spearman")

cor(datos$SalePrice,datos$GarageArea, method = "spearman")

### Comparacion con el arbol de regresion:

datos<-read.csv('train.csv') 
df<-datos[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "SalePrice")]
m1<-rpart(SalePrice ~ ., datos = df, method = "anova")

rpart.plot(m1, type = 3, digits = 3, fallen.leaves = TRUE )



