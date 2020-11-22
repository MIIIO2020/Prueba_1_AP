library(data.table)
library(fitdistrplus)
library(goftest)
library(stats)
library(caret)
library(ggplot2)
library(dataQualityR)
library(Amelia)
library(ggfortify)
library(gridExtra)
library(grid)
library(alluvial)
library(RColorBrewer)
library(corrplot)
library(Matrix)
library(dplyr)
library(MASS)
library(glmnet)
library(BLR)
library(lubridate)
library(data.table)
library(DMwR)
library(MASS)
library(survival)
library(lattice)
library(ggplot2)
library(MLmetrics)
library(car)


#Load the data
train.data=read.csv("/Users/antoniaindaorlandi/Desktop/Prueba Análisis Predictivo/data/data/20949_Train.csv",header=TRUE, sep=",")

test.data=read.csv("/Users/antoniaindaorlandi/Desktop/Prueba Análisis Predictivo/data/data/20949_Test.csv",header=TRUE, sep=",")

F_date<-function(data_X){
  
  #Ajusta los atributos
  data_X$X = NULL
  data_X$date = NULL
  data_X$shop_id=as.factor(data_X$shop_id)
  
  # mueve la columna de i tem_cnt_day
  # Solo incluye los 50 items con mas datos en el modelo
  #data_X <- data_X[,c(2,1,3:404)]
  
  data_X$item_price_Y<-sapply(data_X$item_price_Y, function(x) ifelse(x>0,log(x), x ))
  #data_X$item_cnt_day<-sapply(data_X$item_cnt_day, function(x) ifelse(x>0,log(x), x ))
  # mueve la columna de i tem_cnt_day
  # Solo incluye los 50 items con mas datos en el modelo
  N=ncol(data_X)
  data_X <- data_X[,c(2,1,3:51)]
}

test.data<-F_date(test.data)
train.data<-F_date(train.data)

test.data <- test.data[test.data$shop_id!="36",]


### Predictor variables

x <- model.matrix(item_cnt_day~., train.data)[,-1]

### Outcome variable

y <- train.data$item_cnt_day


### New predictor variables

#newX<- model.matrix(item_cnt_day~., test.data)[,-1]

### New outcome variable

#newY<- test.data$item_cnt_day



#Generación del modelo

set.seed(123)
lm_model <- train(
  item_cnt_day ~., data = train.data, method = "lm",
  trControl = trainControl("cv", number = 10),
  tuneLength = 5
)


#Coeficientes del modelo

coef(lm_model$finalModel)


#Predicciones


predictions <- lm_model %>% predict(test.data)


#Desempeño de las predicciones del modelo


data.frame(
  RMSE = RMSE(predictions, test.data$item_cnt_day),
  Rsquare = R2(predictions, test.data$item_cnt_day),
  MAPE = MAPE(predictions, test.data$item_cnt_day)
)



#Variables utilizadas


varsSelected <- length(coef(lm_model$finalModel))
cat('Regresión Lineal Multiple usa', varsSelected, 'variables en su modelo')


#  Colinealidad (No usar si las variables son tipo factor)
#Para modelo con todas las variables (Regresión lineal múltiple)
X_aux <-train.data[,-c(1)]

#Descomposición de la matriz X'X
XtX <- t(X_aux)%*%as.matrix(X_aux)
s <- svd(XtX)

# Si NC >25 -> colinealidad
#NC número de condición
#svd calcula los valores de la matriz de X'X (separandolos en 3 submatrices, nos interesará la d) en particular svd$d entrega los valores únicos de X

#NC para modelo de regresión lineal múltiple
NC <- sqrt(max(s$d)/min(s$d)) 
NC

#Es para este tipos de resultados que luego se utilizan regresiones Ridge, para regularizar la matriz X'X
#Otra opción de para analizar la multicolinealidad es utilizar la función VIF
#vif(outputIIa)
#Para valores grandes se dice que hay multicolinealidad con más de dos variables con valores elevados 
#O se puede comparar con el resultado de 1/(1-R^2) del modelo


lambda <- 10^seq(-3, 3, length = 10)

# a. Ridge

### Build the model
set.seed(123)
ridge <- train(
  item_cnt_day ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 5),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

### Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)

### Make predictions
predictions <- ridge %>% predict(test.data)

### Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$item_cnt_day),
  Rsquare = R2(predictions, test.data$item_cnt_day),
  MAPE = MAPE(predictions, test.data$item_cnt_day)
)

varsSelected <- length(which(coef(ridge$finalModel, ridge$bestTune$lambda)!=0))
varsNotSelected <- length(which(coef(ridge$finalModel, ridge$bestTune$lambda)==0))
cat('Ridge uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')

#Trazas ridge (No son las de la prueba)
#plot(ridge, main="Ridge")
#plot(ridge, xvar="lambda")


# b. Lasso
#(alpha debe fijarse en 1 para Lasso)
### Build the model
set.seed(123)
lasso <- train(
  item_cnt_day ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 5),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

### Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)

### Make predictions
predictions <- lasso %>% predict(test.data)

### Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$item_cnt_day),
  Rsquare = R2(predictions, test.data$item_cnt_day),
  MAPE = MAPE(predictions, test.data$item_cnt_day)
)

varsSelected <- length(which(coef(lasso$finalModel, lasso$bestTune$lambda)!=0))
varsNotSelected <- length(which(coef(lasso$finalModel, lasso$bestTune$lambda)==0))
cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')

#Trazas lasso (No son las de la prueba)
#plot(lasso, main="LASSO")
#plot(lasso, xvar="lambda")


# c. Elastic net

### Build the model
set.seed(123)
elastic <- train(
  item_cnt_day ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
### Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)

#best alpha
best_alpha_EN<-elastic$bestTune$alpha

### Make predictions
predictions <- elastic %>% predict(test.data)

### Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$item_cnt_day),
  Rsquare = R2(predictions, test.data$item_cnt_day),
  MAPE = MAPE(predictions, test.data$item_cnt_day)
)

varsSelected <- length(which(coef(elastic$finalModel, elastic$bestTune$lambda)!=0))
varsNotSelected <- length(which(coef(elastic$finalModel, elastic$bestTune$lambda)==0))
cat('ElasticNet uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')


#Trazas elastic net (No son las de la prueba)
#plot(elastic, main="Elastic Net")
#plot(elastic, xvar="lambda")


#Trazas V2 (Las que el profesor pidió para la prueba…)

lasso = glmnet(x, y,alpha = 1)

ridge = glmnet(x, y, alpha = 0)

elastic = glmnet(x, y, alpha = best_alpha_EN)

plot(ridge,main="Trazas Ridge", xvar="lambda")

plot(lasso, main="Trazas Lasso",xvar="lambda")

plot(elastic, main="Trazas Elastic Net", xvar="lambda")

