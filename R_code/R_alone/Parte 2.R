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


tr = distinct(train.data,train.data$shop_id)
te = distinct(test.data,test.data$shop_id)

tr<-unlist(tr, use.names = FALSE)
te<-unlist(te, use.names = FALSE)

inter<-intersect(te,tr)

train.data<-train.data[train.data$shop_id %in% inter,]

test.data<-test.data[test.data$shop_id %in% inter,]


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

#test.data <- test.data[test.data$shop_id!="36",]

#Crear variables dummy
charVars = c("shop_id")

#train
train_dummy = train.data
f_tr = as.formula(paste("~", paste(colnames(train_dummy[charVars]), collapse = " + ")))
train_dummy[charVars] = lapply(train_dummy[charVars], function(x) as.factor(x))

dummy_feat_tr =dummyVars(f_tr, data = train_dummy, fullRank = TRUE)
dummy_feat_pred_tr = predict(dummy_feat_tr, train_dummy)

# get binary features                               
binaryVars_tr = c(colnames(dummy_feat_pred_tr))

train_dummy = cbind(train_dummy[,!colnames(train_dummy) %in% charVars], dummy_feat_pred_tr)
aaa_tr = c(binaryVars_tr, "item_cnt_day")
scaled_data_tr = cbind(scale(train_dummy[,!colnames(train_dummy) %in% aaa_tr]),train_dummy[aaa_tr])    

write.csv(scaled_data_tr, '/Users/antoniaindaorlandi/Desktop/Prueba Análisis Predictivo/data/data/scaled_data_tr.csv', row.names = FALSE)

model_data_aux_tr = scaled_data_tr[,!colnames(train_dummy) %in% charVars]
model_data_tr = model_data_aux_tr[!is.na(model_data_aux_tr$item_cnt_day),] 
model_data_tr[is.na(model_data_tr)] <- 0

train.data <- model_data_tr

#test
test_dummy = test.data
f_te = as.formula(paste("~", paste(colnames(test_dummy[charVars]), collapse = " + ")))
test_dummy[charVars] = lapply(test_dummy[charVars], function(x) as.factor(x))

dummy_feat_te =dummyVars(f_te, data = test_dummy, fullRank = TRUE)
dummy_feat_pred_te = predict(dummy_feat_te, test_dummy)

# get binary features                               
binaryVars_te = c(colnames(dummy_feat_pred_te))

test_dummy = cbind(test_dummy[,!colnames(test_dummy) %in% charVars], dummy_feat_pred_te)
aaa_te = c(binaryVars_te, "item_cnt_day")
scaled_data_te = cbind(scale(test_dummy[,!colnames(test_dummy) %in% aaa_te]),test_dummy[aaa_te])    

write.csv(scaled_data_te, '/Users/antoniaindaorlandi/Desktop/Prueba Análisis Predictivo/data/data/scaled_data_te.csv', row.names = FALSE)

model_data_aux_te = scaled_data_te[,!colnames(test_dummy) %in% charVars]
model_data_te = model_data_aux_te[!is.na(model_data_aux_te$item_cnt_day),] 
model_data_te[is.na(model_data_te)] <- 0

test.data<-model_data_te



### Predictor variables

x <- model.matrix(log(item_cnt_day)~., train.data)[,-1]

### Outcome variable

y <- train.data$item_cnt_day


### New predictor variables

#newX<- model.matrix(item_cnt_day~., test.data)[,-1]

### New outcome variable

#newY<- test.data$item_cnt_day

#Generación del modelo

set.seed(123)
lm_model <- train(
  log(item_cnt_day) ~., data = train.data, method = "lm",
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
X_aux <-test.data[,-c(86)]

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

null<-lm(log(item_cnt_day)~1, data=train.data)
full<-lm(log(item_cnt_day)~., data=train.data)
outputIIa<-step(null, scope = list(upper=full), data=train.data, direction="both")
summary(outputIIa)
modelIIa =  outputIIa$call$formula

#Coeficientes del modelo

coef(outputIIa)


#Predicciones


predictions <- outputIIa %>% predict(test.data)


#Desempeño de las predicciones del modelo


data.frame(
  RMSE = RMSE(predictions, test.data$item_cnt_day),
  Rsquare = R2(predictions, test.data$item_cnt_day),
  MAPE = MAPE(predictions, test.data$item_cnt_day)
)


#  Colinealidad
X_aux_SF<-test.data[,-c(1,2,4,6,8,10,11,25,33,41,43,44,49,50,51,54,71,73,74,77,78,80,85,86)]

#Descomposición de la matriz X'X para modelo Stepwise Forward
XtX_SF <- t(X_aux_SF)%*%as.matrix(X_aux_SF)
s_SF <- svd(XtX_SF)

# Si NC >25 -> colinealidad
#NC número de condición
#svd calcula los valores de la matriz de X'X (separandolos en 3 submatrices, nos interesará la d) en particular svd$d entrega los valores únicos de X

#NC para Stepwise Forward
NC_SF <- sqrt(max(s_SF$d)/min(s_SF$d)) 
NC_SF


lambda <- 10^seq(-3, 3, length = 100)


# a. Ridge

### Build the model
set.seed(123)
ridge <- train(
  log(item_cnt_day) ~., data = train.data, method = "glmnet",
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
  log(item_cnt_day) ~., data = train.data, method = "glmnet",
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
  log(item_cnt_day) ~., data = train.data, method = "glmnet",
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


#f elasticidad precio demanda

set.seed(123)
Elasticidad <- train(
  log(item_cnt_day) ~ item_price_Y, data = train.data ,method = "lm",
  trControl = trainControl("cv", number = 10),
  tuneLength = 5
)
summary

### Model coefficients
#coef(Elasticidad$finalModel, Elasticidad$bestTune$lambda)

### Make predictions
predictions_Elasticidad <- Elasticidad %>% predict(test.data)

### Model prediction performance
data.frame(
  RMSE = RMSE(predictions_Elasticidad, test.data$item_cnt_day),
  Rsquare = R2(predictions_Elasticidad, test.data$item_cnt_day),
  MAPE = MAPE(predictions_Elasticidad+1, test.data$item_cnt_day+1)
)
