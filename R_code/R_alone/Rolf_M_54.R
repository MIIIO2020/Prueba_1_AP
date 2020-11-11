#install.packages("dataQualityR")
#install.packages("Amelia")
#install.packages("ggfortify")
#install.packages("alluvial")
#install.packages("BLR")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("caret")
#install.packages("glmnet")
#install.packages("dplyr")
#install.packages("labelVector")
#install.packages("MLmetrics")

library(ggplot2)
library(dataQualityR)
#library(Amelia)
library(ggfortify)
library(gridExtra)
library(grid)
library(alluvial)
library(RColorBrewer)
library(corrplot)
library(Matrix)
library(dplyr)
library(MASS)
library(caret)
library(glmnet)
library(BLR)
library(lubridate)
library(data.table)
library(DMwR)
library(dplyr)
library(labelVector)
library(MLmetrics)

#Descarga la data sale_complete.csv
data_test=fread('C:/Users/rolft/Documents/UAI/
                UAI_2020/AP_git/me_code/Prueba_1/
                data/
                20949__Test.csv')
data_train=fread('C:/Users/rolft/Documents/UAI/UAI_2020/
                 AP_git/me_code/Prueba_1/data/
                 20949_Train_2.csv')


## Funcion de Data####

F_date<-function(data_X){

  #Ajusta los atributos
  data_X$V1 = NULL
  data_X$date = NULL
  data_X$shop_id=as.factor(data_X$shop_id)
   
  # mueve la columna de i tem_cnt_day
  # Solo incluye los 50 items con mas datos en el modelo
  data_X <- data_X[,c(2,1,3:404)]
}

data_test<-F_date(data_test)
data_train<-F_date(data_train)
rm(F_date)



## Reci?n ahora empecemos a resolver la prueba...####

          #revisar si tiene datos NA
          # No tiene ningun NA
          #data_train$item_category_id=as.integer(data_train$item_category_id)
          # tengo que quitarle el formato de factor a item_category_id

# Preg 2. Regresion Lineal Multiple



## a. Selecci?n Stepwise forward ####

null<-lm((item_cnt_day)~1, data=data_train)
full<-lm(item_cnt_day~., data=data_train)

        #error presente si existen NA o un factor 
        #con solo una categoria
        #esto sucede con item_category_id
        # Para ello cambie el formato a entero, 
        #esto solucionó el problema


output2a<-step(null, scope = list(upper=full), 
               data=data_train, direction="both")
#nunca eliminar
summary(output2a)

rm(full,null)


#se contruye un modelo utilizando lo generado por la función step

 model_forward =  output2a$call$formula


### Build the model of  Stepwise forward ####

set.seed(123)
M_forward= train( model_forward , data=data_train ,method = "lm",
  trControl = trainControl("cv", number = 10),
  tuneLength = 5
  )


### Model coefficients
coef(M_forward$finalModel)


### Make predictions
data_test = data_test[shop_id!="36",]

predictions_Forward <- M_forward %>% predict(data_test)

       # "ocurre un erro el si se ejecuta, debido a que hay valores NA, 
       #  
       #  Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
       #  factor shop_id has new levels 36 "
       # Para solusionar esto vere que categorias estan presentes en cada una de las dos datas


### Model prediction performance
Score_Forward=data.frame(
  RMSE = RMSE(predictions_Forward, data_test$item_cnt_day),
  Rsquare = R2(predictions_Forward, data_test$item_cnt_day),
  #MAPE no funciona con valores 0, por lo cual  se +1 para que funcione.
  MAPE= MAPE( predictions_Forward+1,data_test$item_cnt_day+1)
  )

varsSelected <- length(coef(M_forward$finalModel))
cat('Stepwise forward',' uses', varsSelected, 'variables in its model')
rm(varsSelected)





### Build the full model or classic ####
set.seed(123)
lm_model <- train(
  item_cnt_day ~., data = data_train, method = "lm",
  trControl = trainControl("cv", number = 10),
  tuneLength = 5
)

### Model coefficients
coef(lm_model$finalModel)


### Make predictions
predictions_lm_model <- lm_model %>% predict(data_test)

### Model prediction performance
score_lm_model=data.frame(
  RMSE = RMSE(predictions_lm_model, data_test$item_cnt_day),
  Rsquare = R2(predictions_lm_model, data_test$item_cnt_day),
  MAPE= MAPE( predictions_lm_model+1,data_test$item_cnt_day+1)
)

varsSelected <- length(coef(lm_model$finalModel))
cat('Model Clasic  uses', varsSelected, 'variables in its model')
rm(varsSelected)


rm(lm_model)

### Parte 2 B ####


# Ridge

### Build the model

lambda <- 10^seq(-3, 2, length = 100)
lambda = seq(-6,6,0.1)


set.seed(123)
ridge <- train(
  item_cnt_day ~., data = data_train, method = "glmnet",
  trControl = trainControl("cv", number = 5),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

### Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)

### Make predictions
predictions_ridge <- ridge %>% predict(data_test)

### Model prediction performance
Score_Ridge=data.frame(
  RMSE = RMSE(predictions_ridge, data_test$item_cnt_day),
  Rsquare = R2(predictions_ridge, data_test$item_cnt_day),
  MAPE = MAPE(predictions_ridge+1, data_test$item_cnt_day+1)
)

varsSelected <- length(which(coef(ridge$finalModel, ridge$bestTune$lambda)!=0))
varsNotSelected <- length(which(coef(ridge$finalModel, ridge$bestTune$lambda)==0))
cat('Ridge uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')

#Trazas ridge
plot(ridge, main="Ridge", xvar="lambda")

## Parte 2 c####
# b. Lasso
#(alpha debe fijarse en 1 para Lasso)
### Build the model
lambda <- 10^seq(-3, 1, length = 100)
lambda = seq(-6,6,0.1)

set.seed(123)
lasso <- train(
  item_cnt_day ~., data = data_train, method = "glmnet",
  trControl = trainControl("cv", number = 5),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

### Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)

### Make predictions
predictions_lasso <- lasso %>% predict(data_test)

### Model prediction performance
Score_Lasso=data.frame(
  RMSE = RMSE(predictions_lasso, data_test$item_cnt_day),
  Rsquare = R2(predictions_lasso, data_test$item_cnt_day),
  MAPE = MAPE(predictions_lasso+1, data_test$item_cnt_day+1)
)

varsSelected <- length(which(coef(lasso$finalModel, lasso$bestTune$lambda)!=0))
varsNotSelected <- length(which(coef(lasso$finalModel, lasso$bestTune$lambda)==0))
cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')
rm(varsSelected)
#Trazas lasso

plot( lasso, main="LASSO",xvar="lambda")


## Parte 2 d  ####
# c. Elastic net

lambda = seq(-6,6,0.1)
### Build the model
set.seed(123)
elastic <- train(
  item_cnt_day ~., data = data_train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
### Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)

### Make predictions
predictions_Elastic <- elastic %>% predict(data_test)

### Model prediction performance
Score_Elastic=data.frame(
  RMSE = RMSE(predictions_Elastic, data_test$item_cnt_day),
  Rsquare = R2(predictions_Elastic, data_test$item_cnt_day),
  MAPE = MAPE(predictions_Elastic+1, data_test$item_cnt_day+1)
)


varsSelected <- length(which(coef(elastic$finalModel, elastic$bestTune$lambda)!=0))
varsNotSelected <- length(which(coef(elastic$finalModel, elastic$bestTune$lambda)==0))
cat('ElasticNet uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')


#Trazas elastic net
plot(elastic, main="Elastic Net", xvar="lambda")



# c. Elastic net

lambda = seq(-6,6,0.1)
### Build the model
set.seed(123)
elastic <- train(
  item_cnt_day ~., data = data_train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
### Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)

### Make predictions
predictions_Elastic <- elastic %>% predict(data_test)

### Model prediction performance
Score_Elastic=data.frame(
  RMSE = RMSE(predictions_Elastic, data_test$item_cnt_day),
  Rsquare = R2(predictions_Elastic, data_test$item_cnt_day),
  MAPE = MAPE(predictions_Elastic+1, data_test$item_cnt_day+1)
)


varsSelected <- length(which(coef(elastic$finalModel, elastic$bestTune$lambda)!=0))
varsNotSelected <- length(which(coef(elastic$finalModel, elastic$bestTune$lambda)==0))
cat('ElasticNet uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')


#Trazas elastic net
plot(elastic, main="Elastic Net", xvar="lambda")



## f Función de Elasticidad ####

lambda = seq(-6,6,0.1)
### Build the model

set.seed(123)
Elasticidad <- train(
  item_cnt_day ~ item_price_Y, data = data_train ,method = "lm",
  trControl = trainControl("cv", number = 10),
  tuneLength = 5
)
summary

### Model coefficients
#coef(Elasticidad$finalModel, Elasticidad$bestTune$lambda)

### Make predictions
predictions_Elasticidad <- Elasticidad %>% predict(data_test)

### Model prediction performance
Score_Elasticidad=data.frame(
  RMSE = RMSE(predictions_Elasticidad, data_test$item_cnt_day),
  Rsquare = R2(predictions_Elasticidad, data_test$item_cnt_day),
  MAPE = MAPE(predictions_Elasticidad+1, data_test$item_cnt_day+1)
)
