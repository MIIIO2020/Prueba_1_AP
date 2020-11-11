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
install.packages("labelVector")


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

#Descarga la data sale_complete.csv
data_test=fread('C:/Users/rolft/Documents/UAI/UAI_2020/AP_git/me_code/Prueba_1/data/20949__Test.csv')
data_train=fread('C:/Users/rolft/Documents/UAI/UAI_2020/AP_git/me_code/Prueba_1/data/20949_Train_2.csv')

#Visualiza el tipo de columnas
print(sapply(names(data_X), function(x) {class(data_X[[x]])}))
colnames(data_X)


## Funcion de Data####

F_date<-function(data_X){
#Ajusta los atributos
  data_X$V1 = NULL
  data_X$date = NULL
  data_X$shop_id=as.factor(data_X$shop_id)
   
  # mueve la columna de i tem_cnt_day
  # Solo incluye los 50 items con mas datos en el modelo
  data_X <- data_X[,c(2,1,3:54)]

}

data_test<-F_date(data_test)
data_train<-F_date(data_train)
rm(F_date)

## Continuamos con las particiones ####

### (No se deberia usar)Split the data into training and test set####
# Esto no se deberia usar #
# set.seed(123)
# training.samples <- model_data$SalePrice %>%
#   createDataPartition(p = 0.8, list = FALSE)
# 
# train.data  <- model_data[training.samples, ]
# test.data <- model_data[-training.samples, ]
# 
# pred_data = model_data_aux[is.na(model_data_aux$SalePrice),] 
# pred_data$SalePrice = NULL
##################################



### Predictor variables
x <- model.matrix(item_cnt_day~., data_train)[,-1]

### Outcome variable
y <- data_train$item_cnt_day

### New predictor variables
newX<- model.matrix(item_cnt_day~., data_test)[,-1]

### New outcome variable
newY <- data_test$item_cnt_day


## Reci?n ahora empecemos a resolver la prueba...####

#revisar si tiene datos NA
# No tiene ningun NA
#data_train$item_category_id=as.integer(data_train$item_category_id)
# tengo que quitarle el formato de factor a item_category_id

# Preg 2. Regresion Lineal Multiple



## a. Selecci?n Stepwise forward ####

null<-lm((item_cnt_day)~1, data=data_train)
full<-lm(item_cnt_day~., data=data_train)
#error presente si existen NA o un factor con solo una categoria
#esto sucede con item_category_id
# Para ello cambie el formato a entero, esto solucionó el problema


output2a<-step(null, scope = list(upper=full), data=data_train, direction="both")
summary(output2a)

rm(full,null)


#se contruye un modelo utilizando lo generado por la función step

# model2a =  output2a$call$formula

#como se detubo la operación tras 12 horas, se utilizará el ultimo modelo printeado por la función
#tep:  AIC=-26846.07

### Build the model of  Stepwise forward ####

set.seed(123)
Modelo_step_F= train(  item_cnt_day ~ shop_id + `1855` + `16787` + `4870` + `6498` + 
  `13811` + `3734` + `8452` + `4249` + `16056` + `15044` + 
  `3183` + `5672` + `7894` + `1555` + `2445` + `16169` + `1512` + 
  `13071` + `6504` + `5272` + `7098` + `9355` + `7893` + `1830` + 
  `5671` + `1905` + `16455` + `15063` + `4351` + `12908` + 
  `3732` + `6497` + `21487` + `5823` + `5811` + `3341` + `3329` + 
  `19415` + `10659` + `16540` + `2808` + `1809` + `14931` + 
  `4886` + `7807` + `1516` + `16900` + `20608` + `7856` + `3733` + 
  `2269` + `10292` + `6111` + `5669` + `4178` + `21427` + `6503` + 
  `6466` + `14337` + `20609` + `14261` + `968` + `6121` + `5033` + 
  `7018` + `19900` + `6954` + `5637` + `5660` + `972` + `485` + 
  `17717` + `3331` + `6499` + `4810` + `5822` + `1850` + `20947` + 
  `16287` + `4894` + `4242` + `4885` + `10498` + `6086` + `6738` + 
  `1564` + `3342` + `16011` + `10957`, data=data_train ,method = "lm",
  trControl = trainControl("cv", number = 2),
  tuneLength = 5
  )


### Model coefficients
coef(Modelo_step_F$finalModel)


### Make predictions
data_test = data_test[shop_id!="36",]
predictions_Forward <- Modelo_step_F %>% predict(data_test)

       # "ocurre un erro el si se ejecuta, debido a que hay valores NA, 
       #  
       #  Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
       #  factor shop_id has new levels 36 "
       # Para aolusionar esto vere que categorias estan presentes en cada una de las dos datas
      

### Model prediction performance
data.frame(
  RMSE = RMSE(predictions, data_test$item_cnt_day),
  Rsquare = R2(predictions, data_test$item_cnt_day)
)
     #Agregar el MAPE aquí en el data.frame
#print
# RMSE   Rsquare
# 1 0.6291678 0.5268298


varsSelected <- length(coef(Modelo_step_F$finalModel))
cat('Stepwise forward',' uses', varsSelected, 'variables in its model')
#Ridge uses 141 variables in its model





### Build the full model or classic ####
set.seed(123)
lm_model <- train(
  item_cnt_day ~., data = data_train, method = "lm",
  trControl = trainControl("cv", number = 2),
  tuneLength = 5
)
### Model coefficients
coef(lm_model$finalModel)


### Make predictions
predictions_lm_model <- lm_model %>% predict(data_test)

### Model prediction performance
data.frame(
  RMSE = RMSE(predictions_lm_model, data_test$item_cnt_day),
  Rsquare = R2(predictions_lm_model, data_test$item_cnt_day)
)

varsSelected <- length(coef(lm_model$finalModel))
cat('Ridge uses', varsSelected, 'variables in its model')