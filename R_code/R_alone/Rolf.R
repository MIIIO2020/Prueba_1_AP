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
    #data_X$item_category_id=as.factor(data_X$item_category_id)
    #data_X$item_category_id=integer(data_X$item_category_id)
  
  # tengo que quitarle el formato de factor a item_category_id
  
  # mueve la columna de i tem_cnt_day
  data_X <- data_X[,c(2,1,3:1282)]
  
  #return(data_X)
}

data_test<-F_date(data_test)
data_train<-F_date(data_train)
rm(F_date)

## Continuamos con las particiones ####

### (No se deberia usar)Split the data into training and test set####
# Esto no se deberia usar #
set.seed(123)
training.samples <- model_data$SalePrice %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- model_data[training.samples, ]
test.data <- model_data[-training.samples, ]

pred_data = model_data_aux[is.na(model_data_aux$SalePrice),] 
pred_data$SalePrice = NULL
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
#checkDataQuality(data_train, 'DataQualityReport/num.csv', 'DataQualityReport/cat.csv')
# No tiene ningun NA
#data_train$item_category_id=as.integer(data_train$item_category_id)
# tengo que quitarle el formato de factor a item_category_id

# Preg 2. Regresion Lineal Multiple
# a. Selecci?n Stepwise forward

null<-lm((item_cnt_day)~1, data=data_train)
full<-lm(item_cnt_day~., data=data_train)
#error presente si existen NA o un factor con solo una categoria
#esto sucede con item_category_id


output2a<-step(null, scope = list(upper=full), data=data_train, direction="both")
summary(output2a)
model2a =  output2a$call$formula

### Build the model
set.seed(123)
lm_model <- train(
  item_cnt_day ~., data = data_train, method = "lm",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
### Model coefficients
coef(lm_model$finalModel)


### Make predictions
predictions <- lm_model %>% predict(test.data)

### Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$SalePrice),
  Rsquare = R2(predictions, test.data$SalePrice)
)

varsSelected <- length(coef(lm_model$finalModel))
cat('Ridge uses', varsSelected, 'variables in its model')

