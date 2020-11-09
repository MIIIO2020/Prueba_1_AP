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


#Ajusta los atributos
data_test$V1 = NULL
data_test$date = NULL
data_test$shop_id=as.factor(data$shop_id)
data_test$item_category_id=as.factor(data$item_category_id)

#Visualiza el tipo de columnas
print(sapply(names(data), function(x) {class(data[[x]])}))

# mueve la columna de i tem_cnt_day
data_test <- data_test[,c(2,1,3:1248)]
colnames(data_test)



### Predictor variables
x <- model.matrix(item_cnt_day~., data_train)[,-1]

### Outcome variable
y <- data_train$item_cnt_day

### New predictor variables
newX<- model.matrix(item_cnt_day~., data_test)[,-1]

### New outcome variable
newY<- data_test$item_cnt_day


