

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
data=fread('C:/Users/rolft/Documents/UAI/UAI_2020/AP_git/me_code/Prueba_1/data/sale_complete.csv')
#s_train_v2
#Ajusta los atributos
data$date=as.Date(data$date, format = "%d.%m.%Y")
data$item_id=as.factor(data$item_id)
data$shop_id=as.factor(data$shop_id)
data$item_category_id=as.factor(data$item_category_id)
#Elimina las columnas que no se usarán
data$shop_name<- NULL
data$item_name<-NULL
data$item_category_name<-NULL


#Visualiza el tipo de columnas
print(sapply(names(data), function(x) {class(data[[x]])}))

train<-data[V1<2882335,]#2882335 comienza en adelante la data de Test
train$V1<- NULL
train$date_block_num<- NULL


#library(dplyr)
#set.seed(1)
#dat <- as.character(X$item_id)

data %>% 
  group_by(item_id) %>%
  summarise(no_rows = length(item_id))


histogram(data$item_id,data,type="percent")

histogram(data$item_id,data,type="count")

I="20949"

data_min<-data[item_id !=I ,.(item_id)]

histogram(data_min$item_id,data_min,type="count")

# Listado de todos los items posibles
#L_item<- sort(as.numeric(unique(data$item_id))-1)


item_N=data %>% 
  group_by(item_id) %>%
  summarise(no_rows = length(item_id))

item_N <- item_N[order(-item_N$no_rows),]#$item_id


item_N=item_N[item_N$no_rows>500,]#"item_id"

for( i in item_N$item_id){
  print(i)
}
