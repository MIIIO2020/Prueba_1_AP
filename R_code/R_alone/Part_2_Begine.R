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



# Listado de todos los items posibles
L_item<- sort(as.integer(unique(data$item_id)))

# para revisar estilo chapter
#c_item<-as.character(L_item)
rm(data)

#Lo nuevo

I="20949"

Todo_I<-train[item_id==I ,
              .(date=date,item_cnt_day=item_cnt_day,
                shop_id=shop_id,
                item_price=item_price,
                item_category_id=item_category_id
              )]


Price_item=Todo_I[,.(
  date=date,
  shop_id=shop_id
) ]


price_id<-train[date==Todo_I$date & shop_id== Todo_I$shop_id]


price_id<-train[date==Todo_I$date & shop_id== Todo_I$shop_id]


#funciona
#Agrega una columna al data frame con el nombre del id de los items
for (id in as.character(L_item)) {
  x <-replicate(nrow(Price_item),1)  # 1:nrow(Price_item)rc()
  x <- set_label(x, id)
  Price_item[,id]<-x
}

print()





