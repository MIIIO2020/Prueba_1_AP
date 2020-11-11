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
train$V1<- NULL
train$date_block_num<- NULL


## Lista de Items ####
#Listado de todos los items posibles
item_N=data %>% 
  group_by(item_id) %>%
  summarise(no_rows = length(item_id))


item_N <- item_N[order(-item_N$no_rows),]#$Ordena los item de mayor frecuencia a menor frecuencia
item_N=item_N[item_N$no_rows>500,]#"item_id"
item_N<-(item_N['item_id'])
# para revisar  requiere tipo chapter

## Elimina la data no a usar ####
rm(data)



## Comienza generando la data ####

I="20949"

Todo_I<-train[item_id==I ,
              .(date=date,
                item_cnt_day=item_cnt_day,
                shop_id=shop_id,
                item_price=item_price,
                item_category_id=item_category_id
              )]


D_explo<-train[train$date==Todo_I$date&
                 train$shop_id==Todo_I$shop_id ,.(item_id=item_id,date=date,
        shop_id=shop_id,
        item_price=item_price)]

## Función que recorre todos lo item ####
#Agrega una columna al data frame con el nombre del id de los items

for (id in item_N$item_id ){  #as.character(L_item)) {
    id="5822"
    if(id!=I){
#genera un vector el cual contendrá los precios del item id
  x <-replicate(nrow(Todo_I),0)  # 1:nrow(Price_item)
  x <- set_label(x, id)
  
# Antes no tenia el filtrado de Date & shop_id
  x_data=train[item_id==id               ,
               .(date=date,
                 shop_id=shop_id,
                 item_price=item_price)]
  
  
  if(length(x_data)!=0){
      for (r in 1:nrow(Todo_I)){
        y <-x_data[x_data$date==Todo_I[r,]$date 
                   & shop_id == Todo_I[r,]$shop_id]$item_price
        #print(1)
        if(length(y)!=0){
          if( y >0){x[r]<-log(y)}else{x[r]=0}
            }#if length
          else{x[r]=0}
      }#For rows
    }#if_x_dat
  Todo_I[,id]<-x
  }#if not id =I
  }#firts For

write.csv(Todo_I,paste(as.character(I),".csv",sep = ""))
#write.csv(x,paste(as.character(I),".csv",sep = ""))


data_explor<-merge(x=Todo_I,y=x_data,by=c('date','shop_id'),all.x=TRUE)
data_explor[,id]<-lapply(data_explor$item_price.y, function(x) ifelse(is.na(x),0, log(x) ) )




