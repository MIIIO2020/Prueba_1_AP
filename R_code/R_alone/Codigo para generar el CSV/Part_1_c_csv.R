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
#library(labelVector)



#Descarga la data sale_complete.csv
data=fread(paste('C:/Users/rolft/Documents/UAI/UAI_2020/AP_git/me_code/',
            'Prueba_1/data/data.csv',sep = ""))
  
#Ajusta los atributos
data$date=as.Date(data$date, format ="%Y-%m-%d") #"%d.%m.%Y")
data$item_id=as.factor(data$item_id)
data$shop_id=as.factor(data$shop_id)
data$item_category_id=as.factor(data$item_category_id)

#Elimina las columnas que no se usarán
data$date_block_num<- NULL
data$shop_name<- NULL
data$item_name<-NULL
data$item_category_name<-NULL
data$month<- NULL
data$month_name<- NULL
data$year<- NULL
data$day<- NULL
data$weekday<- NULL
data$weekday_name<- NULL
data$shop_id_categorico<- NULL


#Ordena los item de mayor frecuencia a menor frecuencia
data <- data[order(-data$date),]

V1=1:nrow(data)
V1=order(-V1)
data$V1=V1
rm(V1)

#Visualiza el tipo de columnas
print(sapply(names(data), function(x) {class(data[[x]])}))

## Separo en Test / Train
test<-data[format(as.Date(date),"%Y")=="2015"& 
             (as.integer(format(as.Date(data$date),"%m"))=="10"),]
nrow(test)
sep=test$V1[nrow(test)]
rm(sep)
#2882335 comienza en adelante la data de Test
train<-data[V1<sep,]


## Lista de Items ####
#Listado de todos los items posibles
    item_N=data %>% 
      group_by(item_id) %>%
      summarise(no_rows = length(item_id))
    item_N=item_N[item_N$no_rows>1000,]
    #Ordena los item de mayor frecuencia a menor frecuencia
    item_N <- item_N[order(-item_N$no_rows),]
    
    item_N<-item_N['item_id']
    
    ### cambiar####
    #item_N<-item_N[1:101,]
    
    
    # para revisar  requiere tipo chapter
    #rm(data)

## Fucnión que prepara la data ####
Make_data<-function(Data_in){

  Data_in$V1<- NULL
  Data_in$date_block_num<- NULL  
    
  ## Comienza generando la data ####
  
  I="20949"
  
  Todo_I<-Data_in[item_id==I& item_cnt_day >0& item_price > 0 ,
                .(date=date,
                  item_cnt_day=item_cnt_day,
                  shop_id=shop_id,
                  item_price_Y=item_price,
                  item_category_id=item_category_id
                )]
  
  # Función que recorre todos lo item 
  
  #Agrega una columna al data frame con el nombre del id de los items
  for (id in item_N$item_id ){ 

      if(id!=I){
  
  
  # Antes no tenia el filtrado de Date & shop_id
    x_data=train[item_id==id&
                   item_price>0 ,
                 .(date=date,
                   shop_id=shop_id,
                   item_price=item_price)]
    # Realiza el merge
    Todo_I<-merge(x=Todo_I,y=x_data,by=c('date','shop_id'),all.x=TRUE)
    Todo_I[,id]<-sapply(Todo_I$item_price, function(x) ifelse(is.na(x),0, log(x) ) )
    
    
    Todo_I$item_price=NULL
    Todo_I$item_price.y=NULL
    Todo_I$item_price.x=NULL
    }#if not id =I
    }#firts For
  
 
return(Todo_I)}

    
    

##Termina la funcion ####


Prueba<-Make_data(test)

Entrenamiento <- Make_data(train)

## write csv ####
I="20949"

write.csv(Prueba,paste("20949","Test.csv",sep = "_"))

write.csv(Entrenamiento,paste("20949","Train.csv",sep = "_"))


