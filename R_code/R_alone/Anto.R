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



#Load the data
data=read.csv("/Users/antoniaindaorlandi/Desktop/Prueba Análisis Predictivo/20949__Test.csv",header=TRUE, sep=",")

data<-data[data$item_cnt_day > 0, ]




set.seed(0)
training.samples <- data$item_cnt_day %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

### Predictor variables

x <- model.matrix(item_cnt_day~., train.data)[,-1]


### Outcome variable

y <- train.data$item_cnt_day


### New predictor variables

newX<- model.matrix(item_cnt_day~., test.data)[,-1]


### New outcome variable

newY<- test.data$item_cnt_day


#Generación del modelo

null<-lm(log(item_cnt_day)~1, data=train.data)
full<-lm(log(item_cnt_day)~., data=train.data)
outputIIa<-step(null, scope = list(upper=full), data=train.data, direction="both")
summary(outputIIa)
modelIIa =  outputIIa$call$formula

set.seed(0)
lm_model <- train(
  item_cnt_day ~., data = train.data, method = "lm",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
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

lambda <- 10^seq(-3, 3, length = 100)

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

#Trazas ridge
plot(ridge, main="Ridge")
plot(ridge, xvar="lambda")


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

#Trazas lasso
plot(lasso, main="LASSO")
plot(lasso, xvar="lambda")


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


#Trazas elastic net
plot(elastic, main="Elastic Net")
plot(elastic, xvar="lambda")


