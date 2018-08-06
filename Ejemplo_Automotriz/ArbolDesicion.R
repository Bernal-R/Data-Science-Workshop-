#=================== 1.Importamos librerias ===================
library(caret)
library(rpart.plot)
library(rpart)

#=================== 2.Importamos set de datos ===================
setwd("/Users/bernalrojas/Documents/ArbolDesicion")
car_df = read.csv("database.csv", header = TRUE, dec = ".", sep = ",")


#=================== 3.Separamos Set de datos para Entrenamiento y Prueba ===================
set.seed(3033) #Punto de inicio (No aleatorio)
intrain <- createDataPartition(y = car_df$acceptation, p= 0.7, list = FALSE) # 70% - 30%
training <- car_df[intrain,]
testing <- car_df[-intrain,]

#Revisar dimensiones para asegurarse que la partici??n fue exitosa
dim(training); dim(testing);

#=================== PreEntrenamiento ===================
#Nota: Este paso se realiza por buena costumbre, mas no es necesario del todo.
anyNA(car_df)     #Revisamos que no hayan valores perdidos que nos den porblemas luego
summary(car_df)   #Est??disticas descriptivas de los datos

#=================== 4.Entrenamiento ===================
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) #Estructura del arbol, estamos usando el m??todo para Validaci??n Cruzada
dtree_fit <- train(acceptation ~., data = training, method = "rpart", 
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10) #Entrenamiento 
dtree_fit #Mostramos el modelo entrenado

prp(dtree_fit$finalModel, box.palette = "Blues", tweak = 1.2) #Generar modelo gr??fico

#=================== 5.Prediccion ===================
#Predecimos para 1 auto
testing[1,] #Cargamos la fila 1
predict(dtree_fit, newdata = testing[1,]) #Predecimos 

#Probamos para todos los autos
test_pred <- predict(dtree_fit, newdata = testing) #Predecimos
confusionMatrix(test_pred, testing$acceptation )   #Revisar Accuracy 

