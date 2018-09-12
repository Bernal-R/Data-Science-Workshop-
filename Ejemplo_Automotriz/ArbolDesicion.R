#=================== 1.Importamos librerias ===================
library(caret)
library(rpart.plot)
library(rpart)

#=================== 2.Importamos set de datos ===================
download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", destfile = "car.data")
car_df <- read.csv("car.data", sep = ',', header = FALSE)

#=================== 3.Separamos Set de datos para Entrenamiento y Prueba ===================
set.seed(3033) #Punto de inicio (No aleatorio)
intrain <- createDataPartition(y = car_df$V7, p= 0.7, list = FALSE) # 70% - 30%
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
set.seed(3333)
dtree_fit <- train(V7 ~., data = training, method = "rpart", 
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10) #Entrenamiento 
dtree_fit #Mostramos el modelo entrenado

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2) #Geramos el arbol gr??ficamente

#=================== 5.Prediccion ===================
#Predecimos para 1 auto
testing[1,] #Cargamos la fila 1
predict(dtree_fit, newdata = testing[1,]) #Predecimos 

#Probamos para todos los autos
test_pred <- predict(dtree_fit, newdata = testing) #Predecimos
confusionMatrix(test_pred, testing$V7 )   #Revisar Accuracy 


#=================== Alternativa a Prediccion Basada en Coeficientes de Gini ===================
#Nota: Esta es una alternativa m??s precisa para realizar la predicci??n del paso anterior, mas no es necesaria del todo.

#* Entrenamiento con Coficiente de Gini *
set.seed(3333)
dtree_fit_gini <- train(V7 ~., data = training, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10) #Entrenamiento basado en Coeficiente de Gini
dtree_fit_gini #Ver modelo entrenado

prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2) #Generar modelo gr??fico

# * Prediccion con Coficiente de Gini *
test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$V7 )  #Revisar Accuracy
