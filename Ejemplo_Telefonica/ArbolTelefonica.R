library(C50)
library(rpart)
library(rpart.plot) 
data(churn); # carga tablas

Variables      <-c(4,7,16,19,17,20)               # variables elegidas del set de datos
Entrenamiento  <-churnTrain[,Variables]           # tabla entrenamiento
Test           <-churnTest [,Variables]           # tabla  Test

# PASO 2:   Crea Arbol de Decision
# ---------------------------------------------------------------------------
ModeloArbol<-rpart(churn ~ .,data=Entrenamiento,parms=list(split="information"))

# PASO 3:  Predice Desafiliacion en datos de TEST
# ---------------------------------------------------------------------------
Prediccion <- predict(ModeloArbol, Test[-6],type="class") # Predicccion en Test
Prediccion

# PASO 4: Crea Grafico
# ---------------------------------------------------------------------------
rpart.plot(ModeloArbol, type=1, extra=100,cex = .7,
           box.col=c("#D7DADB", "#6DBCDB")[ModeloArbol$frame$yval])
