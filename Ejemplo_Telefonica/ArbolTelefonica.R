# Predecir Perdida de Clientes con Arbol de Decision

# PASO 1:   Carga Package y Set de datos
# ---------------------------------------------------------------------------
library(C50)
library(rpart)
library(rpart.plot) 
library(caret)

setwd("/Users/bernalrojas/Documents")
clientes = read.csv("DataBase_clients.csv", header = TRUE, dec = ".", sep = ",")

intrain <- createDataPartition(y = clientes$churn, p= 0.63, list = FALSE) # 63% - 37%
Entrenamiento <- clientes[intrain,]
Test <- clientes[-intrain,]

# PASO 2:   Crea Arbol de Decision
# ---------------------------------------------------------------------------
ModeloArbol<-rpart(clientes ~ .,data=Entrenamiento,parms=list(split="information"))


# PASO 3:  Predice Desafiliación en datos de TEST
# ---------------------------------------------------------------------------
Prediccion <- predict(ModeloArbol, Test,type="class") # Prediccción en Test
MC         <- table(Test[, "churn"],Prediccion) # Matriz de Confusión


# PASO 4: Crea Grafico
# ---------------------------------------------------------------------------
rpart.plot(ModeloArbol, type=1, extra=100,cex = .7,
           box.col=c("#D7DADB", "#6DBCDB")[ModeloArbol$frame$yval])
