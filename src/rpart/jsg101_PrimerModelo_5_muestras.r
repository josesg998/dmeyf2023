# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
#setwd("X:\\gdrive\\uba2023\\") # Establezco el Working Directory
setwd("~/Data Mining/DM_EyF_23/dmeyf2023")

# cargo el dataset
dataset <- fread("../datasets/competencia_01.csv")

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

seeds <- c(290497, 540187, 987851, 984497, 111893)

for (i in seq(6)){

if (i<6){
  
set.seed(seeds[i])
dtrain <- dtrain[sample(.N,nrow(dtrain)*.8)]}

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables

modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.518916925208492, # esto significa no limitar la complejidad de los splits
        minsplit = 1257, # minima cantidad de registros para que se haga el split
        minbucket = 308, # tamaño minimo de una hoja
        maxdepth = 7
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/BO2001")

# solo los campos para Kaggle

if(i<6){
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = paste0("./exp/BO2001/K101_001_sample",i,".csv"),
        sep = ",")}
else {
  fwrite(dapply[, list(numero_de_cliente, Predicted)],
         file = paste0("./exp/BO2001/K101_001_sample","todo_dataset",".csv"),
         sep = ",")
  
}
}
