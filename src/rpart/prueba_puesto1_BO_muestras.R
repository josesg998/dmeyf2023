# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
#setwd("X:\\gdrive\\uba2023\\") # Establezco el Working Directory
#setwd("~/Data Mining/DM_EyF_23/dmeyf2023")
#
# cargo el dataset
dataset <- fread("datasets/competencia_01.csv")

dataset[,clase_ternaria := toupper(clase_ternaria)]

dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

seeds <- c(290497, 540187, 987851, 984497, 111893)

CORTE <- 9503


get_percentage_sample_of_data_stratified_by_one_variable <- function(data, variable, percentage) {
  # Esta funcion toma una muestra estratificada de una variable
  # data: dataset
  # variable: variable por la cual se estratifica
  # percentage: porcentaje de la muestra
  # return: data.table con la muestra
  data[, .SD[sample(.N, round(.N * percentage), replace = FALSE)], by = variable]
}



for (i in 1:length(seeds)){
  
  set.seed(seeds[i])
  
  dtrain <- get_percentage_sample_of_data_stratified_by_one_variable(dtrain,"clase_ternaria",.5)
  pesos <- copy( dtrain[, ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ] )
  
  
  
  # genero el modelo,  aqui se construye el arbol
  # quiero predecir clase_ternaria a partir de el resto de las variables
  
  modelo <- rpart(
    formula = "clase_binaria ~ . - clase_ternaria",
    data = dtrain, # los datos donde voy a entrenar
    xval = 0,
    cp = -1, # esto significa no limitar la complejidad de los splits
    minsplit = 659, # minima cantidad de registros para que se haga el split
    minbucket = 200, # tamaño minimo de una hoja
    maxdepth = 12,
    weights = pesos
  ) # profundidad maxima del arbol
  
  
  # grafico el arbol
  #prp(modelo,
  #    extra = 101, digits = -5,
  #    branch = 1, type = 4, varlen = 0, faclen = 0
  #)
  
  
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
  # agrego a dapply una columna nueva que es la probabilidad de BAJA
  dapply[, prob_baja := prediccion[, "POS"]]
  
  setorder( dapply, -prob_baja )
  
  # grabo el submit a Kaggle
  dapply[ , Predicted := 0L ]
  dapply[ 1:CORTE, Predicted := 1L ]
  
  
  
  # solo le envio estimulo a los registros
  #  con probabilidad de BAJA+2 mayor  a  1/40
  #dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
  
  # genero el archivo para Kaggle
  # primero creo la carpeta donde va el experimento
  dir.create("exp/")
  dir.create("exp/BO2001_muestras")
  
  # solo los campos para Kaggle
  
  fwrite(dapply[, list(numero_de_cliente, Predicted)],
         file = paste0("./exp/BO2001_muestras/K101_BO001_oversampling_sample_",i,".csv"),
         sep = ",")}
