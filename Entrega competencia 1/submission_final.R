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

dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]


dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

pesos <- copy( dtrain[, ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ] )

param <- list()

param$cp <-  -1 # esto significa no limitar la complejidad de los splits
param$minsplit <-  856 # minima cantidad de registros para que se haga el split
param$minbucket <-  380 # tamaño minimo de una hoja
param$maxdepth <-  11 # profundidad maxima del arbol
param$corte <- 9503


# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
  formula = "clase_binaria ~ . - clase_ternaria",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  control=param,
  weights=pesos
) 


# grafico el arbol
prp(modelo,extra = 101, digits = -5,branch = 1, type = 4, varlen = 0, faclen = 0)

#comparo importancias con etiuquetas
diccionario <- data.table(readODS::read_ods("../datasets/DiccionarioDatos_2023.ods"))

importancias <- data.table(importance=as.vector(modelo$variable.importance),
                           campo=as.vector(names(modelo$variable.importance)))

evaluacion_importancias <- merge(diccionario,importancias)

# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA
dapply[, prob_baja := prediccion[, "POS"]]

setorder( dapply, -prob_baja )

# grabo el submit a Kaggle
dapply[ , Predicted := 0L ]
dapply[ 1:param$corte, Predicted := 1L ]


# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
#dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("exp/")
dir.create("exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "exp/KA2001/submission_final.csv",
       sep = ","
)