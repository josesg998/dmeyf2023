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

f <- function(df, column_name, excluded_variables) {
  # Get the column to divide by all others
  column_to_divide <- df[[column_name]]
  # Get the names of all other columns
  other_column_names <- names(df)[names(df) != column_name]
  # Create a new column for each other column
  for (other_column_name in other_column_names) {
    if (is.numeric(df[[other_column_name]]) && !(other_column_name %in% excluded_variables)) {
      df[[paste0(column_name, "_divided_by_", other_column_name)]] <- column_to_divide / df[[other_column_name]]
    }
  }
  # Return the new data frame
  df
}

#dataset <- f(dataset,"ctrx_quarter","foto_mes")
#dataset <- f(dataset,"ctransferencias_emitidas",append(names(dataset)[grepl("_divided_by",names(dataset))],"foto_mes"))

#dataset[,paste("ctrx_quarter","ccaja_ahorro",sep="_divided_by_"):=ctrx_quarter/ccaja_ahorro]
#dataset[,paste("ctrx_quarter","tcuentas",sep="_divided_by_"):=ctrx_quarter/tcuentas]
#dataset[,paste("ctrx_quarter","ccuenta_corriente",sep="_divided_by_"):=ctrx_quarter/ccuenta_corriente]
#dataset[,paste("ctrx_quarter","cproductos",sep="_divided_by_"):=ctrx_quarter/cproductos]
#dataset[,paste("ctrx_quarter","cliente_edad",sep="_divided_by_"):=ctrx_quarter/cliente_edad]

#dataset[,paste("mpasivos_margen","tcuentas",sep="_divided_by_"):=mpasivos_margen/tcuentas]
#dataset[,paste("mpasivos_margen","cliente_edad",sep="_divided_by_"):=mpasivos_margen/cliente_edad]

#dataset[,paste("mautoservicio","tcuentas",sep="_divided_by_"):=mautoservicio/tcuentas]
#dataset[,paste("mautoservicio","ccuenta_corriente",sep="_divided_by_"):=mautoservicio/ccuenta_corriente]
#dataset[,paste("mautoservicio","cliente_edad",sep="_divided_by_"):=mautoservicio/cliente_edad]
#
#
#dataset[,paste("ctarjeta_debito_transacciones","cliente_edad",sep="_divided_by_"):=ctarjeta_debito_transacciones/cliente_edad]
#dataset[,paste("ctarjeta_debito_transacciones","numero_de_cliente",sep="_divided_by_"):=ctarjeta_debito_transacciones/numero_de_cliente]
#dataset[,paste("ctarjeta_debito_transacciones","tcuentas",sep="_divided_by_"):=ctarjeta_debito_transacciones/tcuentas]
#dataset[,paste("ctarjeta_debito_transacciones","ccuenta_corriente",sep="_divided_by_"):=ctarjeta_debito_transacciones/ccuenta_corriente]
#
#dataset[,paste("mtransferencias_emitidas","cliente_edad",sep="_divided_by_"):=mtransferencias_emitidas/cliente_edad]
#dataset[,paste("mtransferencias_emitidas","ccaja_ahorro",sep="_divided_by_"):=mtransferencias_emitidas/ccaja_ahorro]
#
#dataset[,paste("ctransferencias_emitidas","numero_de_cliente",sep="_divided_by_"):=ctransferencias_emitidas/numero_de_cliente]
#
#

library(readODS)

diccionario <- read_ods("../datasets/DiccionarioDatos_2023.ods")

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

columnas_a_sumar <- diccionario[diccionario$unidad=="pesos",]$campo[c(1:35,37:74)]

dtrain[, (columnas_a_sumar) := lapply(.SD, function(x) x + x * 0.089), .SDcols = columnas_a_sumar]

pesos <- copy( dtrain[, ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ] )

param <- list()

param$cp <-  -1 # esto significa no limitar la complejidad de los splits
param$minsplit <-  659 # minima cantidad de registros para que se haga el split
param$minbucket <-  200 # tamaño minimo de una hoja
param$maxdepth <-  12 # profundidad maxima del arbol
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
#prp(modelo,extra = 101, digits = -5,branch = 1, type = 4, varlen = 0, faclen = 0)

#comparo importancias con etiuquetas
importancias <- data.table(importance=as.vector(modelo$variable.importance),
                           campo=as.vector(names(modelo$variable.importance)))

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
       file = "exp/KA2001/test_prueba_IPC.csv",
       sep = ","
)
