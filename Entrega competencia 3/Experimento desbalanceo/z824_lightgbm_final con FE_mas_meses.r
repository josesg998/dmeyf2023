# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "final_desbalanceo_mas_meses"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(201901, 201902, 201903, 201904, 201905, 201906, 201907, 
                          201908, 201909, 201910, 201911, 201912,202010, 202011,
                          202012, 202101, 202102, 202103,202104,202105,202106,202107)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 100019

# hiperparametros intencionalmente NO optimos
PARAM$finalmodel$optim$num_iterations <- 2002
PARAM$finalmodel$optim$learning_rate <- 0.0200082081155322
PARAM$finalmodel$optim$feature_fraction <- 0.0476821086256962
PARAM$finalmodel$optim$min_data_in_leaf <- 4570
PARAM$finalmodel$optim$num_leaves <- 337
PARAM$finalmodel$optim$neg_bagging_fraction <- 0.274824050643246


# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = TRUE, # Magic Sauce

  seed = PARAM$finalmodel$semilla
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]

# Data Drifting
# por ahora, no hago nada


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso ! corta la bocha
#   https://rdrr.io/cran/data.table/man/shift.html
# FI: Coloco NA a todos los registos en 0
zero_ratio <- list(
  list(mes = 202006, campo = 
         c("active_quarter", "internet", "mrentabilidad", "mrentabilidad_annual", 
           "mcomisiones", "mactivos_margen", "mpasivos_margen", "mcuentas_saldo", 
           "ctarjeta_debito_transacciones","mautoservicio", "ctarjeta_visa_transacciones", 
           "mtarjeta_visa_consumo","ctarjeta_master_transacciones", "mtarjeta_master_consumo",
           "ccomisiones_otras", "mcomisiones_otras","cextraccion_autoservicio","mextraccion_autoservicio",
           "ccheques_depositados","mcheques_depositados","ccheques_emitidos","mcheques_emitidos",
           "ccheques_depositados_rechazados","mcheques_depositados_rechazados","ccheques_emitidos_rechazados",
           "mcheques_emitidos_rechazados","tcallcenter","ccallcenter_transacciones","thomebanking",
           "chomebanking_transacciones","ccajas_transacciones","ccajas_consultas","ccajas_depositos",
           "ccajas_extracciones","ccajas_otras","catm_trx","matm","catm_trx_other","matm_other",
           "tmobile_app","cmobile_app_trx")),
  list(mes = 201910, campo = 
         c("mrentabilidad", "mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen",
           "ccomisiones_otras","mcomisiones_otras","chomebanking_transacciones")),
  list(mes = 201905, campo = 
         c("mrentabilidad", "mrentabilidad_annual", "mcomisiones","mactivos_margen","mpasivos_margen",
           "ccomisiones_otras","mcomisiones_otras")),
  list(mes = 201904, campo = 
         c("ctarjeta_visa_debitos_automaticos","mttarjeta_visa_debitos_automaticos"))
)

for (par in zero_ratio) {
  mes <- par$mes
  feature <- par$campo
  dataset[foto_mes == mes, (feature) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols = feature]
}

#______________________________________________________________
# FI: hago lag de los ultimos 6 meses de todas las features (menos numero cliente, foto mes y clase ternaria)

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

setorder(dataset, numero_de_cliente, foto_mes)

periods <- c(1,3,6) # Seleccionar cantidad de periodos 

for (i in periods){
  lagcolumns <- paste("lag", all_columns,i, sep=".")
  dataset[, (lagcolumns):= shift(.SD, type = "lag", fill = NA, n=i), .SDcols = all_columns,  by =numero_de_cliente]
}

# Delta LAG de 1 y 2 periodos

for (vcol in all_columns){
  dataset[, paste("delta", vcol,1, sep=".") := get(vcol) - get(paste("lag", vcol,1, sep="."))]
}

for (vcol in all_columns){
  dataset[, paste("delta", vcol,3, sep=".") := get(vcol) - get(paste("lag", vcol,3, sep="."))]
}

for (vcol in all_columns){
  dataset[, paste("delta", vcol,6, sep=".") := get(vcol) - get(paste("lag", vcol,6, sep="."))]
}

#________________________________________________
# FI: Ranking de cada cliente de cada mes en todas las features con 0 fijo - V2

# Assuming neg_bagging_fraction is part of col_moneda
col_moneda <- colnames(dataset)
col_moneda <- col_moneda[col_moneda %like% "^(m|Visa_m|Master_m|vm_m)"]

for (campo in col_moneda) {
  cat(campo, " ")
  
  rankcolumns <- paste("rank", campo, sep=".")
  
  # Assuming neg_bagging_fraction is a parameter that can vary
  neg_bagging_fraction <- PARAM$bo_lgb$values[["neg_bagging_fraction"]]
  
  # Rank the variable using frank
  dataset[get(campo) == 0, (rankcolumns) := 0]
  dataset[get(campo) > 0, (rankcolumns) := frank(get(campo), ties.method = "dense") / neg_bagging_fraction, by = foto_mes]
  dataset[get(campo) < 0, (rankcolumns) := -frank(-get(campo), ties.method = "dense") / neg_bagging_fraction, by = foto_mes]
  dataset[, (campo) := NULL]
}

# Fin FE
#--------------------------------------


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)


# genero el modelo
param_completo <- c(PARAM$finalmodel$lgb_basicos,
  PARAM$finalmodel$optim)

modelo <- lgb.train(
  data = dtrain,
  param = param_completo,
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)

#--------------------------------------


# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
  file = "prediccion.txt",
  sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]

  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
