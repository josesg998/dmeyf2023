
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")
require("lightgbm")
require("zoo")



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "final_casalli_semillero"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

PARAM$input$training <- c(201901, 201902, 201903, 201904, 201905, 201906, 
                          201907, 201908, 201909, 201910, 201911, 201912,
                          202010, 202011, 202012,
                          202101, 202102, 202103, 202104, 202105,202106,202107)
juntar_cols = TRUE



PARAM$input$future <- c(202109) # meses donde se aplica el modelo
PARAM$finalmodel$semilla <- c(290497, 540187, 987851, 984497, 111893, 
                              100103, 100189, 101987, 991981, 991987)#,
                              #106853, 191071, 337511, 400067, 991751,
                              #729191, 729199, 729203, 729217, 729257)

#   237_0 
#PARAM$finalmodel$optim$num_iterations <- 829
#PARAM$finalmodel$optim$learning_rate <- 0.0314795407476228
#PARAM$finalmodel$optim$feature_fraction <- 0.575871678909783
#PARAM$finalmodel$optim$min_data_in_leaf <- 5336
#PARAM$finalmodel$optim$num_leaves <- 478

#   232_0
PARAM$finalmodel$optim$num_iterations <- 543
PARAM$finalmodel$optim$learning_rate <- 0.0463797426198357
PARAM$finalmodel$optim$feature_fraction <- 0.639247798653655
PARAM$finalmodel$optim$min_data_in_leaf <- 17446
PARAM$finalmodel$optim$num_leaves <- 1858


# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
# ordernar por mes:
setorderv(dataset, cols=c("foto_mes"), order=c(1L))



reemplazar_con_na = TRUE

cols_reemplazar_ceros = c("active_quarter", "internet", "mcuentas_saldo",
                          "ctarjeta_debito_transacciones", "mautoservicio",
                          "ctarjeta_visa_transacciones", "mtarjeta_visa_consumo",
                          "ctarjeta_master_transacciones", "mtarjeta_master_consumo",
                          "cextraccion_autoservicio", "mextraccion_autoservicio",
                          "ccheques_depositados", "mcheques_depositados", "ccheques_emitidos",
                          "mcheques_emitidos", "ccheques_depositados_rechazados",
                          "mcheques_depositados_rechazados", "ccheques_emitidos_rechazados",
                          "mcheques_emitidos_rechazados", "tcallcenter",
                          "ccallcenter_transacciones", "thomebanking", "ccajas_transacciones",
                          "ccajas_consultas", "ccajas_depositos", "ccajas_extracciones",
                          "ccajas_otras", "catm_trx", "matm", "catm_trx_other", "matm_other",
                          "tmobile_app", "cmobile_app_trx")
for (col in cols_reemplazar_ceros){
  if (reemplazar_con_na){
    dataset[foto_mes==202006, (col) := NA]
  }
  else{
    dataset[, (col) := fifelse( (foto_mes==202006) & (get(col) == 0),  
                                0.5 * (shift(get(col), n=1L, fill=NA, type="lag") + shift(get(col), n=1L, fill=NA, type="lead")),
                                get(col)), 
            by = numero_de_cliente]
  }
}


cols_reemplazar_ceros = c("mrentabilidad", "mrentabilidad_annual", "mcomisiones",
                          "mactivos_margen", "mpasivos_margen", "ccomisiones_otras",
                          "mcomisiones_otras" )
meses = c(201905, 201910, 202006)
for (col in cols_reemplazar_ceros){
  if (reemplazar_con_na){
    dataset[foto_mes %in% meses, (col) := NA]
  }
  else{
    dataset[, (col) := fifelse( (foto_mes %in% meses) & (get(col) == 0),  
                                0.5 * (shift(get(col), n=1L, fill=NA, type="lag") + 
                                         shift(get(col), n=1L, fill=NA, type="lead")),
                                get(col)), 
            by = numero_de_cliente]
  }
}


cols_reemplazar_ceros = c("ctarjeta_visa_debitos_automaticos", "mttarjeta_visa_debitos_automaticos")
for (col in cols_reemplazar_ceros){
  if (reemplazar_con_na){
    dataset[foto_mes==201904, (col) := NA]
  }
  else{
    dataset[, (col) := fifelse( (foto_mes==201904) & (get(col) == 0),  
                                0.5 * (shift(get(col), n=1L, fill=NA, type="lag") + 
                                         shift(get(col), n=1L, fill=NA, type="lead")),
                                get(col)), 
            by = numero_de_cliente]
  }
}


cols_reemplazar_ceros = c("chomebanking_transacciones")
meses = c(201910, 202006)
for (col in cols_reemplazar_ceros){
  if (reemplazar_con_na){
    dataset[foto_mes %in% meses, (col) := NA]
  }
  else{
    dataset[, (col) := fifelse( (foto_mes %in% meses) & (get(col) == 0),  
                                0.5 * (shift(get(col), n=1L, fill=NA, type="lag") + 
                                         shift(get(col), n=1L, fill=NA, type="lead")),
                                get(col)), 
            by = numero_de_cliente]
  }
}

rm(meses, cols_reemplazar_ceros)



#Llenar con ceros:
  
cols_nans_con_ceros = c("mtarjeta_visa_descuentos", "mtarjeta_master_descuentos",
                        "tmobile_app", "cmobile_app_trx")
setnafill(dataset, type="const", fill=0, cols=cols_nans_con_ceros)

rm(cols_nans_con_ceros)



#Arreglar variables con valores no esperados:
  
dataset[, internet := fifelse(internet > 1, 1, internet) ]



#Diferencia de estampas por cliente:
  
# columna con diferencia de estampas por cliente
dataset[, foto_mes_rang:= match(foto_mes, sort(unique(foto_mes))) ]
dataset[, foto_mes_rangdif := {
  c(NA, diff(foto_mes_rang, lag=1, differences=1))
}, .(numero_de_cliente)]
dataset[, foto_mes_rang := NULL]



#Juntar variables:
  
if (juntar_cols)
{
  dataset[, t_delinquency := fcase( Master_delinquency > 0, 1,
                                    Visa_delinquency > 0, 1,
                                    Visa_delinquency == 0, 0,
                                    Master_delinquency == 0, 0,
                                    default = NA ) ]
  dataset[, c("Master_delinquency", "Visa_delinquency") := NULL]
  
  
  sumar_cols = c("_mfinanciacion_limite", "_msaldototal", "_msaldopesos", "_msaldodolares",
                 "_mconsumospesos", "_mconsumosdolares", "_mlimitecompra", "_madelantopesos",
                 "_madelantodolares", "_mpagado", "_mpagospesos", "_mpagosdolares",
                 "_mconsumototal", "_cconsumos", "_cadelantosefectivo", "_mpagominimo" )
  for (col in sumar_cols)
  {
    col1 = paste0("t", col)
    col2 = paste0("Visa", col)
    col3 = paste0("Master", col)
    
    dataset[, (col1) := fcase( !is.na(get(col2)) & !is.na(get(col3)), get(col2) + get(col3),
                               !is.na(get(col2)), get(col2),
                               !is.na(get(col3)), get(col3),
                               default = NA ) ]
    dataset[, (col2) := NULL]
    dataset[, (col3) := NULL]
  }
  
  sumar_cols = c("ctarjeta_visa", "ctarjeta_visa_transacciones", "mtarjeta_visa_consumo",
                 "ctarjeta_visa_debitos_automaticos", "mttarjeta_visa_debitos_automaticos",
                 "ctarjeta_visa_descuentos", "mtarjeta_visa_descuentos")
  for (col in sumar_cols)
  {
    col1 = gsub("visa", "t", col)
    col2 = col
    col3 = gsub("visa", "master", col)
    
    dataset[, (col1) := fcase( !is.na(get(col2)) & !is.na(get(col3)), get(col2) + get(col3),
                               !is.na(get(col2)), get(col2),
                               !is.na(get(col3)), get(col3),
                               default = NA ) ]
    dataset[, (col2) := NULL]
    dataset[, (col3) := NULL]
  }
  
  rm(sumar_cols, col1, col2, col3)
}



#Normalización por inflación:
  
foto_mes = c(201901, 201902, 201903, 201904, 201905, 201906, 201907, 201908, 201909, 201910, 201911, 201912, 202001, 202002, 202003, 202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104, 202105, 202106, 202107, 202108, 202109, 202110, 202111, 202112)

ipc = c(189.6101, 196.7501, 205.9571, 213.0517, 219.5691, 225.537, 230.494, 239.6077, 253.7102, 262.0661, 273.2158, 283.4442, 289.8299, 295.666, 305.5515, 310.1243, 314.9087, 321.9738, 328.2014, 337.0632, 346.6207, 359.657, 371.0211, 385.8826, 401.5071, 415.8595, 435.8657, 453.6503, 468.725, 483.6049, 498.0987, 510.3942, 528.4968, 547.0802, 560.9184, 582.4575)
ipc = ipc/100

mapeo_ipc = data.table(foto_mes=foto_mes, ipc=ipc)
#mapeo_ipc[, foto_mes := as.character(foto_mes)]
rm(foto_mes, ipc)

cols_pesos <- c("mcuentas_saldo", "mprestamos_personales", "mcuenta_corriente_adicional",
                "mcuenta_corriente", "mactivos_margen", "mcaja_ahorro_adicional",
                "mcaja_ahorro", "mcomisiones_mantenimiento", "mplazo_fijo_pesos",
                "mpasivos_margen", "mcomisiones", "mcajeros_propios_descuentos",
                "mtarjeta_visa_consumo", "mtarjeta_t_consumo", 
                "mrentabilidad", "mpayroll", "mpayroll2",
                "mrentabilidad_annual", "Visa_msaldopesos",
                "Visa_msaldototal", "Visa_mpagominimo",
                "mcomisiones_otras", "mextraccion_autoservicio",
                "matm", "mtarjeta_master_consumo",
                "Visa_mpagospesos", "mcaja_ahorro_dolares",
                "mtransferencias_recibidas", "Master_msaldopesos",
                "Master_msaldototal", "mtransferencias_emitidas",
                "Master_mpagominimo", "Visa_mfinanciacion_limite",
                "Visa_mlimitecompra", "Master_mpagosdolares",
                "mcheques_emitidos_rechazados", "minversion1_pesos",
                "minversion2", "mcheques_emitidos", "minversion1_dolares",
                "mprestamos_prendarios", "Master_mconsumosdolares",
                "Master_mpagospesos", "Master_msaldodolares",
                "Visa_mpagosdolares", "mautoservicio",
                "mprestamos_hipotecarios", "mplazo_fijo_dolares",
                "mcuenta_debitos_automaticos", "mttarjeta_visa_debitos_automaticos",
                "mttarjeta_master_debitos_automaticos",
                "mpagodeservicios", "mpagomiscuentas",
                "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos",
                "mforex_buy", "mforex_sell", "mcheques_depositados",
                "mcheques_depositados_rechazados", "matm_other",
                "Master_mfinanciacion_limite", "Master_mconsumospesos",
                "Master_mlimitecompra", "Master_madelantopesos",
                "Master_madelantodolares", "Master_mpagado",
                "Master_mconsumototal", "Visa_msaldodolares",
                "Visa_mconsumospesos", "Visa_mconsumosdolares",
                "Visa_madelantopesos", "Visa_madelantodolares",
                "Visa_mpagado", "Visa_mconsumototal")

cols_pesos2 = c()
for(col in cols_pesos)
{
  if (col %in% colnames(dataset)){
    cols_pesos2 = c(cols_pesos2, col)
  }
  col1 = gsub("visa", "t", col)
  if ((col1 != col) & (col1 %in% colnames(dataset)))
  {
    cols_pesos2 = c(cols_pesos2, col1)
  }
  col1 = gsub("Visa", "t", col)
  if ((col1 != col) & (col1 %in% colnames(dataset)))
  {
    cols_pesos2 = c(cols_pesos2, col1)
  }
}
cols_pesos2 = unique(cols_pesos2)
rm(cols_pesos)

dataset[mapeo_ipc, (cols_pesos2) := (.SD) / ipc , 
        on="foto_mes", .SDcols = (cols_pesos2)]

rm(mapeo_ipc, cols_pesos2, col1)



#Media en ventana, lag2, lag3, dif1, dif2:
  
campos_media <- setdiff(
  colnames(dataset),
  c("foto_mes_rangdif",
    "clase_ternaria", "foto_mes", "numero_de_cliente", 
    "cliente_edad", "cliente_antiguedad", "ccuenta_corriente")
)

#----- media
n <- 5L # 3L
nuevas_cols = c()
for(col in campos_media)
{
  nuevas_cols = c(nuevas_cols, paste0(col, "_media_", n))
}

dataset[, (nuevas_cols) := frollmean(.SD, n=(n), fill=NA, align="right", algo="fast"), 
        .SDcols = (campos_media), by=numero_de_cliente]

dataset[, (nuevas_cols) := shift(.SD, n=1L, fill=NA, type="lag"),
        .SDcols = (nuevas_cols), by=numero_de_cliente] 

#----- lag2
nuevas_cols = c()
for(col in campos_media)
{
  nuevas_cols = c(nuevas_cols, paste0(col, "lag2"))
}

dataset[, (nuevas_cols) := shift(.SD, n=2L, fill=NA, type="lag"), 
        .SDcols = (campos_media), by=numero_de_cliente]

#----- lag3
nuevas_cols = c()
for(col in campos_media)
{
  nuevas_cols = c(nuevas_cols, paste0(col, "lag3"))
}

dataset[, (nuevas_cols) := shift(.SD, n=3L, fill=NA, type="lag"), 
        .SDcols = (campos_media), by=numero_de_cliente]

#----- dif1
nuevas_cols = c()
for(col in campos_media)
{
  nuevas_cols = c(nuevas_cols, paste0(col, "_dif1"))
}

dataset[, (nuevas_cols) := shift(.SD, n=1L, fill=NA, type="lag"), 
        .SDcols = (campos_media), by=numero_de_cliente]

for (i in 1:length(campos_media))
{
  dataset[, (nuevas_cols[i]) := dataset[[campos_media[i]]] - dataset[[nuevas_cols[i]]] ]
}

#dataset[, (nuevas_cols) := .SD - shift(.SD, n=1L, fill=NA, type="lag"), 
#                        .SDcols = (campos_media), by=numero_de_cliente] 

#----- dif2
nuevas_cols2 = c()
for (col in campos_media)
{
  nuevas_cols2 = c(nuevas_cols2, paste0(col, "_dif2"))
}

dataset[, (nuevas_cols2) := shift(.SD, n=1L, fill=NA, type="lag"), 
        .SDcols = (nuevas_cols), by=numero_de_cliente]

for (i in 1:length(campos_media))
{
  dataset[, (nuevas_cols2[i]) := dataset[[nuevas_cols[i]]] - dataset[[nuevas_cols2[i]]] ]
}

#dataset[, (nuevas_cols2) := .SD - shift(.SD, n=1L, fill=NA, type="lag"), 
#                        .SDcols = (nuevas_cols), by=numero_de_cliente] 

rm(campos_media, nuevas_cols2, nuevas_cols)




#Final:
  
setwd("~/buckets/b1")
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

# genero la tabla de entrega
tb_entrega <- data.table()

for (semilla in PARAM$finalmodel$semilla){
cat(paste0("Arranco con semilla ",semilla))
#----------------------
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
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0
  
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
  
  extra_trees = TRUE, # Magic Sauce
  
  seed = semilla
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
# aplico el modelo a los datos nuevos
dapply <- dataset[foto_mes == PARAM$input$future]

prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

tb_entrega_aux <- dapply[, list(numero_de_cliente, foto_mes)]

tb_entrega_aux[,semilla:=semilla]
tb_entrega_aux[,prob:=prediccion]

tb_entrega <- rbind(tb_entrega,tb_entrega_aux)

# grabo las probabilidad del modelo
fwrite(tb_entrega,
       file = paste0("prediccion",semilla,".txt"),
       sep = "\t"
)

}



tb_entrega <- tb_entrega[,sum(prob),by=numero_de_cliente]

# ordeno por probabilidad descendente
setorder(tb_entrega, -V1)

# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
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
