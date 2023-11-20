
rm(list = ls()) # remove all objects
gc()            # garbage collection

require("data.table")
require("rlist")
require("lightgbm")
# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")
require("zoo")



# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

PARAM$experimento <- "HT8235_casalli"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# los meses en los que vamos a entrenar
#  mucha magia emerger de esta eleccion
PARAM$input$testing <- c(202107)
PARAM$input$validation <- c(202106)
PARAM$input$training <- c(201901, 201902, 201903, 201904, 201905, 201906, 201907, 
                          201908, 201909, 201910, 201911, 201912,202010, 202011,
                          202012, 202101, 202102, 202103,202104,202105)

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$undersampling <- 0.5
PARAM$trainingstrategy$semilla_azar <- 886609 # Aqui poner su  primer  semilla

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

# Aqui poner su segunda semilla
PARAM$lgb_semilla <- 151051
#------------------------------------------------------------------------------

# Hiperparametros FIJOS de  lightgbm
PARAM$lgb_basicos <- list(
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
  num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds
  
  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0
  
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
  
  extra_trees = TRUE, # Magic Sauce
  
  seed = PARAM$lgb_semilla
)


# Aqui se cargan los hiperparametros que se optimizan
#  en la Bayesian Optimization
PARAM$bo_lgb <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.02, upper = 0.1),
  makeNumericParam("feature_fraction", lower = 0.2, upper = 1.0),
  makeIntegerParam("num_leaves", lower = 300L, upper = 2048L),
  makeIntegerParam("min_data_in_leaf", lower = 5000L, upper = 50000L)
)

# si usted es ambicioso, y tiene paciencia, podria subir este valor a 100
PARAM$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana



#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
    reg, arch = NA, folder = "./exp/",
    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)
  
  if (!file.exists(archivo)) # Escribo los titulos
  {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )
    
    cat(linea, file = archivo)
  }
  
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )
  
  cat(linea, file = archivo, append = TRUE) # grabo al archivo
  
  if (verbose) cat(linea) # imprimo por pantalla
}
#------------------------------------------------------------------------------
GLOBAL_arbol <- 0L
GLOBAL_gan_max <- -Inf
vcant_optima <- c()

fganancia_lgbm_meseta <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")
  
  
  GLOBAL_arbol <<- GLOBAL_arbol + 1
  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1,
                   PARAM$hyperparametertuning$POS_ganancia,
                   PARAM$hyperparametertuning$NEG_ganancia  )
  ))
  
  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  
  tbl[, gan_suavizada :=
        frollmean(
          x = gan_acum, n = 2001, align = "center",
          na.rm = TRUE, hasNA = TRUE
        )]
  
  gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]
  
  
  pos <- which.max(tbl[, gan_suavizada])
  vcant_optima <<- c(vcant_optima, pos)
  
  if (GLOBAL_arbol %% 10 == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan
    
    cat("\r")
    cat(
      "Validate ", GLOBAL_iteracion, " ", " ",
      GLOBAL_arbol, "  ", gan, "   ", GLOBAL_gan_max, "   "
    )
  }
  
  
  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbm <- function(x) {
  gc()
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  
  # hago la union de los parametros basicos y los moviles que vienen en x
  param_completo <- c(PARAM$lgb_basicos, x)
  
  param_completo$early_stopping_rounds <-
    as.integer(400 + 4 / param_completo$learning_rate)
  
  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  modelo_train <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalidate),
    eval = fganancia_lgbm_meseta,
    param = param_completo,
    verbose = -100
  )
  
  cat("\n")
  
  cant_corte <- vcant_optima[modelo_train$best_iter]
  
  # aplico el modelo a testing y calculo la ganancia
  prediccion <- predict(
    modelo_train,
    data.matrix(dataset_test[, campos_buenos, with = FALSE])
  )
  
  tbl <- copy(dataset_test[, list("gan" = ifelse(clase_ternaria == "BAJA+2",
                                                 PARAM$hyperparametertuning$POS_ganancia, 
                                                 PARAM$hyperparametertuning$NEG_ganancia))])
  
  tbl[, prob := prediccion]
  setorder(tbl, -prob)
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = 2001,
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]
  
  
  ganancia_test <- tbl[, max(gan_suavizada, na.rm = TRUE)]
  
  cantidad_test_normalizada <- which.max(tbl[, gan_suavizada])
  
  rm(tbl)
  gc()
  
  ganancia_test_normalizada <- ganancia_test
  
  
  # voy grabando las mejores column importance
  if (ganancia_test_normalizada > GLOBAL_gananciamax) {
    GLOBAL_gananciamax <<- ganancia_test_normalizada
    tb_importancia <- as.data.table(lgb.importance(modelo_train))
    
    fwrite(tb_importancia,
           file = paste0("impo_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
           sep = "\t"
    )
    
    rm(tb_importancia)
  }
  
  
  # logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))
  
  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelo_train$best_iter
  xx$estimulos <- cantidad_test_normalizada
  xx$ganancia <- ganancia_test_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion
  
  loguear(xx, arch = "BO_log.txt")
  
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  return(ganancia_test_normalizada)
}



# Aqui empieza el programa

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") 

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")


setwd("~/buckets/b1/") 

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)
# ordernar por mes:
setorderv(dataset, cols=c("foto_mes"), order=c(1L))



# Arreglar datos nulos en algunos meses
  
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



# Llenar con ceros
  
cols_nans_con_ceros = c("mtarjeta_visa_descuentos", "mtarjeta_master_descuentos",
                        "tmobile_app", "cmobile_app_trx")
setnafill(dataset, type="const", fill=0, cols=cols_nans_con_ceros)

rm(cols_nans_con_ceros)



# Arreglar variables con valores no esperados
  
dataset[, internet := fifelse(internet > 1, 1, internet) ]



# Diferencia de estampas por cliente
  
# columna con diferencia de estampas por cliente
dataset[, foto_mes_rang:= match(foto_mes, sort(unique(foto_mes))) ]
dataset[, foto_mes_rangdif := {
  c(NA, diff(foto_mes_rang, lag=1, differences=1))
}, .(numero_de_cliente)]
dataset[, foto_mes_rang := NULL]



# Juntar variables
  
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



# Normalización por inflación
  
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



# Media en ventana, lag2, lag3, dif1, dif2
  
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



if (FALSE)
{
  cols_nrm_rango2 <- c("ccomisiones_otras", "cpayroll_trx",
                       "ccomisiones_mantenimiento", "cextraccion_autoservicio",
                       "catm_trx", "ccajas_consultas",
                       "ccallcenter_transacciones",
                       "cprestamos_prendarios",
                       "ctarjeta_debito_transacciones",
                       "cprestamos_hipotecarios", "cplazo_fijo",
                       "ccuenta_debitos_automaticos",
                       "ctarjeta_visa_debitos_automaticos",
                       "ctarjeta_master_debitos_automaticos",
                       "cpagomiscuentas", "ccajeros_propios_descuentos",
                       "ctarjeta_visa_descuentos", "ctarjeta_master_descuentos",
                       "ccajas_depositos", "ccajas_otras",
                       "catm_trx_other"
  )
  
  cols_nrm_rango = c()
  for(col in cols_nrm_rango2)
  {
    if (col %in% colnames(dataset)){
      cols_nrm_rango = c(cols_nrm_rango, col)
    }
    col1 = gsub("visa", "t", col)
    if ((col1 != col) & (col1 %in% colnames(dataset)))
    {
      cols_nrm_rango = c(cols_nrm_rango, col1)
    }
    col1 = gsub("Visa", "t", col)
    if ((col1 != col) & (col1 %in% colnames(dataset)))
    {
      cols_nrm_rango = c(cols_nrm_rango, col1)
    }
  }
  cols_nrm_rango = unique(cols_nrm_rango)
  rm(cols_nrm_rango2)
  
  nuevas_cols = c()
  for(col in cols_nrm_rango)
  {
    nuevas_cols = c(nuevas_cols, paste0(col, "_nrmr"))
  }
  dataset[, (nuevas_cols) := frank(.SD)/.N,
          .SDcols = (cols_nrm_rango), by=foto_mes]
  
  if (TRUE) # remover cols originales
  {
    for(col in cols_nrm_rango)
    {
      dataset[, (col) := NULL ]
    }
  }
  
  rm(cols_nrm_rango)
}



# ahora SI comienza la optimizacion Bayesiana
setwd("~/buckets/b1/") 
setwd(paste0("./exp/", PARAM$experimento, "/"))

GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_gananciamax <- -1 # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_gananciamax <- tabla_log[, max(ganancia)]
}



# paso la clase a binaria que tome valores {0,1}  enteros
dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
set.seed(PARAM$trainingstrategy$semilla_azar)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, 
                   ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
                          ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)



# defino los datos que forman parte de validation
#  no hay undersampling
dataset[, validation := 0L]
dataset[ foto_mes %in% PARAM$input$validation,  validation := 1L]

dvalidate <- lgb.Dataset(
  data = data.matrix(dataset[validation == 1L, campos_buenos, with = FALSE]),
  label = dataset[validation == 1L, clase01],
  weight = dataset[validation == 1L, 
                   ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
                          ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)


# defino los datos de testing
dataset[, testing := 0L]
dataset[ foto_mes %in% PARAM$input$testing,  testing := 1L]


dataset_test <- dataset[testing == 1, ]

# libero espacio
rm(dataset)
gc()

# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = PARAM$bo_lgb, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600, # se graba cada 600 segundos
  save.file.path = kbayesiana
) # se graba cada 600 segundos

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = PARAM$bo_iteraciones
) # cantidad de iteraciones

# defino el método estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())


# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana) # retomo en caso que ya exista
}


cat("\n\nLa optimizacion Bayesiana ha terminado\n")
