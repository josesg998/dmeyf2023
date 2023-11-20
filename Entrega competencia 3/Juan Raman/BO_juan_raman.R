# Este script esta pensado para correr en Google Cloud
#   8 vCPU
# 128 GB memoria RAM

# se entrena con clase_binaria2  POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm,

# Personalizacion :
#   1. Voy contra el Public Leaderboard como en primera competencia
#   2. ganancia_hibrida =  Public + Validate (magia negra)
#   3. 11000 estimulos FIJO
#   4. feature_fraction = 0.80  FIJO (no va a la bayesiana)
#   5. undersampling del 10%
#   6. Catastrophe Analysis  reemplazo ceros por NA
#   7. lags y delta lags  1, 2 y 6

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("lightgbm")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

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

PARAM$experimento <- "HT8530_juan_raman"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# los meses en los que vamos a entrenar
#  mucha magia emerger de esta eleccion
PARAM$input$testing <- c(202109)
PARAM$input$validation <- c(202107)
PARAM$input$training <- c( 201901, 201902, 201903, 201904, 201905, 202906,
                           201907, 201908,201908, 201909, 201910, 201911, 201912, 202001, 202002,
                           # huevo de la pandemia
                           202010, 202011, 202012, 202101, 202102, 202103, 202104,202105,202106)

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$undersampling <- 0.1
PARAM$trainingstrategy$semilla_azar <- 161729 # Aqui poner su  primer  semilla

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

# Aqui poner su segunda semilla
PARAM$lgb_semilla <- 221729

PARAM$lgb_corte <- 11000  # valor estimado previamente
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
  feature_fraction = 0.8, # FIJO
  
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
  makeIntegerParam("num_leaves", lower = 4L, upper = 100L),
  makeIntegerParam("min_data_in_leaf", lower = 1000L, upper = 20000L)
)

# si usted es ambicioso, y tiene paciencia, podria subir este valor a 100
PARAM$bo_iteraciones <- 40 - 12 # iteraciones de la Optimizacion Bayesiana

#------------------------------------------------------------------------------

leer_numero <- function( mensaje ) {
  res <- readline( mensaje )
  while( is.na( as.numeric( res ))) {
    cat( "Debe introducit un numero, el separador decimal es la coma\n" )
    res <- readline( mensaje )
  }
  
  return( as.numeric(res) )
}
#------------------------------------------------------------------------------

leer_verificado <- function( mensaje ) {
  repeat {
    
    n1 <- leer_numero( mensaje )
    cat( "Por favor, vuelva a cargar el mismo numero\n" )
    n2 <- leer_numero( mensaje )
    
    if( n1 != n2 )  cat( "Los numeros no coinciden, debe volver a cargar\n\n" )
    if( n1== n2 ) break
  }
  
  return( n1 )
}
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
  
  tbl <- copy(dataset_test[, list(numero_de_cliente)])
  
  tbl[, prob := prediccion]
  setorder(tbl, -prob)
  
  tbl[, Predicted := 0L ]
  tbl[ 1:PARAM$lgb_corte, Predicted := 1L ]
  
  nom_submit <- paste0("KA_", sprintf( "%03d", GLOBAL_iteracion ), ".csv")
  
  fwrite( tbl[, list(numero_de_cliente, Predicted)],
          file= nom_submit,
          sep= "," )
  
  # solicito que el humano a cargo ingrese la ganancia publica
  mensaje <- paste0( "haga el submit a Kaggle de ", nom_submit,
                     " y cargue la ganancia publica en millones : " )
  
  ganancia_public <- leer_verificado( mensaje )
  
  rm(tbl)
  gc()
  
  # hibrida = Validation + Public
  ganancia_hibrida <- round(GLOBAL_gan_max/1000000,2) + ganancia_public
  
  # voy grabando las mejores column importance
  if (ganancia_hibrida > GLOBAL_gananciamax) {
    GLOBAL_gananciamax <<- ganancia_hibrida
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
  xx$estimulos <- PARAM$lgb_corte
  
  xx$Validation <- round(GLOBAL_gan_max/1000000,2)
  xx$Public <- ganancia_public
  xx$ganancia <- ganancia_hibrida
  xx$iteracion_bayesiana <- GLOBAL_iteracion
  
  loguear(xx, arch = "BO_log.txt")
  
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  return(ganancia_hibrida)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)
setorder( dataset, numero_de_cliente, foto_mes )

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")



# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]
dataset[foto_mes == 201901, ctransferencias_recibidas := NA]
dataset[foto_mes == 201901, mtransferencias_recibidas := NA ]

dataset[foto_mes == 201902, ctransferencias_recibidas := NA]
dataset[foto_mes == 201902, mtransferencias_recibidas := NA]

dataset[foto_mes == 201903, ctransferencias_recibidas := NA]
dataset[foto_mes == 201903, mtransferencias_recibidas := NA]

dataset[foto_mes == 201904, ctarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201904, ctransferencias_recibidas := NA]
dataset[foto_mes == 201904, mtransferencias_recibidas := NA]
dataset[foto_mes == 201904, mttarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201904, Visa_mfinanciacion_limite := NA]

dataset[foto_mes == 201905, ccomisiones_otras := NA]
dataset[foto_mes == 201905, ctarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201905, ctransferencias_recibidas := NA]
dataset[foto_mes == 201905, mactivos_margen := NA]
dataset[foto_mes == 201905, mcomisiones := NA]
dataset[foto_mes == 201905, mcomisiones_otras := NA]
dataset[foto_mes == 201905, mpasivos_margen := NA]
dataset[foto_mes == 201905, mrentabilidad_annual := NA]
dataset[foto_mes == 201905, mrentabilidad := NA]
dataset[foto_mes == 201905, mtransferencias_recibidas := NA]

dataset[foto_mes == 201910, ccajeros_propios_descuentos := NA]
dataset[foto_mes == 201910, ccomisiones_otras := NA]
dataset[foto_mes == 201910, chomebanking_transacciones := NA]
dataset[foto_mes == 201910, ctarjeta_master_descuentos := NA]
dataset[foto_mes == 201910, ctarjeta_visa_descuentos := NA]
dataset[foto_mes == 201910, mactivos_margen := NA]
dataset[foto_mes == 201910, mcajeros_propios_descuentos := NA]
dataset[foto_mes == 201910, mcomisiones := NA]
dataset[foto_mes == 201910, mcomisiones_otras := NA]
dataset[foto_mes == 201910, mpasivos_margen := NA]
dataset[foto_mes == 201910, mrentabilidad_annual := NA]
dataset[foto_mes == 201910, mrentabilidad := NA]
dataset[foto_mes == 201910, mtarjeta_master_descuentos := NA]
dataset[foto_mes == 201910, mtarjeta_visa_descuentos := NA]

dataset[foto_mes == 202001, cliente_vip := NA]

dataset[foto_mes == 202006, active_quarter := NA]
dataset[foto_mes == 202006, catm_trx := NA]
dataset[foto_mes == 202006, catm_trx_other := NA]
dataset[foto_mes == 202006, ccajas_consultas := NA]
dataset[foto_mes == 202006, ccajas_depositos := NA]
dataset[foto_mes == 202006, ccajas_extracciones := NA]
dataset[foto_mes == 202006, ccajas_otras := NA]
dataset[foto_mes == 202006, ccajas_transacciones := NA]
dataset[foto_mes == 202006, ccallcenter_transacciones := NA]
dataset[foto_mes == 202006, ccheques_depositados := NA]
dataset[foto_mes == 202006, ccheques_depositados_rechazados := NA]
dataset[foto_mes == 202006, ccheques_emitidos := NA]
dataset[foto_mes == 202006, ccheques_emitidos_rechazados := NA]
dataset[foto_mes == 202006, ccomisiones_otras := NA]
dataset[foto_mes == 202006, cextraccion_autoservicio := NA]
dataset[foto_mes == 202006, chomebanking_transacciones := NA]
dataset[foto_mes == 202006, cmobile_app_trx := NA]
dataset[foto_mes == 202006, ctarjeta_debito_transacciones := NA]
dataset[foto_mes == 202006, ctarjeta_master_transacciones := NA]
dataset[foto_mes == 202006, ctarjeta_visa_transacciones := NA]
dataset[foto_mes == 202006, ctrx_quarter := NA]
dataset[foto_mes == 202006, mactivos_margen := NA]
dataset[foto_mes == 202006, matm := NA]
dataset[foto_mes == 202006, matm_other := NA]
dataset[foto_mes == 202006, mautoservicio := NA]
dataset[foto_mes == 202006, mcheques_depositados := NA]
dataset[foto_mes == 202006, mcheques_depositados_rechazados := NA]
dataset[foto_mes == 202006, mcheques_emitidos := NA]
dataset[foto_mes == 202006, mcheques_emitidos_rechazados := NA]
dataset[foto_mes == 202006, mcomisiones := NA]
dataset[foto_mes == 202006, mcomisiones_otras := NA]
dataset[foto_mes == 202006, mcuentas_saldo := NA]
dataset[foto_mes == 202006, mextraccion_autoservicio := NA]
dataset[foto_mes == 202006, mpasivos_margen := NA]
dataset[foto_mes == 202006, mrentabilidad_annual := NA]
dataset[foto_mes == 202006, mrentabilidad := NA]
dataset[foto_mes == 202006, mtarjeta_master_consumo := NA]
dataset[foto_mes == 202006, mtarjeta_visa_consumo := NA]
dataset[foto_mes == 202006, tcallcenter := NA]
dataset[foto_mes == 202006, thomebanking := NA]


# Data Drifting
# por ahora, no hago nada


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso !  corta la bocha
# defino las columnas a las que les puedo calcular el lag
cols_lagueables <- copy(setdiff( colnames(dataset),
                                 c("numero_de_cliente", "foto_mes", "clase_ternaria") ))

# lags de orden 1
dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
        by = numero_de_cliente,
        .SDcols = cols_lagueables
]

# agrego los delta lags de orden 1
for (vcol in cols_lagueables) dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]


# lags de orden 2
dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
        by = numero_de_cliente,
        .SDcols = cols_lagueables
]

# agrego los delta lags de orden 2
for (vcol in cols_lagueables) dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]


# lags de orden 6
dataset[, paste0(cols_lagueables, "_lag6") := shift(.SD, 6, NA, "lag"),
        by = numero_de_cliente,
        .SDcols = cols_lagueables
]

# agrego los delta lags de orden 6
for (vcol in cols_lagueables) dataset[, paste0(vcol, "_delta6") := get(vcol) - get(paste0(vcol, "_lag6"))]


# ahora SI comienza la optimizacion Bayesiana

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