# Optimizacion Bayesiana de hiperparametros de  rpart
# que va directamente contra el Public Leaderboard
# este script AUN no entrena en un dataset con oversampling de los BAJA+2

# dedicado a Federico Idoeta, Impossible is Nothing,  02-sep-2022


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

require("lightgbm")

# Defino la  Optimizacion Bayesiana
PARAM <- list()
PARAM$experimento <- "HT3990_LGBM"

# cantidad de iteraciones de la Optimizacion Bayesiana
PARAM$BO_iter <- 24  # iteraciones inteligentes   24= 40 - 4*4

PARAM$input$training <- c(201901, 201902, 201903, 201904, 201905,201906,201907,
                          201908,201909,201910,201911,201912,
                          202101, 202102, 202103,202104)
PARAM$input$testing <- c(202105)

# Aqui se cargan los hiperparametros que se optimizan
#  en la Bayesian Optimization
PARAM$bo_lgb <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.02, upper = 0.3),
  makeNumericParam("feature_fraction", lower = 0.01, upper = 1.0),
  makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
  makeIntegerParam("min_data_in_leaf", lower = 100L, upper = 50000L),
  makeIntegerParam("corte", lower = 8000L, upper = 15000L)
)


# un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$undersampling <- 1.0
PARAM$semilla_azar <- 290497 # Aqui poner su  primer  semilla

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

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
  
  seed = PARAM$semilla_azar
)
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

loguear <- function(reg, arch = NA, folder = "./work/", ext = ".txt",
                    verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  # Escribo los titulos
  if (!file.exists(archivo)) {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )

    cat(linea, file = archivo)
  }

  # la fecha y hora
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
    gsub(", ", "\t", toString(reg)), "\n"
  )

  # grabo al archivo
  cat(linea, file = archivo, append = TRUE)

  # imprimo por pantalla
  if (verbose) cat(linea)
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros, lamentablemente se pasan como variables globales
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


EstimarGanancia_lightgbm <- function(x) {
  gc()
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  
  # hago la union de los parametros basicos y los moviles que vienen en x
  param_completo <- c(PARAM$lgb_basicos, x)
  
  #param_completo$early_stopping_rounds <-
  #  as.integer(400 + 4 / param_completo$learning_rate)
  
  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()
  set.seed(PARAM$semilla_azar, kind = "L'Ecuyer-CMRG")
  modelo_train <- lgb.train(
    data = dtrain,
    #valids = list(valid = dvalidate),
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
                                                 PARAM$hyperparametertuning$NEG_ganancia),
                                  numero_de_cliente)])
  tbl[, prob := prediccion]
  setorder(tbl, -prob)
  tbl[,Predicted:=0L]
  tbl[ 1:param_completo$corte, Predicted := 1L ]

  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = 2001,
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]
  
  
  ganancia_test <- tbl[, max(gan_suavizada, na.rm = TRUE)]
  
  cantidad_test_normalizada <- which.max(tbl[, gan_suavizada])
  
  
  ganancia_test_normalizada <- ganancia_test
  nom_submit <- paste0("TEGANAREJRAMAN_", sprintf( "%03d", GLOBAL_iteracion ), ".csv" )
  fwrite( tbl[ , list(numero_de_cliente, Predicted)],
          file= nom_submit,
          sep= "," )
  rm(tbl)
  gc()

  # solicito que el humano a cargo ingrese la ganancia publica
  mensaje <- paste0( "haga el submit a Kaggle de ", nom_submit,
                     " y cargue la ganancia publica : " )
  
  ganancia_public <- leer_verificado( mensaje )

  # logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))
  
  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelo_train$best_iter
  xx$estimulos <- cantidad_test_normalizada
  xx$ganancia <- ganancia_test_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion
  
  loguear(xx, arch = "BO_log.txt")
  
  set.seed(PARAM$semilla_azar, kind = "L'Ecuyer-CMRG")
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Establezco el Working Directory
setwd("~/buckets/b1/")

# cargo los datos
dataset <- fread("./datasets/competencia_02.csv.gz")

# los campos que se van a utilizar
columnas_a_calcular <- setdiff(colnames(dataset), c("numero_de_cliente","foto_mes","clase_ternaria", "clase01"))

#--------------------------------------


#for (mes in unique(dataset$foto_mes)){
#  for (columna in columnas_a_calcular){
#    if (all(dataset[foto_mes==mes,{{columna}}]==0)==TRUE){
#      dataset[dataset$foto_mes==mes,{{columna}}] = NA
#    }
#  }
#}


#--------------------------------------
# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))



#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso !  corta la bocha

columnas_a_calcular <- campos_buenos[!campos_buenos %in% c("numero_de_cliente","foto_mes")]

dataset[, paste((columnas_a_calcular),"lag_1",sep="_") := lapply(.SD, function(x) shift(x, type = "lag", n = 1)), 
        by = numero_de_cliente, .SDcols = columnas_a_calcular]

dataset[, paste((columnas_a_calcular),"lag_2",sep="_") := lapply(.SD, function(x) shift(x, type = "lag", n = 2)), 
        by = numero_de_cliente, .SDcols = columnas_a_calcular]


dataset[, (paste(columnas_a_calcular, "rank", sep = "_")) := lapply(.SD, function(x) rank(-x)),
        by = foto_mes, .SDcols = columnas_a_calcular]


# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))


# creo la carpeta donde va el experimento
#  HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create( paste0("./exp/", PARAM$experimento, "/"), 
           showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd( paste0("./exp/", PARAM$experimento, "/") )


# en estos archivos quedan los resultados
archivo_log <- "BO_log.txt"
archivo_BO <- "bayesian.RDATA"

# leo si ya existe el log

GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_gananciamax <- -1 # inicializo la variable global

if (file.exists(archivo_log)) {
  tabla_log <- fread(archivo_log)
  GLOBAL_iteracion <- nrow(tabla_log)
}



# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
set.seed(PARAM$semilla_azar)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

# paso la clase a binaria que tome valores {0,1}  enteros
dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]

# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
set.seed(PARAM$semilla_azar)
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

# defino los datos de testing
dataset[, testing := 0L]
dataset[ foto_mes %in% PARAM$input$testing,  testing := 1L]


dataset_test <- dataset[testing == 1, ]

# libero espacio
rm(dataset)
gc()

# en estos archivos quedan los resultados
archivo_log <- "BO_log.txt"
archivo_BO <- "bayesian.RDATA"

# leo si ya existe el log
#  para retomar en caso que se se corte el programa
GLOBAL_iteracion <- 0

if (file.exists(archivo_log)) {
  tabla_log <- fread(archivo_log)
  GLOBAL_iteracion <- nrow(tabla_log)
}

# Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar <- EstimarGanancia_lightgbm

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,
#  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
# minimize= FALSE estoy Maximizando la ganancia
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar,
  minimize = FALSE,
  noisy = TRUE,
  par.set = PARAM$bo_lgb,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl(
  save.on.disk.at.time = 600,
  save.file.path = archivo_BO
)

ctrl <- setMBOControlTermination(ctrl, iters = PARAM$BO_iter)
ctrl <- setMBOControlInfill(ctrl,  crit = makeMBOInfillCritEI())

surr.km <- makeLearner("regr.km",
  predict.type = "se",
  covtype = "matern3_2", control = list(trace = TRUE)
)


# para que mlrMBO tenga todo reseteado
set.seed( PARAM$semilla_azar )

# inicio la optimizacion bayesiana
if (!file.exists(archivo_BO)) {
  run <- mbo(
    fun = obj.fun,
    learner = surr.km,
    control = ctrl
  )
} else {
  run <- mboContinue(archivo_BO)
}
# retomo en caso que ya exista
