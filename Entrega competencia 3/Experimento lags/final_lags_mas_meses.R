## -------------------------------------------------------------------------------------------------
rm(list = ls()) # remove all objects
gc()            # garbage collection

require("data.table")
require("rlist")
require("lightgbm")


## -------------------------------------------------------------------------------------------------
PARAM <- list()
PARAM$experimento <- "exp_final_mas_meses"
PARAM$nombreexp <- "V4_2_mas_meses"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(201909, 201910, 201911, 201912, 202001, 202002, 
                          202009, 202010, 202011, 202012, 202101, 202102, 202103,
                          202104,202105,202106,202107)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo
PARAM$finalmodel$semilla <- 151051

# V4
PARAM$finalmodel$optim$num_iterations <- 1181
PARAM$finalmodel$optim$learning_rate <- 0.031810
PARAM$finalmodel$optim$feature_fraction <- 0.683272
PARAM$finalmodel$optim$min_data_in_leaf <- 19603
PARAM$finalmodel$optim$num_leaves <- 624

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

  seed = PARAM$finalmodel$semilla
)


## -------------------------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
# ordernar por mes:
setorderv(dataset, cols=c("foto_mes"), order=c(1L))


## -------------------------------------------------------------------------------------------------
meses_a_usar = c(202003, 202004, 202005, 202006, 202007, 202008, 202009,
                 202010, 202011, 202012, 202101, 202102, 202103, 202104, 202105, 202106, 202107,202108,202109)

dataset = dataset[foto_mes %in% meses_a_usar]

if (FALSE) {  # Si se quiere borrar información de la pandemia
  meses_pandemia = c(202003, 202004, 202005, 202006, 202007, 202008, 202009)
  cols_a_borrar = setdiff( colnames(dataset), c("numero_de_cliente", "foto_mes"))
  dataset[foto_mes %in% meses_pandemia, (cols_a_borrar) := NA]
  rm(cols_a_borrar)
} else {      # Si se quiere mantener información de la pandemia
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
                            "tmobile_app", "cmobile_app_trx", "mrentabilidad", "mrentabilidad_annual",
                            "mcomisiones", "mactivos_margen", "mpasivos_margen", "ccomisiones_otras",
                            "mcomisiones_otras", "chomebanking_transacciones")
  dataset[foto_mes==202006, (cols_reemplazar_ceros) := NA]
  rm(cols_reemplazar_ceros)
}



## -------------------------------------------------------------------------------------------------
cols_con_lag <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "foto_mes", "numero_de_cliente",
    "cliente_edad", "cliente_antiguedad")
)

#----- media
if (TRUE) {
  n <- 5L
  cols_media = c()
  for(col in cols_con_lag)
  {
    cols_media = c(cols_media, paste0(col, "_media_", n))
  }

  dataset[, (cols_media) := frollmean(.SD, n=(n), fill=NA, na.rm=TRUE, align="right", algo="fast"),
                        .SDcols = (cols_con_lag), by=numero_de_cliente]

  dataset[, (cols_media) := shift(.SD, n=1L, fill=NA, type="lag"),
                        .SDcols = (cols_media), by=numero_de_cliente]

  rm(cols_media, n)
}


#----- lags
n_lags = c(1,3,6)

for (i in n_lags)
{
  cols_lag = c()
  for(col in cols_con_lag)
  {
    cols_lag = c(cols_lag, paste0(col, "_lag_", i))
  }
  dataset[, (cols_lag) := shift(.SD, n=(i), fill=NA, type="lag"),
                        .SDcols = (cols_con_lag), by=numero_de_cliente]

  rm(cols_lag)
}


#----- delta lags
if (TRUE) {
  for (i in n_lags)
  {
    for(col in cols_con_lag)
    {
      col_lag = paste0(col, "_lag_", i)
      col_delta_lag = paste0(col, "_delta_", i)
      dataset[, (col_delta_lag) := get(col) - get(col_lag)]
    }
    rm(col_lag, col_delta_lag)
  }
}

rm(cols_con_lag)


## -------------------------------------------------------------------------------------------------
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

# genero el modelo
param_completo <- c(
  PARAM$finalmodel$lgb_basicos,
  PARAM$finalmodel$optim)

semillas = c(886609)#, 201821, 623423, 105389, 151051,
             #978323, 594421, 797897, 911159, 892627,
             #605167, 982337, 178807, 596053, 435583,
             #451547, 970699, 717659, 671303, 345647)

tb_fs = data.table(semilla = semillas,
                   gan_max = 0,
                   envios_max = 0)

#for (s in semillas) {
param_completo$seed = semillas[1]

  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )

  #--------------------------------------
  # aplico el modelo a los datos nuevos
  dapply <- dataset[foto_mes == PARAM$input$future]

  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )

  # genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  tb_entrega[, prob := prediccion]
  # ordeno por probabilidad descendente
  setorder(tb_entrega, -prob)

  cortes <- seq(8000, 15000, by = 500)
  #tb_f <- data.table(nenvios = cortes)
  #tb_f[, gan := 0]

  for (envios in cortes) {
    tb_entrega[, Predicted := 0L]
    tb_entrega[1:envios, Predicted := 1L]

    #tb_entrega[, gan := fifelse((Predicted == 1) & (clase_ternaria == "BAJA+2"),
                        #        273000, -7000)]

    #tb_entrega[, gan_acum := cumsum(gan)]
    #gan_tot = tb_entrega[envios, gan_acum]
    #tb_f[nenvios == envios, gan := gan_tot]
  }

  #tb_f[, gan_suavizada :=
  #  frollmean(
  #    x = gan, n = 3, align = "center",
  #    na.rm = FALSE, hasNA = FALSE
  #  )]

  #gan_max_ <- tb_f[, max(gan_suavizada, na.rm = TRUE)]
  #pos_max_ <- which.max(tb_f[, gan_suavizada])
  #envios_max_ <- tb_f[pos_max_, nenvios]

  #tb_fs[ semilla == s, gan_max := (gan_max_) ]
  #tb_fs[ semilla == s, envios_max := (envios_max_) ]
#}

#fwrite(tb_fs,
#  file = paste0(PARAM$nombreexp, "_res.csv"),
#  sep = ","
#)

fwrite(tb_entrega,
       file = paste0(PARAM$nombreexp, "_prob.csv"),
       sep = ","
)

