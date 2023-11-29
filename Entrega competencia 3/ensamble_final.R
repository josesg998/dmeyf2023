setwd('~/buckets/b1')
require("data.table")

# entrega de Pengue y Marchesini en competencia 02
pengue <- fread("exp/KA8240_03_pengue_marchesini/prediccion.txt")

# entrega de Casali en competencia 02
casalli <- fread("exp/final_casalli/prediccion.txt")

# entrega de Serpa en competencia 02
serpa <- fread('exp/KA8240_serpa/prediccion.txt')

# Entrega de Flores en competencia 02
flores <- fread('exp/KA5240_vanesa_flores/prediccion.txt')

# Experimento de lags de Bettachini y Kalousek
## con 7 meses
lags <- fread('exp/exp_final/V4_2_prob.csv')[,list(numero_de_cliente,foto_mes,prob)]
## con varios meses
lags2 <- fread('exp/exp_final_mas_meses/V4_2_mas_meses_prob.csv')[,list(numero_de_cliente,foto_mes,prob)]

# experimento de neg_bagging_fraction de Boemo y Clas
## con 7 meses
desbalanceo <- fread('exp/final_desbalanceo/prediccion.txt')
## con mas meses
desbalanceo2 <- fread('exp/final_desbalanceo_mas_meses/prediccion.txt')

# Experimento de corrección de variables rotas de Serpa y Saint Germain
## Con 7 meses
moda <- fread('exp/KA8240_mode/prediccion.csv')[,list(numero_de_cliente,foto_mes,prob)]
## Con mas meses
moda2 <- fread('exp/KA8240_mode2/prediccion.csv')[,list(numero_de_cliente,foto_mes,prob)]


# ensamblamos todo y hacemos los cortes
df <- rbind(pengue,
            casalli,
            serpa,
            moda,moda2,
            lags,lags2,
            desbalanceo,desbalanceo2,
            flores)
df <- df[,sum(prob),by=numero_de_cliente]
df <- setorder(df,-V1)

dir.create("exp/ensamble_1")
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  df[, Predicted := 0L]
  df[1:envios, Predicted := 1L]
  
  fwrite(df[, list(numero_de_cliente, Predicted)],
         file = paste0("exp/ensamble_1/envio_ensamble", "_", envios, ".csv"),
         sep = ","
  )
}
