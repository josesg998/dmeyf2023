setwd('~/buckets/b1')
require("data.table")

pengue <- fread("exp/KA8240_03_pengue_marchesini/prediccion.txt")
casalli <- fread("exp/final_casalli/prediccion.txt")
serpa <- fread('exp/KA8240_serpa/prediccion.txt')
flores <- fread('exp/KA5240_vanesa_flores/prediccion.txt')
lags <- fread('exp/exp_final/V4_2_prob.csv')[,list(numero_de_cliente,foto_mes,prob)]
moda2 <- fread('exp/KA8240_mode2/prediccion.csv')[,list(numero_de_cliente,foto_mes,prob)]
moda <- fread('exp/KA8240_mode/prediccion.csv')[,list(numero_de_cliente,foto_mes,prob)]

df <- rbind(pengue,
            casalli,
            serpa,moda,lags,moda2,
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
