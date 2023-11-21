setwd('~/buckets/b1')
require("data.table")

pengue <- fread("exp/KA8240_03_pengue_marchesini/prediccion.txt")
casalli <- fread("exp/final_casalli/prediccion.txt")

df <- merge(pengue,casalli,by=c("numero_de_cliente","foto_mes"))


dir.create("exp/ensamble_1")
df <- setorder(df,-prob)
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  df[, Predicted := 0L]
  df[1:envios, Predicted := 1L]
  
  fwrite(df[, list(numero_de_cliente, Predicted)],
         file = paste0("exp/ensamble_1/envio_ensamble", "_", envios, ".csv"),
         sep = ","
  )
}
