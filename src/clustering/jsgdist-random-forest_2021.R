# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("randomForest")

PARAM <- list()
PARAM$experimento <- "DRF_2021_NAS_imputados"

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

# los meses en los que vamos a entrenar
PARAM$input$training <- c(202101, 202102, 202103, 202104, 202105)


# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
dataset <- dataset[dataset$foto_mes %in% PARAM$input$training]

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

dataset1 <- dataset[dataset$clase_ternaria=='BAJA+2']

dataset1_sin_nas <- na.roughfix(dataset1[,..campos_buenos])

modelo <- randomForest(dataset1_sin_nas, ntree = 1000, proximity=TRUE,oob.prox = T)

proximidades <- modelo$proximity

# Realizar clustering utilizando las proximidades
hclust_result <- hclust(as.dist(1 - proximidades),method = "ward.D2")

for (k in 3:11){
  clusters <- cutree(hclust_result, k = k)
  dataset1[,paste0("labels_",k):= clusters]
}

# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

fwrite(dataset1[,list(numero_de_cliente,
                          labels_3,
                          labels_4,
                          labels_5,
                          labels_6,
                          labels_7,
                          labels_8,
                          labels_9,
                          labels_10,
                          labels_11
                          )],file = paste0(PARAM$experimento, ".csv"),sep = ",")

saveRDS(hclust_result,file="resultado_clustering.rds")
fwrite(data.table(modelo$importance,keep.rownames = T),file="importance.txt",sep='\t',dec = ",")
setwd("../../../../dmeyf2023/exp/DRF/")
saveRDS(hclust_result,file="resultado_clustering.rds")
fwrite(data.table(modelo$importance,keep.rownames = T),file="importance.txt",sep='\t',dec = ",")