# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("randomForest")

PARAM <- list()
PARAM$experimento <- "DRF"

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

# los meses en los que vamos a entrenar
#PARAM$input$training <- c(201901, 201902, 201903, 201904, 201905,201906,201907,
#                          201908,201909,201910,201911,201912,
#                          202101, 202102, 202103, 202104, 202105)


# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

dataset1 <- dataset[dataset$clase_ternaria=='BAJA+2']

dataset2 <- dataset[dataset$numero_de_cliente %in% unique(dataset1$numero_de_cliente)]

dataset1[is.na(dataset1), ] <- 0

modelo <- randomForest(dataset1[,..campos_buenos], ntree = 1000, proximity=TRUE)

proximidades <- modelo$proximity

# Realizar clustering utilizando las proximidades
hclust_result <- hclust(as.dist(1 - proximidades))

# Realiza el corte en el dendrograma para obtener 7 clusters
clusters <- cutree(hclust_result, k = 7)

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
