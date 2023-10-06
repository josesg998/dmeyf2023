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

dataset1 <- dataset[dataset$clase_ternaria=='BAJA+2']

dataset2 <- dataset[dataset$numero_de_cliente %in% unique(dataset1$numero_de_cliente)]

dataset1[is.na(dataset1), ] <- 0

proximidades <- randomForest(dataset1, ntree = 1000, proximity=TRUE)$proximity

# Realizar clustering utilizando las proximidades
clusters <- hclust(as.dist(1 - proximidades))

dataset1[labels:= clusters$labels]

# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

fwrite(proximidades,file = paste0(PARAM$experimento, ".csv"),sep = ",")

