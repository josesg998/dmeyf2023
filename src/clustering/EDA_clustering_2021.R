library(data.table)

# construyo los datasets
dataset <- fread("../datasets/competencia_02.csv.gz",stringsAsFactors = T)
dataset1 <- dataset[dataset$clase_ternaria=='BAJA+2']
dataset2 <- dataset[dataset$numero_de_cliente %in% unique(dataset1$numero_de_cliente)]
#importo los clusters
clusters <-fread("../datasets/exp_DRF_2021_DRF_2021.csv",stringsAsFactors = T)

#importo las etiquetas
etiquetas <- readODS::read_ods("../datasets/DiccionarioDatos_2023.ods")

importance <- fread("../datasets/importance_DRF_2021.txt",header = T,dec = ",")
importance <- importance[order(MeanDecreaseGini,decreasing = T)]
importance[,rank:=1:nrow(importance)]

dataset1 <- dataset1[foto_mes>=202101]

#los incorporo al dataset1
dataset1[,label:=clusters$labels_7]

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

temp <- dataset1
temp[,clase_ternaria:=NULL]

#armo las medias de todas las variables numericas
medias <- temp[,lapply(.SD,mean),by=label]
medias[is.na(medias)] <- 0

medias[,rango_cluster:= c(1,3,7,5,2,6,4)]
medias <- medias[order(rango_cluster)]

graficar_campo <- function(campo){

# Agrega etiquetas con los valores de Y
text(
  x = barplot(medias[[campo]], 
              col = viridis::viridis(7), 
              main = paste0(campo," - (Puesto ",importance[importance$rn==campo,rank],")"), 
              names.arg = medias$label,
              ylab = etiquetas[etiquetas$campo==campo,"unidad"]),
  y = medias[[campo]],  # Ajusta la posición vertical de las etiquetas
  labels = round(medias[[campo]],3),  # Etiquetas con los valores de Y
  pos = 1,  # Posición de las etiquetas (3 = arriba)
  col = c("white","white","white","black","black","black","black"),  # Color del texto
  cex = 0.8  # Tamaño del texto
)
  
}

# genero los graficos, uno por hoja
pdf("exp/DRF/medias_clusters_2021.pdf", width = 10, height = 5)


for (campo in importance[,rn]) {
  cat(campo, "  ")
  graficar_campo(campo)
  
  etiqueta <- etiquetas[etiquetas$campo==campo,"Significado"]
  etiqueta <- strwrap(etiqueta,120)
  etiqueta <- paste(etiqueta,collapse="\n")
  
  # Agregar un título (caption) en la parte inferior del gráfico
  mtext(text = etiqueta,side = 1,line=4)
}

dev.off()
