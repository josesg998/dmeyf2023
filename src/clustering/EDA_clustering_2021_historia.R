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


campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

dataset2 <- merge(dataset2,clusters,by="numero_de_cliente")
dataset2$mes <- as.Date(paste(dataset2$foto_mes, "01", sep = ""), format = "%Y%m%d")

graficar_campo <- function(campo,cluster){


df <- dataset2[dataset2$labels_7==cluster,mean(get(campo),na.rm=T),by=mes]
df <- df[order(mes)]
plot(x = df$mes,y = df$V1,main=paste0(campo," - cluster ",cluster),type="o",ylab =etiquetas[etiquetas$campo==campo,"unidad"],xlab="",xaxt="n")
axis.Date(1, at = seq(min(df$mes), max(df$mes), by = "4 month"), format = "%b-%y")
etiqueta <- etiquetas[etiquetas$campo==campo,"Significado"]
etiqueta <- strwrap(etiqueta,120)
etiqueta <- paste(etiqueta,collapse="\n")

# Agregar un título (caption) en la parte inferior del gráfico
mtext(text = etiqueta,side = 1,line=4)


}

# genero los graficos, uno por hoja
pdf("exp/DRF/historia_clusters.pdf", width = 10, height = 5)

for (campo in importance[,rn]){
  cat(campo, "  ")
  for (cluster in 1:7){
    graficar_campo(campo,cluster)
  }
}

dev.off()

