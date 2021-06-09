install.packages("pheatmap")
install.packages("foreign")
install.packages("ggplot2")

library(foreign)
library(pheatmap)
library(ggplot2)

#Файлы скачал с сайта, открыл с помощью read.xport из пакета foreign:
demo <- read.xport("DEMO_I.XPT")
bio <- read.xport("BIOPRO_I.XPT")

db <- merge(bio, demo, by = "SEQN")
db2$SEQN <- NULL
dbSDDSRVYR <- NULL
db$RIDAGEMN <- NULL
db$RIDSTATR <-  NULL
db$LBDSCASI <- NULL
women <- subset(db, RIAGENDR == 2)
men <- subset(db, RIAGENDR == 1)

#попарные графики

#матрица корреляций между переменными
matrix <- cor(db, use = "pairwise.complete.obs")
write.csv2(matrix, "matrix.csv")

#тепловая карта
matrix[is.na(matrix)] <- 0
heatmap(matrix)
matrix

matrix[which(is.nan(matrix))] = NA
matrix[which(matrix==Inf)] = NA


cal_z_score <- function(x){
  (x - mean(x)) / sd(x)
}

data_subset_norm <- t(apply(matrix, 1, cal_z_score))
pheatmap(data_subset_norm, display_numbers = F, filename = "diagram10.png")

pheatmap(matrix, display_numbers = TRUE,fontsize = 10,  filename = "diagram4.png")
warnings()
pheatmap(as.matrix(db),display_numbers = T, filename = "diagram.png")
