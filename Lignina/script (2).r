nombres <- list.files(path="/home/felipe/Lignina", pattern=".CSV")
datos <- lapply(nombres, read.csv, header=FALSE)

datos2 <- lapply(datos, "[", 	2) #extraigo todas las columnas de absorbancia, en una lista de dataframes
matriz2 <- lapply(datos2, rbind)
write.table(matriz2, "matriz2.txt", sep="\t")
matriz3 <- read.table(file="matriz2.txt",header=TRUE)	

