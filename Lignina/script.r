nombres <- list.files(path="/home/felipe/Lignina/2", pattern=".CSV")
datos <- lapply(nombres, read.csv, header=FALSE)

datos2 <- lapply(datos, "[", 	2) 
write.table(datos2, "datos2.txt", sep="\t")
matriz <- read.table(file="datos2.txt",header=TRUE) 
espectro129 <- matriz[,13]


matriz2 <- matriz[,-13] 


promedios <- matrix(,ncol=95, nrow=1869)

for (i in 1:1869){
	for (j in 1:95){
	promedios[i,j] <- NA
	}
}		

for (k in 1:1869) {

for (m in 1:95){

	promedios[k,m] <- (matriz2[k,(3*m)-2] + matriz2[k,((3*m)-2)+1] + matriz2[k,(((3*m)-2)+2)])/3 
}	
}


