#setwd("/home/felipe/Lignina/3/129")
nombres <- list.files(path="/home/felipe/Lignina/3/129", pattern=".CSV")
tresdatos <- lapply(nombres, read.csv, header=FALSE)
datos2 <- lapply(tresdatos, "[", 	2) 
write.table(datos2, "tresdatos.txt", sep="\t")
matriz <- read.table(file="tresdatos.txt",header=TRUE) 
long <-lapply(tresdatos, "[", 	1)
long2 <- long[[1]][[1]]

promedio129 <- matrix(, ncol=1, nrow=1869 )
for (i in 1:1869) {
 for (j in 1:1){
  promedio129[i,j] <- NA
}		
}		


for (m in 1:1869) {
 for (n in 1:1){
  promedio129[m,n] <- ( matriz[m,1]  + matriz[m,2] + matriz[m,3] )/ 3
}		
}	

write.table(promedio129, file="promedio129.txt", sep=";", dec=".")


todo129 <-data.frame(longitud= long2, primero = matriz[,1], segundo = matriz[,2], tercero = matriz[,3] , promedio= promedio129[,1])
a <- 0.05
colores <- matrix(c("green","red","blue","darkgreen","black"), ncol=1)
tipo <- matrix(c(0,1,1,1,2), ncol=1)
pdf(file="129.pdf")
for (i in 2:5){

matplot(todo129$longitud,  todo129[,i] ,  col=colores[i,1], type="l", ylab="Abs" ,xlab="Wavenumber" , ylim=(0:1/10) ,lty=tipo[i,1], xlim=c(1900,800))


legend("topright", c("129-1", "129-2", "129-3", "129-prom"), col=c("red","blue","darkgreen","black"), lty=1)

readline("new")
par(new=T)
}		
abline(v=c(1330,1335), col=alpha(rgb(0,0,0), 0.2))
abline(v=c(1270,1266), col=alpha(rgb(0,0,0), 0.2))
abline(v=c(1128,1125), col=alpha(rgb(0,0,0), 0.2))



dev.off()


#vec <- matrix(, ncol=1, nrow=96)

#for (i in 1:96)
#vec[,1]<-

# Hacer un if, else con 1, 2 y 3 leyendo una columna de un objeto creado a partir de archivo de texto plano creado en hoja de cálculo con las clases de cada muestra, de longitud 96 ( o que concuerde con la cantidad de muestras)

#el vector resultado se añade a la matriz o se usa directamente en text()

