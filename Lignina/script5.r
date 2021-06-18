m <- read.table("matrizbien.txt")


m2 <- m[,-2]

mt <- t(m2)
colnames(mt) <- as.character(unlist(mt[1,]))
mt2 <- mt[-1, ]
mt3 <- mt2 [,-1869]
mt4 <- mt3[,209:779]
mt42 <- mt4[-c(4,5,96),]

b <- prcomp(mt4, scale=T)

#valores propios
vp<-(b$sdev)^2
#varianza en porcentaje
varianza<-(vp*100)/sum(vp)
#varianza acumulada
varianza.acumulada<-cumsum(varianza)
#juntando todo en un data frame
matriz.de.varianzas<-data.frame(valores.propios=vp, varianza=varianza, varianza.acumulada=varianza.acumulada)
pdf(file="barrastodos.pdf")

barplot(matriz.de.varianzas[,2],names.arg=1:nrow(matriz.de.varianzas),       
    main = "varianzas",
    xlab = "componentes principales",
    ylab = "varianzas en porcentaje",
    col = "aquamarine2",
    )
readline("varianzas en porcentaje")
dev.off()
ind.coord<- b$x

#pdf(file="sinescalar.pdf")


clase <-matrix(c(1,2,3,1,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,1,3,1,3,1,3,1,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,4,2,5,3,6,7,1,8,2,9,3,1,2,3,1,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),nrow=96,ncol=1)
color <- matrix(, nrow=1, ncol=96)
for(i in 1:96){
   color[i]<-"NA" ## inicialziar variables
    if(clase[i] == 1){color[i]<-"red"}else{if(clase[i]==2){color[i]<-"blue"}else{if(clase[i]== 3){color[i]<-"green"}else{color[i]<-"black"}}}
} 

pdf(file="indtodos.pdf")

plot(ind.coord[,1], ind.coord[,2], pch = 19, col=color[1,],
xlab="PC1 - 84.42%",ylab="PC2 - 11.35%")
abline(h=0, v=0, lty = 2)
text(ind.coord[,1], ind.coord[,2], labels=rownames(ind.coord),
cex=0.7, pos = 3)


dev.off()

readline("individuos")

z <-  prcomp(mt42, scale=T)



#valores propios
vp2<-(z$sdev)^2
#varianza en porcentaje
varianza2<-(vp2*100)/sum(vp2)
#varianza acumulada
varianza.acumulada2<-cumsum(varianza2)
#juntando todo en un data frame
matriz.de.varianzas2<-data.frame(valores.propios2=vp2, varianza2=varianza2, varianza.acumulada2=varianza.acumulada2)

pdf(file="barsinoutliers.pdf")
barplot(matriz.de.varianzas2[,2],names.arg=1:nrow(matriz.de.varianzas2),       
    main = "varianzas",
    xlab = "componentes principales",
    ylab = "varianzas en porcentaje",
    col = "aquamarine2",
    )
dev.off()
readline("varianzas en porcentaje 2")
pdf(file="indsinoutliers.pdf")
ind.coord2<- z$x
color2 <- color[,-c(4,5,96)]
plot(ind.coord2[,1], ind.coord2[,2], pch = 19, col=color2,
xlab="PC1 - 84.42%",ylab="PC2 - 11.35%")
abline(h=0, v=0, lty = 2)
text(ind.coord2[,1], ind.coord2[,2], labels=rownames(ind.coord),
cex=0.7, pos = 3)
dev.off()
readline("individuos 2")




















