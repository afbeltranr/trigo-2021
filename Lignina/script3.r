m <- read.table("matrizbien.txt")
a <- read.csv("125-2.CSV", header=FALSE)
a1 <- a[,1]
write.table(a1, file="num.txt", sep=";", dec=",")
#plot(m$lambda, m[,3], type="l")
m2 <- m[,-2]
mt <- t(m2)

colnames(mt) <- as.character(unlist(mt[1,]))
mt2 <- mt[-1, ]
mt3 <- mt2 [,-1869]


#-------------------------sin---escalar---------------------------------
a <- prcomp(mt3) #pca

#valores propios
vp<-(a$sdev)^2
#varianza en porcentaje
varianza<-(vp*100)/sum(vp)
#varianza acumulada
varianza.acumulada<-cumsum(varianza)
#juntando todo en un data frame
matriz.de.varianzas<-data.frame(valores.propios=vp, varianza=varianza, varianza.acumulada=varianza.acumulada)

barplot(matriz.de.varianzas[,2],names.arg=1:nrow(matriz.de.varianzas),       
    main = "varianzas",
    xlab = "componentes principales",
    ylab = "varianzas en porcentaje",
    col = "aquamarine2",
    )
readline("varianzas en porcentaje")

ind.coord<- a$x

#pdf(file="sinescalar.pdf")

plot(ind.coord[,1], ind.coord[,2], pch = 19,
xlab="PC1 - 75.13%",ylab="PC2 - 18.43%")
abline(h=0, v=0, lty = 2)
text(ind.coord[,1], ind.coord[,2], labels=rownames(ind.coord),
cex=0.7, pos = 3)

#dev.off()

readline("individuos sin escalar")


#-------------------------escalado---------------------------------

a2 <- prcomp(mt3, scale=TRUE) #pca
#valores propios
vp2<-(a2$sdev)^2
#varianza en porcentaje
varianza2<-(vp*100)/sum(vp2)
#varianza acumulada
varianza.acumulada2<-cumsum(varianza2)
#juntando todo en un data frame
matriz.de.varianzas2<-data.frame(valores.propios2=vp2, varianza2=varianza2, varianza.acumulada2=varianza.acumulada2)

barplot(matriz.de.varianzas2[,2],names.arg=1:nrow(matriz.de.varianzas2),       
    main = "varianzas",
    xlab = "componentes principales",
    ylab = "varianzas en porcentaje",
    col = "aquamarine2",
    )
readline("varianzas en porcentaje")

ind.coord2<- a2$x

#pdf(file="sinescalar.pdf")

plot(ind.coord2[,1], ind.coord2[,2], pch = 19,
xlab="PC1 - 75.13%",ylab="PC2 - 18.43%")
abline(h=0, v=0, lty = 2)
text(ind.coord2[,1], ind.coord2[,2], labels=rownames(ind.coord2),
cex=0.7, pos = 3)

#dev.off()


readline("individuos escalado")

write.table(m2, file="m2.txt", sep=";", dec=",")
