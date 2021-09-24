
means1 <- mean[metadata.leaves.Si$spectra.index,]

## Promedios
tiff('./plots/means.tiff',
     width = 4320,
     height = 3240, 
     res = 800
)
win.graph()

tiff('./plots/baselineCorrection.tiff',
     width = 12960,
     height = 3240, 
     res = 800)

par(mfrow = c(1,3))
for(i in  1:length(rownames(means1))){
  
  plot(wavenumbers1,
       means1[i,],
       axes = F,
       xlab = '', 
       ylab = '',
       xlim = c(1700, 445),
       ylim= c(0,0.105),
       type = 'l',
       col =cols.means[i]
       
  )
  par(new = T)
}


axis(1, at = c(1700, 1500, 1300, 1100, 900, 700, 500))
axis(2)
box()
title(main = '',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab ='Absorbance (a.u.)')
  

plot(spc[metadata.leaves.Si$spectra.index], 
     wl.reverse = TRUE)
plot(bl[metadata.leaves.Si$spectra.index], add=TRUE, col=2,wl.reverse = TRUE)
box()

plot(spc3[metadata.leaves.Si$spectra.index],wl.reverse = TRUE)
box()
dev.off()

## Grafica comparando antes y despues de correccion de linea base

win.graph()
leavesSiSpectra <- corrected[metadata.leaves.Si$spectra.index,]
leavesSiSpectraRaw <- mean[metadata.leaves.Si$spectra.index,] 

par(mfrow = c(1,2))

for (i in 1:length(rownames(leavesSiSpectraRaw ))){
  
  plot(as.numeric(colnames(leavesSiSpectraRaw )), 
       leavesSiSpectraRaw [i,],
       xlab = '',
       ylab = '',
       axes = F,
       type = 'l',
       xlim = c(1700,400),
       ylim = c(0,0.1))
  par(new = T)
}
box()
axis(1)
axis(2)
title(main = '',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab = 'Absorbance (a.u.)')

par(new = F)

for (i in 1:length(rownames(leavesSiSpectra))){
  
  plot(as.numeric(colnames(leavesSiSpectra)), 
       leavesSiSpectra[i,],
       xlab = '',
       ylab = '',
       axes = F,
       type = 'l',
       xlim = c(1700,400),
       ylim = c(0,0.1))
  par(new = T)
}
box()
axis(1)
axis(2)
title(main = '',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab = 'Absorbance (a.u.)')


## Matriz de covarianza

win.graph()
library(colorRamps)
library(colorspace)
library(viridis)
corleaves <- cov(leavesSiSpectra)

image(as.numeric(colnames(leavesSiSpectra)),
      as.numeric(colnames(leavesSiSpectra)),
      corleaves,
      col=viridis(100),
      axes=FALSE,
      xlab=expression(paste('Wave number (cm'^'-1',')')),
      ylab=expression(paste('Wave number (cm'^'-1',')')),
      xlim=c(1700,400),
      ylim=c(1700,400))
contour(as.numeric(colnames(leavesSiSpectra)),
        as.numeric(colnames(leavesSiSpectra)),
        corleaves,
        add=TRUE,
        col="black",
        xlab="",
        ylab="",
        labcex=1.1,
        ylim=c(1700,400),
        labels=""
)
contour(as.numeric(colnames(leavesSiSpectra)),
        as.numeric(colnames(leavesSiSpectra)),
        corleaves,
        lty=0,
        labcex=1.3,
        add=TRUE,
        col="black",
        vfont=c("sans serif", "bold italic"),
        nlevels=2
)

axis(1)
axis(2)
box(main = '',
    xlab = expression(paste('Wave number (cm'^'-1',')')),
    ylab = expression(paste('Wave number (cm'^'-1',')')))


## Seleccion de variables


# tiff('./plots/VarSelectionCalLines.tiff',

#      width = 10000,
#      height = 12960, 
#      res = 1500
# )

 win.graph()


par(mfcol=c(3,2))

for(j in c(1,5,9)){
  for (i in 1:length(rownames(leavesSiSpectra))){
    plot(as.numeric(colnames(leavesSiSpectra)),
         leavesSiSpectra[i,],
         xlab = '',
         ylab = '',
         axes = F,
         type = 'l',
         xlim = c(1700,400),
         ylim = c(0,0.06),
         col = cols[i])
    par(new = T)
  }
  box()
  axis(1)
  axis(2)
  title(main = paste(as.character(c(4:15)[j]),'variables'),
        xlab = expression(paste('Wave number (cm'^'-1',')')),
        ylab = 'Absorbance (a.u.)')
  abline(v = as.numeric(colnames(leavesSiSpectra)[gen$bestsets[j,]]),
         col = 1,
         lty = 2)
  legend('topleft',
         c('4 ', '8 ', '12 ')[i],
         lty = 2, 
         col = 'black',
         cex = 1
        )
}

## Curvas de calibracion


for(i in c(1,5,9)){
  plot(predX8Var,
       listOfPredictions1[[i]],
       xlab="Actual Si (mg/kg)" ,
       ylab="Predicted Si (mg/kg)",
       pch=17,
       cex=1.2,
       col="darkorchid4",
       cex.lab=1
  )
  abline(a=0  , b=1, col=1, lty=1, lwd=2)
}


dev.off()




## Model residuals
tiff('./plots/REsiduals.tiff',
     width = 5760,
     height = 3240, 
     res = 800
)
dp1 <- ggplot(ResidualsTable, aes(x=Variables, y= Residuals, fill=Variables)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill='white')+
  labs(title="",x="# of variables selected", y = "Residuals (mg/kg)")
dp1 + scale_fill_brewer(palette="Greens") + theme_minimal()
dev.off()
## Cross validation RMSEP

library(ggplot2)
  tiff('./plots/CVRMSEP.tiff',
       width = 8640,
       height = 4860, 
       res = 1200
       )
  # svg('./plots/CVRMSEP.svg',
  #     width = 11.88,
  #     height = 6.37,
  #     pointsize = 15)
  library(ggplot2)
  dp <- ggplot(RMSEPTable, aes(x=variables, y=RMSEP, fill=variables)) + 
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.1, fill='white')+
    labs(title=" ",x="# of variables selected", y = "RMSE (n = 1000, mg/kg)")
  dp + scale_fill_brewer(palette="Blues") + theme_light()
  
  dev.off()
