dev.off()

## Grafica de los espectros y el cambio de rango

win.graph()
par(mfrow = c(2,2))

for(i in  1:length(rownames(spectra.df))){
  
  plot(wavenumbers,
       spectra.df[i,],
       axes = F,
       xlab = '', 
       ylab = '',
       xlim = c(4000, 400),
       ylim= c(0,0.2),
       type = 'l',
       col =cols[i]
       
  )
  par(new = T)
}

box()
axis(1)
axis(2)
title(main = 'raw spectra full range',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab ='absorbance (a.u.)')

par(new = F)

for(i in  1:length(rownames(range1))){
  
  plot(wavenumbers1,
       range1[i,],
       axes = F,
       xlab = '', 
       ylab = '',
       xlim = c(1700, 400),
       ylim= c(0,0.2),
       type = 'l',
       col =cols[i]
       
  )
  par(new = T)
}

box()
axis(1)
axis(2)
title(main = 'raw spectra - ROI',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab ='absorbance (a.u.)')

par(new = F)
for(i in  1:length(rownames(mean))){
  
  plot(wavenumbers1,
       mean[i,],
       axes = F,
       xlab = '', 
       ylab = '',
       xlim = c(1700, 400),
       ylim= c(0,0.2),
       type = 'l',
       col =cols.means[i]
       
  )
  par(new = T)
}
par(new = F)
box()
axis(1)
axis(2)
title(main = 'averaged spectra',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab ='absorbance (a.u.)')

for(i in  1:length(rownames(mean))){
  
  plot(wavenumbers1,
       corrected[i,],
       axes = F,
       xlab = '', 
       ylab = '',
       xlim = c(1700, 400),
       ylim= c(0,0.2),
       type = 'l',
       col =cols.means[i]
       
  )
  par(new = T)
}

box()
axis(1)
axis(2)
title(main = 'corrected spectra',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab ='absorbance (a.u.)')



## Promedios
tiff('./plots/means.tiff',
     width = 4320,
     height = 3240, 
     res = 800
)

for(i in  1:length(rownames(mean))){
  
  plot(wavenumbers1,
       mean[i,],
       axes = F,
       xlab = '', 
       ylab = '',
       xlim = c(1700, 400),
       ylim= c(0,0.135309),
       type = 'l',
       col =cols.means[i]
       
  )
  par(new = T)
}


axis(1, at = c(1700, 1500, 1300, 1100, 900, 700, 500))
axis(2)
title(main = '',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab ='absorbance (a.u.)')
  
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

win.graph()
tiff('./plots/VarSelection.tiff',
     width = 5000,
     height = 12960, 
     res = 1500
)

par(mfrow=c(3,1))
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
}
dev.off()

## Curvas de calibracion

win.graph()

win.graph()
tiff('./plots/CalLines.tiff',
     width = 5000,
     height = 12960, 
     res = 1500
)
par(mfrow = c(3,1))
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

## baseline correction
tiff('./plots/baseline.tiff',
     width = 4320,
     height = 3240, 
     res = 800
)
plot(spc, 
     wl.reverse = TRUE)
plot(bl, add=TRUE, col=2,wl.reverse = TRUE)
dev.off()

tiff('./plots/baseline2.tiff',
     width = 4320,
     height = 3240, 
     res = 800
)
plot(spc3,wl.reverse = TRUE)

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
  svg('./plots/CVRMSEP.svg',
      width = 8640,
      height = 4860)
  library(ggplot2)
  dp <- ggplot(RMSEPTable, aes(x=variables, y=RMSEP, fill=variables)) + 
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.1, fill='white')+
    labs(title=" ",x="# of variables selected", y = "RMSE (n = 1000, mg/kg)")
  dp + scale_fill_brewer(palette="Blues") + theme_light()
  
  dev.off()
