# mean spectra full range plot
tiff('./plots/meansFullRange.tiff',
     width = 4320,
     height = 3240, 
     res = 800
)

# par(mfrow = c(1,3))
for(i in  1:length(rownames(spectra.df))){
  
  plot(wavenumbers,
       spectra.df[i,],
       axes = F,
       xlab = '', 
       ylab = '',
       xlim = c(4000, 445),
       ylim= c(-0.005,0.12),
       type = 'l',
       col =cols.means[i]
       
  )
  par(new = T)
}


axis(1, at =  rev(seq(500,4500,200)))
axis(2)
box()
title(main = '',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab ='Absorbance (a.u.)')

dev.off()


## mean spectra ROI
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

box()
axis(1)
axis(2)
title(main = '',
      xlab = expression(
        paste('Wave number (cm'^'-1',
              ')')
      ),
      ylab ='absorbance (a.u.)')

dev.off()

## Mean spectra ROI leaves samples with Si content

tiff('./plots/meansLeavesSi.tiff',
     width = 4320,
     height = 3240, 
     res = 800
)

means1 <- mean[metadata.leaves.Si$spectra.index,]

for(i in  1:length(rownames(means1 ))){
  
  plot(wavenumbers1,
       means1 [i,],
       axes = F,
       xlab = '', 
       ylab = '',
       xlim = c(1700, 445),
       ylim= c(-0.005,0.12),
       type = 'l',
       col =cols.means[i]
       
  )
  par(new = T)
}


axis(1, at =  rev(seq(500,1700,200)))
axis(2)
box()
title(main = '',
      xlab = expression(paste('Wave number (cm'^'-1',')')),
      ylab ='Absorbance (a.u.)')

dev.off()



# baseline correction for leaves samples with silicon content

tiff('./plots/baselineCorrection.tiff',
     width = 4320,
     height = 3240, 
     res = 800)



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
 # 
 #      width = 10000,
 #      height = 12960,
 #      res = 1500
 # )

 win.graph()


par(mfrow=c(2,3))

for(j in c(0,1,2)){
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
  title(main = '',
        xlab = expression(paste('Wave number (cm'^'-1',')')),
        ylab = 'Absorbance (a.u.)')
  abline(v = as.numeric(colnames(leavesSiSpectra)[gen$bestsets[(4*j)+1,]]),
         col = 1,
         lty = 2)
  text(1560,
       0.055,
         c('4 variables', '8 variables', '12 variables')[j+1],
         lty = 2, 
         col = 'black',
         cex = 1,
         font = 4
        )
  
}

## Curvas de calibracion


# par(mfrow = c(1,3))
for(i in c(0,1,2)){
  plot(ActualSi,
       listOfPredictions1[[(4*i)+1]],
       xlab="Actual Si (mg/kg)" ,
       ylab="Predicted Si (mg/kg)",
       pch=17,
       cex=1.2,
       col="darkorchid4",
       cex.lab=1
  )
  abline(a=0  , b=1, col=1, lty=1, lwd=2)
  text(rep(15000,3)[i+1],
       rep(62000,3)[i+1],
       c(expression(paste('R'['adj']^'2','= 0.7823')),
         expression(paste('R'['adj']^'2','= 0.9782')),
         expression(paste('R'['adj']^'2','= 0.9965')))[i+1],
       col = 'black',
       cex= 1.2,
       font = 4
  )
  text(rep(55000,3)[i+1],
       rep(5000,3)[i+1],
       c(expression(paste('p'['model'],'= 1.51e+06')),
         expression(paste('p'['model'],'= 1.14e-11')),
         expression(paste('p'['model'],'= 2.42e-12')))[i+1],
       col = 'black',
       cex= 1.2,
       font = 4
  )
par(new=F)
}


# dev.off()




## Model residuals
# tiff('./plots/REsiduals.tiff',
#      width = 5760,
#      height = 3240, 
#      res = 800
# )

win.graph()

library(ggplot2)
library(gridExtra)

dp1 <- ggplot(ResidualsTable, aes(x=Variables, y= Residuals, fill=Variables)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill='white')+
  labs(title="",x="# of variables selected", y = "Residuals (mg/kg)") +
    scale_fill_brewer(palette="Greens") + theme_minimal()


  dp <- ggplot(RMSEPTable, aes(x=variables, y=RMSEP, fill=variables)) + 
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.1, fill='white')+
    labs(title=" ",x="# of variables selected", y = "RMSE (n = 1000, mg/kg)") +
  scale_fill_brewer(palette="Blues") + theme_light()
  
grid.arrange(dp1,dp)
  
