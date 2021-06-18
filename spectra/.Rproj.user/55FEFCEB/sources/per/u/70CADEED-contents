dev.off()
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