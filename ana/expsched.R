source('ana/shared.R')
source('ana/learningRates.R')

getTrialList <- function(){
  
  
  #Aligned Session
  #Create data frame containing values
  trial <- c(1:420)
  
  #1 means aligned cursor
  #2 means random
  #3 means perturb
  #4 means washout
  
  align <- matrix(rep(1,48), nrow = 48, ncol = 1)
  random <- matrix(rep(2,48), nrow = 48, ncol = 1)
  perturb <- matrix(rep(3,90), nrow = 90, ncol = 1)
  washout <- matrix(rep(4,48), nrow = 48, ncol = 1)
  
  
  task <- rbind(align, random, perturb, washout,
                random, perturb, washout)
  sched_df<- data.frame(trial, task)
  return(sched_df)
  
}

plotExpSched <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig1_expsched.svg', width=8.5, height=4, pointsize=14, system_fonts=list(sans="Arial"))#width is 8.5, height is 4, pointsize is 14
  }

  #might need to specify different X and Y for each trial
  sched_df <- getTrialList()
  
  X1 <- c(1,48)
  X2 <- c(49,96)
  X3 <- c(97,186)
  X4 <- c(187,234)
  X5 <- c(235,282)
  X6 <- c(283,372)
  X7 <- c(373,420)
  
  Y <- c(0, 1)
  Y1<- c(0, .5)
  Y2<- c(.5, 1)
  
  plot(c(1:length(sched_df$trial)), seq (0,30, length.out = length(sched_df$trial)), type = 'n', axes = FALSE,
       xlab = 'Trial', ylab = '',
       xlim = c(0,421), ylim = c(-0.2,2.5))#, cex.main=.65, cex.lab=.65)
  
  #set variables for colours
  alignedcol <- '#b4b4b4'
  randcol    <- '#dadada'
  rotcol     <- '#e51636ff'
  mircol     <- '#005de4ff'
  
  #specify rects
  rect(X1[1], Y[1], X1[2], Y[2], border = alignedcol, col = alignedcol)
  
  rect(X2[1], Y[1], X2[2], Y[2], border = randcol, col = randcol)
  rect(X3[1], Y1[1], X3[2], Y1[2], border = mircol, col = mircol)
  rect(X3[1], Y2[1], X3[2], Y2[2], border = rotcol, col = rotcol)
  rect(X4[1], Y[1], X4[2], Y[2], border = alignedcol, col = alignedcol)
  
  
  rect(X5[1], Y[1], X5[2], Y[2], border = randcol, col = randcol)
  rect(X6[1], Y1[1], X6[2], Y1[2], border = rotcol, col = rotcol)
  rect(X6[1], Y2[1], X6[2], Y2[2], border = mircol, col = mircol)
  rect(X7[1], Y[1], X7[2], Y[2], border = alignedcol, col = alignedcol)
  

  axis(side=1, at=c(1,49,97,187,235,283,373,420))#, cex.axis=.65) ## add axes back
  # axis(side=2, at=c(0.1,0.9), labels=c('0','30'))
  #Ncols <- ceiling(6/4) #6 labels, 4 rows
  
  legend(250,2.3,legend=c('Aligned cursor/ Washout', 'Random rotation', 'Rotation', 'Mirror reversal'),
         col=c(alignedcol,randcol,rotcol,mircol),
         #text.col=c("#000000","#76BA1B","#4C9A2A","#A4DE02",drkbl,lgtbl),
         lty=1,bty='n',lwd=5, cex=.8)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}