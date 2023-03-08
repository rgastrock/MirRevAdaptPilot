source('ana/shared.R')
source('ana/learningRates.R')

#Order effects: ROT----

getROTOrderEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getROTGroupLearningCurves(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #even ID gets ROT first, then MIR; odd ID gets MIR first, then ROT
  #but participant count starts at 0, whereas dat already starts at one
  #so at this point, odd numbers should have ROT first, while even have ROT second
  pp_ROTfirst <- dat[,c(seq(1,ncol(dat),2))]
  pp_ROTsecond <- dat[,c(seq(2,ncol(dat),2))]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROTfirst <- cbind(trial, pp_ROTfirst)
  pp_ROTsecond <- cbind(trial, pp_ROTsecond)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROTfirst)
  } else if (condition == 2){
    return(pp_ROTsecond)
  }
  
}

getROTOrderEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/ROT_noninstructed_CI_ordereffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/ROT_instructed_CI_ordereffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getROTOrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/ROT_noninstructed_CI_ordereffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/ROT_instructed_CI_ordereffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

#for simplicity, I will make 2 functions that will generate order effects plots for non instructed and instructed groups separately
plotNIROTOrderEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_ROT_NI_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Order Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_ordereffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('ROT_First','ROT_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIROTOrderEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_ROT_I_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Order Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_ordereffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('ROT_First','ROT_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Order effects: MIR----

getMIROrderEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupLearningCurves(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #even ID gets ROT first, then MIR; odd ID gets MIR first, then ROT
  #but participant count starts at 0, whereas dat already starts at one
  #so at this point, odd numbers should have ROT first, while even have ROT second
  pp_MIRfirst <- dat[,c(seq(1,ncol(dat),2))]
  pp_MIRsecond <- dat[,c(seq(2,ncol(dat),2))]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_MIRfirst <- cbind(trial, pp_MIRfirst)
  pp_MIRsecond <- cbind(trial, pp_MIRsecond)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_MIRfirst)
  } else if (condition == 2){
    return(pp_MIRsecond)
  }
  
}

getMIROrderEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/MIR_noninstructed_CI_ordereffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/MIR_instructed_CI_ordereffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getMIROrderEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/MIR_noninstructed_CI_ordereffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/MIR_instructed_CI_ordereffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

#for simplicity, I will make 2 functions that will generate order effects plots for non instructed and instructed groups separately
plotNIMIROrderEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_MIR_NI_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Order Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_ordereffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('MIR_First','MIR_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIMIROrderEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_MIR_I_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Order Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_ordereffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('MIR_First','MIR_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Order Effects: Stats----

# We can just compare the two conditions per perturbation type across blocks of trials (much like learning rate analysis below)
# We are only analyzing noninstructed group for now
# Start with ROT

ROTgetLongFormat <- function(conditions=c(1,2), group){
  
  for (condition in conditions){
    if(group == 'noninstructed'){
      ROT1dat <- getROTOrderEffects(group='noninstructed',maxppid=15,location='maxvel',condition=condition)
    } else if(group == 'instructed'){
      ROT1dat <- getROTOrderEffects(group='instructed',maxppid=31,location='maxvel',condition=condition)
    }
    
    
    if(condition == 1){
      ppcols <- c('First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
      colnames(ROT1dat) <- c('trial', 'First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
    } else if(condition == 2){
      ppcols <- c('Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
      colnames(ROT1dat) <- c('trial', 'Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
    }
    ROT1dat <- as.data.frame(ROT1dat)
    
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longdata <- gather(ROT1dat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)

    write.csv(longdata, file=sprintf('data/ROT_%s_ordereffects_long_%s.csv',group, condition), row.names = F)

    
  }
}

ROTgetBlockedOrderEffectsAOV <- function(conditions = c(1,2), blockdefs, group) {
  #function reads in learningcurves_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  for (condition in conditions){  
    curves <- read.csv(sprintf('data/ROT_%s_ordereffects_long_%s.csv',group,condition), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    diffcond <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        diffcond <- c(diffcond, condition)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(diffcond,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$diffcond <- as.factor(LCaov$diffcond)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

ROTordereffectsANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- ROTgetBlockedOrderEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=block,between=diffcond,type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

ROTordereffectsBayesANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- ROTgetBlockedOrderEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #Bayes ANOVA - can use long format
  #will compare models to null (intercept) or no effect - this will be 1
  #higher than 1 will be evidence for alternative hypothesis, lower will be evidence for null hypothesis
  #compare models either if only main effects, interaction of effects
  #use lmBF function for specific models
  LC4aov$participant <- as.factor(LC4aov$participant)
  bfLC<- anovaBF(compensation ~ diffcond*block + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  bfinteraction <- bfLC[4]/bfLC[3]
  print(bfLC)
  print(bfinteraction)
}

#Then do the same for MIR
MIRgetLongFormat <- function(conditions=c(1,2), group){
  
  for (condition in conditions){
    if(group == 'noninstructed'){
      MIR1dat <- getMIROrderEffects(group='noninstructed',maxppid=15,location='maxvel',condition=condition)
    } else if(group == 'instructed'){
      MIR1dat <- getMIROrderEffects(group='instructed',maxppid=31,location='maxvel',condition=condition)
    }
    
    
    if(condition == 1){
      ppcols <- c('First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
      colnames(MIR1dat) <- c('trial', 'First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
    } else if(condition == 2){
      ppcols <- c('Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
      colnames(MIR1dat) <- c('trial', 'Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
    }
    MIR1dat <- as.data.frame(MIR1dat)
    
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longdata <- gather(MIR1dat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longdata, file=sprintf('data/MIR_%s_ordereffects_long_%s.csv',group,condition), row.names = F)
  }
}

MIRgetBlockedOrderEffectsAOV <- function(conditions = c(1,2), blockdefs, group) {
  #function reads in learningcurves_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  for (condition in conditions){  
    curves <- read.csv(sprintf('data/MIR_%s_ordereffects_long_%s.csv',group, condition), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    diffcond <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        diffcond <- c(diffcond, condition)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(diffcond,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$diffcond <- as.factor(LCaov$diffcond)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

MIRordereffectsANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- MIRgetBlockedOrderEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=block,between=diffcond,type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

MIRordereffectsBayesANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- MIRgetBlockedOrderEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #Bayes ANOVA - can use long format
  #will compare models to null (intercept) or no effect - this will be 1
  #higher than 1 will be evidence for alternative hypothesis, lower will be evidence for null hypothesis
  #compare models either if only main effects, interaction of effects
  #use lmBF function for specific models
  LC4aov$participant <- as.factor(LC4aov$participant)
  bfLC<- anovaBF(compensation ~ diffcond*block + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  bfinteraction <- bfLC[4]/bfLC[3]
  print(bfLC)
  print(bfinteraction)
}

# Target Location Effects: ROT----
getROTParticipantTargetLoc <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 11, task = 'rotation')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 5, task = 'rotation')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #note that rotation direction is counterbalanced (CCW and CW)
  # alltargetsbef <- c(67.5, 75, 82.5,
  #                    157.5, 165, 172.5,
  #                    247.5, 255, 262.5,
  #                    337.5, 345, 352.5) #should compensate for 30 degrees
  # alltargetsaft <- c(7.5, 15, 22.5,
  #                    97.5, 105, 112.5,
  #                    187.5, 195, 202.5,
  #                    277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(RT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){

      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30

      #multiply by negative 1 bec targets after axis will have negative values

      #RT$compensate[which(RT$targetangle == target)] <- 30

  }
  
  # for (target in angles){
  #   if (target %in% alltargetsbef){
  #     RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
  #     #RT$compensate[which(RT$targetangle == target)] <- 30
  #   } else if (target %in% alltargetsaft){
  #     #multiply by negative 1 bec targets after axis will have negative values
  #     RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
  #     #RT$compensate[which(RT$targetangle == target)] <- 30
  #   }
  # }
  
  #RT$reachdev <- ((RT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(RT)
}

getROTGroupTargetLoc <- function(group, maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getROTParticipantTargetLoc(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getROTTargetEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getROTGroupTargetLoc(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1 get rot after axis, 2,3 get rot before axis and so on
  #but participant count starts at 0, whereas dat already starts at one
  #so condition 1 will be those falling after axis, condition 2 will be before
  pp_ROTfirst <- dat[,c(1,2,5,6,9,10,13,14)] #targets fall after axis
  pp_ROTsecond <- dat[,c(3,4,7,8,11,12,15,16)] #targets fall before axis
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROTfirst <- cbind(trial, pp_ROTfirst)
  pp_ROTsecond <- cbind(trial, pp_ROTsecond)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROTfirst)
  } else if (condition == 2){
    return(pp_ROTsecond)
  }
  
}

getROTTargetEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/ROT_noninstructed_CI_targeteffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/ROT_instructed_CI_targeteffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getROTTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/ROT_noninstructed_CI_targeteffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/ROT_instructed_CI_targeteffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

plotNIROTTargetEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_ROT_NI_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_targeteffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('After Axis','Before Axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIROTTargetEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_ROT_I_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_targeteffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('After Axis','Before Axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Target Effect Stats: ROT----

ROTgetTargetLongFormat <- function(conditions=c(1,2), group){
  
  for (condition in conditions){
    if(group == 'noninstructed'){
      ROT1dat <- getROTTargetEffects(group='noninstructed',maxppid=15,location='maxvel',condition=condition)
    } else if(group == 'instructed'){
      ROT1dat <- getROTTargetEffects(group='instructed',maxppid=31,location='maxvel',condition=condition)
    }
    
    
    if(condition == 1){
      ppcols <- c('First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
      colnames(ROT1dat) <- c('trial', 'First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
    } else if(condition == 2){
      ppcols <- c('Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
      colnames(ROT1dat) <- c('trial', 'Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
    }
    ROT1dat <- as.data.frame(ROT1dat)
    
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longdata <- gather(ROT1dat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longdata, file=sprintf('data/ROT_%s_targeteffects_long_%s.csv',group, condition), row.names = F)
  }
}

ROTgetBlockedTargetEffectsAOV <- function(conditions = c(1,2), blockdefs, group) {
  #function reads in learningcurves_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  for (condition in conditions){  
    curves <- read.csv(sprintf('data/ROT_%s_targeteffects_long_%s.csv',group, condition), stringsAsFactors=FALSE)
    if (condition == 1){
      curves$compensation <- (curves$compensation*-1)
    }
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    diffcond <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        diffcond <- c(diffcond, condition)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(diffcond,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$diffcond <- as.factor(LCaov$diffcond)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

ROTtargeteffectsANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- ROTgetBlockedTargetEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=block,between=diffcond,type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

ROTtargeteffectsBayesANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- ROTgetBlockedTargetEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #Bayes ANOVA - can use long format
  #will compare models to null (intercept) or no effect - this will be 1
  #higher than 1 will be evidence for alternative hypothesis, lower will be evidence for null hypothesis
  #compare models either if only main effects, interaction of effects
  #use lmBF function for specific models
  LC4aov$participant <- as.factor(LC4aov$participant)
  bfLC<- anovaBF(compensation ~ diffcond*block + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  bfinteraction <- bfLC[4]/bfLC[3]
  print(bfLC)
  print(bfinteraction)
}

#Target Location Effects: MIR----
getMIRParticipantTargetLoc <- function(group, id, location){
  #same as rotation, we look into percentage of compensation, but note that magnitude to compensate differs per target
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 5, task = 'mirror')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 11, task = 'mirror')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
    
  }
  #after baseline correction, we need to assign specific targets to corresponding magnitudes to compensate
  #we have 24 possible targets, but they differ depending on which side of mirror axis they are (before or after mirror)
  #this will affect calculations later on (due to negative values)
  #so we separate them by amount of compensation, and whether they are before or after mirror axis
  alltargets15bef <- c(82.5, 172.5, 262.5, 352.5) #should compensate for 15 degrees
  alltargets15aft <- c(7.5, 97.5, 187.5, 277.5)
  alltargets30bef <- c(75, 165, 255, 345) #30 degrees
  alltargets30aft <- c(15, 105, 195, 285)
  alltargets45bef <- c(67.5, 157.5, 247.5, 337.5) #45 degrees
  alltargets45aft <- c(22.5, 112.5, 202.5, 292.5)
  
  angles <- unique(RT$targetangle)
  RT['compensate'] <- NA
  
  #we want percentage of compensation
  #we multily by -1 so that getting positive values mean that the hand went to the correct direction
  #above 100 values would mean an overcompensation, 0 is going directly to target, negative values are undercompensation
  
  #since we are testing for target effects, we can disregard multiplying by minus 1
  for (target in angles){
    if (target %in% alltargets15bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(RT)  
}

getMIRGroupTargetLoc<- function(group, maxppid, location) { # add angle?
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getMIRParticipantTargetLoc(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getMIRTargetEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupTargetLoc(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1 get mir before axis, 2,3 get mir after axis and so on
  #but participant count starts at 0, whereas dat already starts at one
  #so condition 1 will be those falling before axis, condition 2 will be after
  pp_ROTfirst <- dat[,c(1,2,5,6,9,10,13,14)] #targets fall before axis
  pp_ROTsecond<- dat[,c(3,4,7,8,11,12,15,16)] #targets fall after axis
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROTfirst <- cbind(trial, pp_ROTfirst)
  pp_ROTsecond <- cbind(trial, pp_ROTsecond)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROTfirst)
  } else if (condition == 2){
    return(pp_ROTsecond)
  }
  
}

getMIRTargetEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIRTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/MIR_noninstructed_CI_targeteffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/MIR_instructed_CI_targeteffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getMIRTargetEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/MIR_noninstructed_CI_targeteffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/MIR_instructed_CI_targeteffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

plotNIMIRTargetEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_MIR_NI_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_targeteffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Before Axis','After Axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIMIRTargetEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_MIR_I_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_targeteffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Before Axis','After Axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Target Effect Stats: MIR----

MIRgetTargetLongFormat <- function(conditions=c(1,2),group){
  
  for (condition in conditions){
    if(group == 'noninstructed'){
      MIR1dat <- getMIRTargetEffects(group='noninstructed',maxppid=15,location='maxvel',condition=condition)
    } else if(group == 'instructed'){
      MIR1dat <- getMIRTargetEffects(group='instructed',maxppid=31,location='maxvel',condition=condition)
    }
    
    
    if(condition == 1){
      ppcols <- c('First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
      colnames(MIR1dat) <- c('trial', 'First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
    } else if(condition == 2){
      ppcols <- c('Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
      colnames(MIR1dat) <- c('trial', 'Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
    }
    MIR1dat <- as.data.frame(MIR1dat)
    
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longdata <- gather(MIR1dat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longdata, file=sprintf('data/MIR_%s_targeteffects_long_%s.csv',group,condition), row.names = F)
  }
}

MIRgetBlockedTargetEffectsAOV <- function(conditions = c(1,2), blockdefs, group) {
  #function reads in learningcurves_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  for (condition in conditions){  
    curves <- read.csv(sprintf('data/MIR_%s_targeteffects_long_%s.csv',group,condition), stringsAsFactors=FALSE)
    if (condition == 2){
      curves$compensation <- (curves$compensation*-1)
    }
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    diffcond <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        diffcond <- c(diffcond, condition)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(diffcond,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$diffcond <- as.factor(LCaov$diffcond)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

MIRtargeteffectsANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- MIRgetBlockedTargetEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=block,between=diffcond,type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

MIRtargeteffectsBayesANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- MIRgetBlockedTargetEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #Bayes ANOVA - can use long format
  #will compare models to null (intercept) or no effect - this will be 1
  #higher than 1 will be evidence for alternative hypothesis, lower will be evidence for null hypothesis
  #compare models either if only main effects, interaction of effects
  #use lmBF function for specific models
  LC4aov$participant <- as.factor(LC4aov$participant)
  bfLC<- anovaBF(compensation ~ diffcond*block + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  bfinteraction <- bfLC[4]/bfLC[3]
  print(bfLC)
  print(bfinteraction)
}

#Axis Effects: ROT----

getROTAxisEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getROTGroupLearningCurves(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1,2,3 gets ROT on HOR axis, 4,5,6,7 gets ROT on VER axis
  #but participant count starts at 0, whereas dat already starts at one
  #first condition would be horizontal, second would be vertical
  pp_ROTfirst <- dat[,c(1,2,3,4,9,10,11,12)]
  pp_ROTsecond <- dat[,c(5,6,7,8,13,14,15,16)]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROTfirst <- cbind(trial, pp_ROTfirst)
  pp_ROTsecond <- cbind(trial, pp_ROTsecond)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROTfirst)
  } else if (condition == 2){
    return(pp_ROTsecond)
  }
  
}

getROTAxisEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/ROT_noninstructed_CI_axiseffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/ROT_instructed_CI_axiseffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getROTAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/ROT_noninstructed_CI_axiseffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/ROT_instructed_CI_axiseffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

#for simplicity, I will make 2 functions that will generate order effects plots for non instructed and instructed groups separately
plotNIROTAxisEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_ROT_NI_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_axiseffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Hor_axis','Ver_axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIROTAxisEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_ROT_I_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis Effects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_axiseffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Hor_axis','Ver_axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Axis effects: MIR----

getMIRAxisEffects <- function(group, maxppid, location,condition){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupLearningCurves(group=group, maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1,2,3 gets MIR on VER axis, 4,5,6,7 gets MIR on HOR axis
  #but participant count starts at 0, whereas dat already starts at one
  #first condition would be vertical, second would be horizontal
  pp_MIRfirst <- dat[,c(1,2,3,4,9,10,11,12)]
  pp_MIRsecond <- dat[,c(5,6,7,8,13,14,15,16)]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_MIRfirst <- cbind(trial, pp_MIRfirst)
  pp_MIRsecond <- cbind(trial, pp_MIRsecond)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_MIRfirst)
  } else if (condition == 2){
    return(pp_MIRsecond)
  }
  
}

getMIRAxisEffectsConfidenceInterval <- function(group, maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIRAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/MIR_noninstructed_CI_axiseffects_1.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/MIR_instructed_CI_axiseffects_1.csv', row.names = F)
      }
      
    }
  }else if (condition == 2){
    data <- getMIRAxisEffects(group = group, maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (group == 'noninstructed'){
        write.csv(confidence, file='data/MIR_noninstructed_CI_axiseffects_2.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/MIR_instructed_CI_axiseffects_2.csv', row.names = F)
      }
      
    }
  }
  
}

#for simplicity, I will make 2 functions that will generate order effects plots for non instructed and instructed groups separately
plotNIMIRAxisEffects <- function(group = 'noninstructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_MIR_NI_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_axiseffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Ver_axis','Hor_axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIMIRAxisEffects <- function(group = 'instructed', conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_MIR_I_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis Effects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_axiseffects_%d.csv', group, condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('Ver_axis','Hor_axis'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Axis Effects: Stats----

# We can just compare the two conditions per perturbation type across blocks of trials (much like learning rate analysis below)
# We are only analyzing noninstructed group for now
# Start with ROT

ROTgetAxisLongFormat <- function(conditions=c(1,2), group){
  
  for (condition in conditions){
    if(group == 'noninstructed'){
      ROT1dat <- getROTAxisEffects(group='noninstructed',maxppid=15,location='maxvel',condition=condition)
    } else if(group == 'instructed'){
      ROT1dat <- getROTAxisEffects(group='instructed',maxppid=31,location='maxvel',condition=condition)
    }
    
    
    if(condition == 1){
      ppcols <- c('First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
      colnames(ROT1dat) <- c('trial', 'First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
    } else if(condition == 2){
      ppcols <- c('Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
      colnames(ROT1dat) <- c('trial', 'Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
    }
    ROT1dat <- as.data.frame(ROT1dat)
    
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longdata <- gather(ROT1dat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longdata, file=sprintf('data/ROT_%s_axiseffects_long_%s.csv',group,condition), row.names = F)
  }
}

ROTgetBlockedAxisEffectsAOV <- function(conditions = c(1,2), blockdefs, group) {
  #function reads in learningcurves_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  for (condition in conditions){  
    curves <- read.csv(sprintf('data/ROT_%s_axiseffects_long_%s.csv',group,condition), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    diffcond <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        diffcond <- c(diffcond, condition)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(diffcond,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$diffcond <- as.factor(LCaov$diffcond)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

ROTaxiseffectsANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- ROTgetBlockedAxisEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=block,between=diffcond,type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

ROTaxiseffectsBayesANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- ROTgetBlockedAxisEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #Bayes ANOVA - can use long format
  #will compare models to null (intercept) or no effect - this will be 1
  #higher than 1 will be evidence for alternative hypothesis, lower will be evidence for null hypothesis
  #compare models either if only main effects, interaction of effects
  #use lmBF function for specific models
  LC4aov$participant <- as.factor(LC4aov$participant)
  bfLC<- anovaBF(compensation ~ diffcond*block + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  bfinteraction <- bfLC[4]/bfLC[3]
  print(bfLC)
  print(bfinteraction)
}

#Then do the same for MIR
MIRgetAxisLongFormat <- function(conditions=c(1,2), group){
  
  for (condition in conditions){
    if(group == 'noninstructed'){
      MIR1dat <- getMIRAxisEffects(group='noninstructed',maxppid=15,location='maxvel',condition=condition)
    } else if(group == 'instructed'){
      MIR1dat <- getMIRAxisEffects(group='instructed',maxppid=31,location='maxvel',condition=condition)
    }
    
    
    if(condition == 1){
      ppcols <- c('First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
      colnames(MIR1dat) <- c('trial', 'First_p1','First_p2', 'First_p3', 'First_p4', 'First_p5', 'First_p6', 'First_p7', 'First_p8')
    } else if(condition == 2){
      ppcols <- c('Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
      colnames(MIR1dat) <- c('trial', 'Second_p1','Second_p2', 'Second_p3', 'Second_p4', 'Second_p5', 'Second_p6', 'Second_p7', 'Second_p8')
    }
    MIR1dat <- as.data.frame(MIR1dat)
    
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longdata <- gather(MIR1dat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longdata, file=sprintf('data/MIR_%s_axiseffects_long_%s.csv',group,condition), row.names = F)
  }
}

MIRgetBlockedAxisEffectsAOV <- function(conditions = c(1,2), blockdefs,group) {
  #function reads in learningcurves_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  for (condition in conditions){  
    curves <- read.csv(sprintf('data/MIR_%s_axiseffects_long_%s.csv',group,condition), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    diffcond <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        diffcond <- c(diffcond, condition)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(diffcond,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$diffcond <- as.factor(LCaov$diffcond)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

MIRaxiseffectsANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- MIRgetBlockedAxisEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=block,between=diffcond,type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

MIRaxiseffectsBayesANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- MIRgetBlockedAxisEffectsAOV(blockdefs=blockdefs, group=group)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #Bayes ANOVA - can use long format
  #will compare models to null (intercept) or no effect - this will be 1
  #higher than 1 will be evidence for alternative hypothesis, lower will be evidence for null hypothesis
  #compare models either if only main effects, interaction of effects
  #use lmBF function for specific models
  LC4aov$participant <- as.factor(LC4aov$participant)
  bfLC<- anovaBF(compensation ~ diffcond*block + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  bfinteraction <- bfLC[4]/bfLC[3]
  print(bfLC)
  print(bfinteraction)
}

