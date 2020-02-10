library(tidyr)
library(RColorBrewer)
library(svglite)
library(scales)

#Generic Functions----

getGroupParticipants <- function(group) {
  
  #added header=F and the next two lines because this sheet was manually generated
  all_part <- read.csv(file = "data/pilot/SELECTED/participants_files.csv", header=FALSE)
  all_part <- all_part[-1,]
  colnames(all_part) <- c('id','folder')
  #return all participant ID's for whichever group specified
  participants_grouped <- as.vector(all_part$id[which(all_part$folder == group)]) 
  return (participants_grouped)
  
}

getParticipantTaskData <- function(group, id, taskno, task) {
  
  if (id < 10){
    filepath <- sprintf('data/pilot/SELECTED/%s/p00%d/p00%d-%d-%s_selected.txt', group, id, id, taskno,task) #creates the file path relative to current directory
  } else{
    filepath <- sprintf('data/pilot/SELECTED/%s/p0%d/p0%d-%d-%s_selected.txt', group, id, id, taskno,task) #creates the file path relative to current directory
  }
  
  df <- read.table(file = filepath) #these files need headers to be added
  # count the number of columns
  CountCol <- ncol(df)
  
  
  # 19 columns, use these as headers
  if (CountCol == 19) {
    colnames(df) <- c("cursorx_cm", "cursory_cm", "homex_cm", "homey_cm", "mousex_cm", "mousey_cm",
                      "participant", "rotation", "step", "targetangle_deg", "targetx_cm", "targety_cm",
                      "time_ms", "trial", "trialselected_bool", "sampleselected_bool", "sampleinterpolated_bool",
                      "maxvelocity_idx", "unsure")
  }
  return(df)
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}

getBSConfidenceInterval <- function(data, resamples) {
  
  data <- data[which(is.finite(data))] #need is.finite due to NA values
  
  #bootstrap to 95% CI with replacement (done when normal t-distribution is not assumed)
  #so multiplies data times 1000 and replaces the values
  samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
  #apply mean function to this new matrix
  BS <- apply(samplematrix, c(1), FUN=mean) #use mean instead of median (we mainly use mean for analyses, even though median is robust to outliers)
  #95% confidence that data falls within range
  #2.5% to 97.5%, with 50% being the 'median mean', which is close to actual mean
  return(quantile(BS, probs = c(0.025, 0.50, 0.975)))
  
}

t.interval <- function(data, variance = var(data, na.rm = TRUE), conf.level = 0.95) {
  #same as getConfidenceInterval, but assumes a normal t-distribution
  
  z <- qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar <- mean(data, na.rm = TRUE)
  sdx <- sqrt(variance/length(data))
  
  return(c(xbar - z * sdx, xbar, xbar + z * sdx)) 
  
}

getColourScheme <- function(groups = c('noninstructed','instructed')){
  #create a list containing the colourscheme per group
  for (group in groups){
    colourscheme <- list()
    
    #colourscheme[['WASH0']] <- list('S'='#ff8200ff', # pure orange
     #                                     'T'='#ff82002f')    #2f gives a lighter shade of the color
    
    colourscheme[['noninstructed']] <- list('S'='#e51636ff', #vivid/york red
                                         'T'='#e516362f')
    
    #colourscheme[['WASH1']] <- list('S'='#c400c4ff', #strong magenta
     #                                     'T'='#c400c42f')
    
    colourscheme[['instructed']] <-   list('S'='#005de4ff', #pure blue
                                         'T'='#005de42f')
    
    #colourscheme[['ALIGNED']] <-   list('S'='#A9A9A9ff', #dark grey
     #                               'T'='#A9A9A92f')
    
  }
  return(colourscheme)
}

getPtypeColourScheme <- function(perturb = c('ROT','MIR')){
  #create a list containing the colourscheme per group
  for (ptype in perturb){
    colourscheme <- list()
    
    #colourscheme[['WASH0']] <- list('S'='#ff8200ff', # pure orange
    #                                     'T'='#ff82002f')    #2f gives a lighter shade of the color
    
    colourscheme[['ROT']] <- list('S'='#e51636ff', #vivid/york red
                                            'T'='#e516362f')
    
    #colourscheme[['WASH1']] <- list('S'='#c400c4ff', #strong magenta
    #                                     'T'='#c400c42f')
    
    colourscheme[['MIR']] <-   list('S'='#005de4ff', #pure blue
                                           'T'='#005de42f')
    
    #colourscheme[['ALIGNED']] <-   list('S'='#A9A9A9ff', #dark grey
    #                               'T'='#A9A9A92f')
    
  }
  return(colourscheme)
}
#Learning Rates----

getTrialReachAngleAt <- function(trialdf, location = 'maxvel') {
  
  
  # location (string) determines where the angle of thereach is determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=1)
  
  # if the trial was rejected, return empty matrix now
  if (trialdf[1,'trialselected_bool'] == 0) {
    
    return(reachangle);
    
  }
  
  # extract the relevant reach information
  X <- trialdf[trialdf$sampleselected_bool == 1,'mousex_cm']
  Y <- trialdf[trialdf$sampleselected_bool == 1,'mousey_cm']
  MV <- trialdf[trialdf$sampleselected_bool == 1,'maxvelocity_idx']
  angle <- trialdf[1,'targetangle_deg']
  
  # print(X)
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is reached
  # this assumes that people don't go back, or that there is only one movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  #reachangle[1,2] <- angle #I don't know why I have to remove this for it to work!But it's the only thing keeping this function from being generic
  
  return(reachangle)
  
}

getReachAngles <- function(df, starttrial=0, endtrial=NULL, location = 'maxvel') {
  
  trialnumbers <- c(starttrial:endtrial)
  
  #place holders for variables in data frame
  trial <- c()
  targetangle <- c()
  reachdev <- c()
  
  for (trialnumber in trialnumbers) {
    
    indices <- which(df$trial == trialnumber) #rows of current trial
    
    if (length(indices) > 0) { 
      
      trialdf <- subset(df, trial == trialnumber) #get current trial number
      targetangle <- c(targetangle, trialdf$targetangle_deg[1]%%360) #target angle in degrees (all 12 for aligned); 1 is the index to get this value
      reachdev <- c(reachdev, getTrialReachAngleAt(trialdf, location = location)) #relies on reach deviation function
      trial <- c(trial, trialnumber) #counter to keep going
      
    } else {
      #set values to NA if not greater than zero
      #this part helps to fill in missing values in data
      targetangle <- c(targetangle, NA)
      reachdev <- c(reachdev, NA)
      trial <- c(trial, trialnumber) #trial numbers would still be displayed (i.e., is not NA)
      
    }
    
    
  }
  
  #build a data frame
  angularreachdeviations <- data.frame(trial, targetangle, reachdev)
  return(angularreachdeviations)
  
}

getAlignedTrainingBiases <- function(df, location) {
  
  #trials are 0 to 47
  df <- getReachAngles(df=df, starttrial = 0, endtrial = 47, location = location) 
  #get median reachdev for each angle
  trainingBiases <- aggregate(reachdev ~ targetangle, data= df, FUN = median) 
  return(trainingBiases)
  
}

getROTParticipantLearningCurve <- function(group, id, location) {
  
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
    alltargetsbef <- c(67.5, 75, 82.5,
                       157.5, 165, 172.5,
                       247.5, 255, 262.5,
                       337.5, 345, 352.5) #should compensate for 30 degrees
    alltargetsaft <- c(7.5, 15, 22.5,
                       97.5, 105, 112.5,
                       187.5, 195, 202.5,
                       277.5, 285, 292.5) #compensate 30 degrees
    
    angles <- unique(RT$targetangle)
    #RT['compensate'] <- NA
    
    for (target in angles){
      if (target %in% alltargetsbef){
        RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
        #RT$compensate[which(RT$targetangle == target)] <- 30
      } else if (target %in% alltargetsaft){
        #multiply by negative 1 bec targets after axis will have negative values
        RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
        #RT$compensate[which(RT$targetangle == target)] <- 30
      }
    }
    
    #RT$reachdev <- ((RT$reachdev * -1)/30)*100
    
    #use below for absolute errors:
    #so we subtract rotation size (30deg) from all reach deviations
    #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
    #RT$reachdev <- RT$reachdev - 30 #if we want positive values
    return(RT)
}

getMIRParticipantLearningCurve <- function(group, id, location){
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
  for (target in angles){
    if (target %in% alltargets15bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(RT)  
}

getROTGroupLearningCurves <- function(group, maxppid, location) {
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
    ppangles <- getROTParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
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

getMIRGroupLearningCurves <- function(group, maxppid, location) { # add angle?
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
    ppangles <- getMIRParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
    # if (angle == 15){
    #   ppangles <- subset(ppangles, compensate == 15)
    #   reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    #   trial <- c(1:length(reaches)) #sets up trial column
    #   dat <- cbind(trial, reaches)
    #   #rdat <- dat$reaches
    #   
    #   if (prod(dim(dataoutput)) == 0){
    #     dataoutput <- dat
    #   } else {
    #     dataoutput <- cbind(dataoutput, reaches)
    #   }
    # } else if (angle == 30){
    #   ppangles <- subset(ppangles, compensate == 30)
    #   reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    #   trial <- c(1:length(reaches)) #sets up trial column
    #   dat <- cbind(trial, reaches)
    #   #rdat <- dat$reaches
    #   
    #   if (prod(dim(dataoutput)) == 0){
    #     dataoutput <- dat
    #   } else {
    #     dataoutput <- cbind(dataoutput, reaches)
    #   }
    # } else if (angle == 45){
    #   ppangles <- subset(ppangles, compensate == 45)
    #   reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    #   trial <- c(1:length(reaches)) #sets up trial column
    #   dat <- cbind(trial, reaches)
    #   #rdat <- dat$reaches
    #   
    #   if (prod(dim(dataoutput)) == 0){
    #     dataoutput <- dat
    #   } else {
    #     dataoutput <- cbind(dataoutput, reaches)
    #   }
    # }
    
    
    
  }
  return(dataoutput)
}

getROTGroupConfidenceInterval <- function(group, maxppid, location, type){
  #for (group in groups){
    # get the confidence intervals for each trial of each group
    data <- getROTGroupLearningCurves(group = group, maxppid = maxppid, location = location)
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
        write.csv(confidence, file='data/ROT_noninstructed_CI_learningcurve.csv', row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file='data/ROT_instructed_CI_learningcurve.csv', row.names = F)
      }
      
    }
  #}
}

getMIRGroupConfidenceInterval <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  #compangle <- c(15,30,45)
  #for (comp in compangle){
  data <- getMIRGroupLearningCurves(group = group, maxppid = maxppid, location = location) #angle = comp
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
      write.csv(confidence, file='data/MIR_noninstructed_CI_learningcurve.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_CI_learningcurve.csv', row.names = F)
    }
  }
  #}
}

plotROTLearningCurves <- function(groups = c('noninstructed', 'instructed'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3ROT_learningcurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_learningcurve.csv', group))
      
      colourscheme <- getColourScheme(groups = group)
      #take only first, last and middle columns of file
      lower <- groupconfidence[,1]
      upper <- groupconfidence[,3]
      mid <- groupconfidence[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
      
      meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  legend(70,-100,legend=c('Non-Instructed','Instructed'),
         col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotMIRLearningCurves <- function(groups = c('noninstructed', 'instructed'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3MIR_learningcurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_learningcurve.csv', group))
    
    colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  legend(70,-100,legend=c('Non-Instructed','Instructed'),
         col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotLearningCurves <- function(target='inline'){
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_learningcurve.svg', width=7, height=10, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow = c(2,1))
  
  plotROTLearningCurves()
  plotMIRLearningCurves()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

# Individual data: Learning Curves----

#first split 90 trials into sets of 6 trials each
#then in each set, plot individual data as lines

getBlockedIndividualLearningCurves <- function(group, maxppid, location, targetno, perturb){
  
  if (perturb == 'ROT'){
    data <- getROTGroupLearningCurves(group = group, maxppid = maxppid, location = location)
  } else if (perturb == 'MIR'){
    data <- getMIRGroupLearningCurves(group = group, maxppid = maxppid, location = location)
  }
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- targetno;
  ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  ndat$trial <- c(1:length(ndat$trial))
  
  #but data is in wide format, we would want it in long format
  #this requires library(tidyr)
  ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
  ndat_long$participant <- as.character(ndat_long$participant)

  participants <- unique(ndat_long$participant)
  
  
  PPindex <- 0
  
  for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
  }
    
  return(ndat_long)
}

# plotUncleanedBlockedIndLC <- function(group, maxppid, location, targetno, target='inline'){
#   
#   #but we can save plot as svg file
#   if (target=='svg') {
#     svglite(file='doc/fig/Fig2A_UncleanedBlockedIndLearningCurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
#   }
#   
#   data <- getBlockedIndividualLearningCurves(group = group, maxppid = maxppid, location = location, targetno = targetno)
#   
#   plot(NA, NA, xlim = c(0,16), ylim = c(-200,200), 
#        xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
#        main = "Individual Learning Curves by Block", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   abline(h = 0, col = 8, lty = 2)
#   axis(1, at=c(1:15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
#   axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200)) #tick marks for y axis
#   
#   
#   participants <- unique(data$participant)
#   linetypeidx <- 1
#   
#   #library(RColorBrewer)
#   #all palette available from RColorBrewer
#   #display.brewer.all()
#   #we will select the first n colors in the Set1 palette, depending on how many pp we have
#   cols<-brewer.pal(n=maxppid+1,name="Set1")
#   #cols contain the names of n different colors
#   colidx <- 1
#   
#   
#   for (pp in participants){
#     row.idx <- which(data$participant == pp)
#     lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = linetypeidx, col = cols[colidx])
#     points(data$trial[row.idx],data$reachdev[row.idx], pch = 19, col = cols[colidx])
#     
#     linetypeidx <- linetypeidx + 1
#     colidx <- colidx +1
#   }
#   
#   #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
#    #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
#      #     lty=1,bty='n',cex=1)
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
#  
# }

#from the function above, we see that one participant seems to have "anti-learned", so we remove them then try to plot a mean for all participants
plotBlockedIndLC <- function(group, maxppid, location, targetno, perturb, target='inline'){
  
  if (perturb == 'ROT'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/Fig2ROT_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualLearningCurves(group = group, maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    # data <- subset(data, participant != 'pp0')
    # data <- subset(data, participant != 'pp1')
    
    plot(NA, NA, xlim = c(0,16), ylim = c(-200,200), 
         xlab = "Block", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Learning Rate by Blocks: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = 8, lty = 2)
    axis(1, at=c(1:15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)
    #linetypeidx <- 1
    
    #library(RColorBrewer)
    #all palette available from RColorBrewer
    #display.brewer.all()
    #we will select the first n colors in the Set1 palette, depending on how many pp we have
    #cols<-brewer.pal(n=maxppid+1,name="Set1")
    #cols contain the names of n different colors
    #colidx <- 1
    
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = 'grey')
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 19, col = 'grey')
      
      #linetypeidx <- linetypeidx + 1
      #colidx <- colidx +1
    }
    
    #then create a mean for all, according to trial number
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for (block in blockno){
      row.idx <- which(data$trial == block)
      blockmean <- data$reachdev[row.idx]
      val <- mean(blockmean, na.rm = TRUE)
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- val
      } else {
        allmeans <- rbind(allmeans, val)
      }
    }
    
    lines(c(1:length(allmeans)),allmeans[,1], lwd = 2, lty = 1, col = 'red')
    points(c(1:length(allmeans)),allmeans[,1], pch = 19, col = 'red')
    
    #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
    #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
    #     lty=1,bty='n',cex=1)
  } else if (perturb == 'MIR'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/Fig2MIR_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualLearningCurves(group = group, maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    # data <- subset(data, participant != 'pp0')
    # data <- subset(data, participant != 'pp1')
    
    plot(NA, NA, xlim = c(0,16), ylim = c(-200,200), 
         xlab = "Block", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Learning Rate by Blocks: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = 8, lty = 2)
    axis(1, at=c(1:15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)
    #linetypeidx <- 1
    
    #library(RColorBrewer)
    #all palette available from RColorBrewer
    #display.brewer.all()
    #we will select the first n colors in the Set1 palette, depending on how many pp we have
    #cols<-brewer.pal(n=maxppid+1,name="Set1")
    #cols contain the names of n different colors
    #colidx <- 1
    
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = 'grey')
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 19, col = 'grey')
      
      #linetypeidx <- linetypeidx + 1
      #colidx <- colidx +1
    }
    
    #then create a mean for all, according to trial number
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for (block in blockno){
      row.idx <- which(data$trial == block)
      blockmean <- data$reachdev[row.idx]
      val <- mean(blockmean, na.rm = TRUE)
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- val
      } else {
        allmeans <- rbind(allmeans, val)
      }
    }
    
    lines(c(1:length(allmeans)),allmeans[,1], lwd = 2, lty = 1, col = 'red')
    points(c(1:length(allmeans)),allmeans[,1], pch = 19, col = 'red')
    
    #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
    #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
    #     lty=1,bty='n',cex=1)
  }
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotROTMIRLC <- function(groups = c('noninstructed', 'instructed'), noninstmax = 15, instmax = 31, location = 'maxvel', targetno = 6, target = 'inline'){
  
  #fix titles to include instructed and noninstructed in title
  #instmax and noninstmax will differ depending on maximum pp id number in data
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(2,2))
  
  for(group in groups){
    if(group == 'noninstructed'){
      plotBlockedIndLC(group=group, maxppid=noninstmax, location =location, targetno = targetno, perturb = 'ROT')
      plotBlockedIndLC(group=group, maxppid=noninstmax, location =location, targetno = targetno, perturb = 'MIR')
    } else if (group == 'instructed'){
      plotBlockedIndLC(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'ROT')
      plotBlockedIndLC(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'MIR')
    }
  }
  

  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


#Reach Aftereffects -----

#this should be the same as Learning Curves, since all aftereffects measures are in one washout block
#getting mean RAE does not make sense since the deadapted trials will bias the mean
#thus the RAE from early trials of this washout block will not be represented well
#hence, we go for the rate of deadaptation
#essentially same as Learning Curves, just going the opposite direction

getROTParticipantAftereffects <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    washoutTrials <- getParticipantTaskData(group, id, taskno = 13, task = 'washout1')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    washoutTrials <- getParticipantTaskData(group, id, taskno = 7, task = 'washout0')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 48 trials
  WT<- getReachAngles(washoutTrials, starttrial=0, endtrial=47, location = location) #washout is same length as aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    WT$reachdev[which(WT$targetangle == target)] <- WT$reachdev[which(WT$targetangle == target)] - bias
  }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #reachdeviations are all negative values, we just multiply by -1 to make percentage positive
  #we will still get some negative values because of some that go below 0%, but the direction of means if plotted will make more sense
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(WT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  #WT$reachdev <- ((WT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(WT)
}

getROTGroupAftereffects <- function(group, maxppid, location) {
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
    ppangles <- getROTParticipantAftereffects(group=group, id=participant, location = location) #for every participant, get aftereffects data
    
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

getROTGroupRAEConfidenceInterval <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getROTGroupAftereffects(group = group, maxppid = maxppid, location = location)
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
      write.csv(confidence, file='data/ROT_noninstructed_CI_aftereffects.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROT_instructed_CI_aftereffects.csv', row.names = F)
    }
    
  }
  #}
}

getMIRParticipantAftereffects <- function(group, id, location){
  #same as rotation, we look into percentage of compensation, but note that magnitude to compensate differs per target
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    washoutTrials <- getParticipantTaskData(group, id, taskno = 7, task = 'washout0')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    washoutTrials <- getParticipantTaskData(group, id, taskno = 13, task = 'washout1')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 48 trials
  WT<- getReachAngles(washoutTrials, starttrial=0, endtrial=47, location = location) #washout same as aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    WT$reachdev[which(WT$targetangle == target)] <- WT$reachdev[which(WT$targetangle == target)] - bias
    
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
  
  angles <- unique(WT$targetangle)
  WT['compensate'] <- NA
  
  #we want percentage of compensation
  #we multily by -1 so that getting positive values mean that the hand went to the correct direction
  #above 100 values would mean an overcompensation, 0 is going directly to target, negative values are undercompensation
  for (target in angles){
    if (target %in% alltargets15bef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/15)*100
      WT$compensate[which(WT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      WT$compensate[which(WT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/30)*100
      WT$compensate[which(WT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      WT$compensate[which(WT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/45)*100
      WT$compensate[which(WT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/45)*100
      WT$compensate[which(WT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(WT)  
}

getMIRGroupAftereffects <- function(group, maxppid, location) { # add angle?
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
    ppangles <- getMIRParticipantAftereffects(group=group, id=participant, location = location) #for every participant, get learning curve data
    
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

getMIRGroupRAEConfidenceInterval <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  #compangle <- c(15,30,45)
  #for (comp in compangle){
  data <- getMIRGroupAftereffects(group = group, maxppid = maxppid, location = location) #angle = comp
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
      write.csv(confidence, file='data/MIR_noninstructed_CI_aftereffects.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_CI_aftereffects.csv', row.names = F)
    }
  }
  #}
}

plotROTAftereffects <- function(groups = c('noninstructed', 'instructed'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4ROT_aftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of Deadaptation: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_aftereffects.csv', group))
    
    colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  legend(35,-100,legend=c('Non-Instructed','Instructed'),
         col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotMIRAftereffects <- function(groups = c('noninstructed', 'instructed'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4MIR_aftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of Deadaptation: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_aftereffects.csv', group))
    
    colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  legend(35,-100,legend=c('Non-Instructed','Instructed'),
         col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotAftereffects <- function(target='inline'){
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4_aftereffects.svg', width=7, height=10, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow = c(2,1))
  
  plotROTAftereffects()
  plotMIRAftereffects()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}




# plotRAE <- function(perturb = c('ROT','MIR'),target='inline') {
#   
#   
#   #but we can save plot as svg file
#   if (target=='svg') {
#     svglite(file='doc/fig/Fig4_aftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
#   }
#   
#   # create plot
#   meanGroupReaches <- list() #empty list so that it plots the means last
#   
#   #NA to create empty plot
#   # could maybe use plot.new() ?
#   plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
#        xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
#        main = "Rate of Deadaptation", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   axis(1, at = c(1, 12, 24, 36, 48)) #tick marks for x axis
#   axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
#   
#   
#   for (ptype in perturb) {
#     #read in files created by getGroupConfidenceInterval in filehandling.R
#     groupconfidence <- read.csv(file=sprintf('data/%s_CI_aftereffects.csv', ptype))
#     
#     colourscheme <- getColourScheme(perturb=ptype)
#     #take only first, last and middle columns of file
#     lower <- groupconfidence[,1]
#     upper <- groupconfidence[,3]
#     mid <- groupconfidence[,2]
#     
#     col <- colourscheme[[ptype]][['T']] #use colour scheme according to group
#     
#     #upper and lower bounds create a polygon
#     #polygon creates it from low left to low right, then up right to up left -> use rev
#     #x is just trial nnumber, y depends on values of bounds
#     polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
#     
#     meanGroupReaches[[ptype]] <- mid #use mean to fill in empty list for each group
#   }
#   
#   for (ptype in perturb) {
#     # plot mean reaches for each group
#     col <- colourscheme[[ptype]][['S']]
#     lines(meanGroupReaches[[ptype]],col=col,lty=1)
#   }
#   
#   #add legend
#   legend(38,-150,legend=c('Rotation','Mirror Reversal'),
#          col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
#          lty=1,bty='n',cex=1,lwd=2)
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
#   
# }

# Individual data: Aftereffects----

#first split 48 trials into sets of 6 trials each
#then in each set, plot individual data as lines

getBlockedIndividualAftereffects <- function(group, maxppid, location, targetno, perturb){
  
  if (perturb == 'ROT'){
    data <- getROTGroupAftereffects(group = group, maxppid = maxppid, location = location)
  } else if (perturb == 'MIR'){
    data <- getMIRGroupAftereffects(group = group, maxppid = maxppid, location = location)
  }
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- targetno;
  ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  ndat$trial <- c(1:length(ndat$trial))
  
  #but data is in wide format, we would want it in long format
  #this requires library(tidyr)
  ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
  ndat_long$participant <- as.character(ndat_long$participant)
  
  participants <- unique(ndat_long$participant)
  
  
  PPindex <- 0
  
  for (pp in participants) {
    row.idx <- which(ndat_long$participant == pp)
    ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
    
    PPindex <- PPindex + 1
  }
  
  return(ndat_long)
}

plotBlockedIndRAE <- function(group, maxppid, location, targetno, perturb, target='inline'){
  
  if (perturb == 'ROT'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/Fig5ROT_BlockedIndAftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualAftereffects(group = group, maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    
    plot(NA, NA, xlim = c(0,9), ylim = c(-200,200), 
         xlab = "Block", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Blocked Individual Aftereffects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = 8, lty = 2)
    axis(1, at=c(1:8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)
    #linetypeidx <- 1
    
    #library(RColorBrewer)
    #all palette available from RColorBrewer
    #display.brewer.all()
    #we will select the first n colors in the Set1 palette, depending on how many pp we have
    #cols<-brewer.pal(n=maxppid+1,name="Set1")
    #cols contain the names of n different colors
    #colidx <- 1
    
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = 'grey')
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 19, col = 'grey')
      
      #linetypeidx <- linetypeidx + 1
      #colidx <- colidx +1
    }
    
    #then create a mean for all, according to trial number
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for (block in blockno){
      row.idx <- which(data$trial == block)
      blockmean <- data$reachdev[row.idx]
      val <- mean(blockmean, na.rm = TRUE)
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- val
      } else {
        allmeans <- rbind(allmeans, val)
      }
    }
    
    lines(c(1:length(allmeans)),allmeans[,1], lwd = 2, lty = 1, col = 'red')
    points(c(1:length(allmeans)),allmeans[,1], pch = 19, col = 'red')
    
    #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
    #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
    #     lty=1,bty='n',cex=1)
  } else if (perturb == 'MIR'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/Fig5MIR_BlockedIndAftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualAftereffects(group = group, maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    
    plot(NA, NA, xlim = c(0,9), ylim = c(-200,200), 
         xlab = "Block", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Blocked Individual Aftereffects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = 8, lty = 2)
    axis(1, at=c(1:8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)
    #linetypeidx <- 1
    
    #library(RColorBrewer)
    #all palette available from RColorBrewer
    #display.brewer.all()
    #we will select the first n colors in the Set1 palette, depending on how many pp we have
    #cols<-brewer.pal(n=maxppid+1,name="Set1")
    #cols contain the names of n different colors
    #colidx <- 1
    
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = 'grey')
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 19, col = 'grey')
      
      #linetypeidx <- linetypeidx + 1
      #colidx <- colidx +1
    }
    
    #then create a mean for all, according to trial number
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for (block in blockno){
      row.idx <- which(data$trial == block)
      blockmean <- data$reachdev[row.idx]
      val <- mean(blockmean, na.rm = TRUE)
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- val
      } else {
        allmeans <- rbind(allmeans, val)
      }
    }
    
    lines(c(1:length(allmeans)),allmeans[,1], lwd = 2, lty = 1, col = 'red')
    points(c(1:length(allmeans)),allmeans[,1], pch = 19, col = 'red')
    
    #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
    #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
    #     lty=1,bty='n',cex=1)
  }
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotROTMIRRAE <- function(groups = c('noninstructed', 'instructed'), noninstmax = 15, instmax = 31, location = 'maxvel', targetno = 6, target = 'inline'){
  #need to indicate non instructed and instructed in title
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5_BlockedIndAftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(2,2))
  
  for(group in groups){
    if(group == 'noninstructed'){
      plotBlockedIndRAE(group=group, maxppid=noninstmax, location =location, targetno = targetno, perturb = 'ROT')
      plotBlockedIndRAE(group=group, maxppid=noninstmax, location =location, targetno = targetno, perturb = 'MIR')
    } else if (group == 'instructed'){
      plotBlockedIndRAE(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'ROT')
      plotBlockedIndRAE(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'MIR')
    }
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


#Reaction Time across Blocks -----

#movement time measures are currently found in testCorrect.R script
#code for RT will be similar, but now we only want step 3
#start of step 3 is when target signal turns to go
#end of step 3 (when it switches to step 4) is when cursor distance from home is greater than radius (which is 0.5 cm * pixpercm or 35)
#so essentially, RT is difference between time that go signal occurs and time when cursor is half a centimeter away from home position

getRTTrials <- function(group, id, task, taskno){
  #allows for this function to work with each file
  #specify pp id, the task type, and task number
  #note that task type and taskno have to match, depending on present csv files
  #Note to change filepath once data folder is arranged
  # if (id < 10){
  #   dat <- read.csv(file = sprintf('data/pilot/SELECTED/%s/p00%d/p00%d-%d-%s.csv', group, id, id, taskno,task))
  # } else{
  #   dat <- read.csv(file = sprintf('data/pilot/SELECTED/%s/p0%d/p0%d-%d-%s.csv', group, id, id, taskno,task))
  # }
  
  dat <- getParticipantTaskData(group = group, id = id, taskno = taskno, task = task)
  
  #only steps 6 and 7 will have 0 or 1 in trial_column
  #ndat <- dat[dat$step == 3, ]
  
  trials <- unique(dat$trial) #need to be dat, not ndat because not all participants have step 3 recorded on every trial
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- dat[dat$trial == trialno,]
    subndat <- subndat[subndat$step == 3,]
    
    if (nrow(subndat)==0){
      reactiontime <- NA #will assign NaN if step 3 does not occur
      trial <- trialno
      
    } else{
      firststep3 <- subndat[1,]
      laststep3 <- subndat[nrow(subndat),]
      
      step3start <- firststep3$time_ms
      step3end <- laststep3$time_ms
      
      reactiontime <- step3end - step3start
      trial <- trialno
      
    }
    feedback <- c(trial, reactiontime, task)
    
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
  colnames(proportion) <- c('trial', 'reaction_time', 'task')
  proportion$participant <- id
  return(proportion)
}

getBlockedRTTrials <- function(group,id,task,taskno){
  
  data <- getRTTrials(group=group,id=id,task=task,taskno=taskno)
  reactiontime <- data.frame(as.numeric(data$reaction_time))
  
  #get mean of every 6th trial
  n <- 6
  ndat <- aggregate(reactiontime,list(rep(1:(nrow(reactiontime)%/%n+1),each=n,len=nrow(reactiontime))),mean, na.rm=TRUE)[-1]
  colnames(ndat) <- 'reaction_time'
  ndat$block <- seq(1,length(ndat$reaction_time),1)
  ndat$participant <- id
  ndat$task <- task
  
  return(ndat)
}


#but to actually do something with RT data, we could plot RT across trials separately for all MIR and ROT
#we would want all baseline, perturb, washout (aligned RTs and perturb RTs and washout)
#may have to create functions similar to Learning Curve data that does it for Aligned, Perturb, Washout


getAlignedRTData <- function(group, maxppid, task = 'aligned', taskno = 1){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  dataoutput<- data.frame()
  
  for(ppno in participants){
    dat <- getBlockedRTTrials(group=group, id=ppno, task=task, taskno=taskno)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- rbind(dataoutput, dat)
    }
  }
  #dataoutput <- subset(dataoutput, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(dataoutput)
}

getAlignedRTConfInt <- function(group, maxppid, task = 'aligned', taskno = 1, type = 't'){
  
  
  dat <- getAlignedRTData(group=group, maxppid=maxppid, task=task, taskno=taskno)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/ALIGNED_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/ALIGNED_I_CI_ReactionTime.csv', row.names = F)
  }
}

getRotationRTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Rotdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      RotRT <- getBlockedRTTrials(group=group, id=pp, taskno = 11, task = 'rotation')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      RotRT <- getBlockedRTTrials(group=group, id=pp, taskno = 5, task = 'rotation')
    }
    
    if (prod(dim(Rotdat)) == 0){
      Rotdat <- RotRT
    } else {
      Rotdat <- rbind(Rotdat, RotRT)
    }
    
  }
  #Rotdat <- subset(Rotdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Rotdat)
}

getRotationRTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getRotationRTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/ROTATION_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/ROTATION_I_CI_ReactionTime.csv', row.names = F)
  }
}

getMirrorRTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Mirdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      MirRT <- getBlockedRTTrials(group=group, id=pp, taskno = 5, task = 'mirror')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      MirRT <- getBlockedRTTrials(group=group, id=pp, taskno = 11, task = 'mirror')
    }
    
    if (prod(dim(Mirdat)) == 0){
      Mirdat <- MirRT
    } else {
      Mirdat <- rbind(Mirdat, MirRT)
    }
    
  }
  #Mirdat <- subset(Mirdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Mirdat)
}

getMirrorRTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getMirrorRTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/MIRROR_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/MIRROR_I_CI_ReactionTime.csv', row.names = F)
  }

}

getRotationWashRTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Rotdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      RotRT <- getBlockedRTTrials(group=group, id=pp, taskno = 13, task = 'washout1')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      RotRT <- getBlockedRTTrials(group=group, id=pp, taskno = 7, task = 'washout0')
    }
    
    if (prod(dim(Rotdat)) == 0){
      Rotdat <- RotRT
    } else {
      Rotdat <- rbind(Rotdat, RotRT)
    }
    
  }
  #Rotdat <- subset(Rotdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Rotdat)
}

getRotationWashRTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getRotationWashRTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/ROTATIONWASH_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/ROTATIONWASH_I_CI_ReactionTime.csv', row.names = F)
  }

}

getMirrorWashRTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Mirdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      MirRT <- getBlockedRTTrials(group=group, id=pp, taskno = 7, task = 'washout0')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      MirRT <- getBlockedRTTrials(group=group, id=pp, taskno = 13, task = 'washout1')
    }
    
    if (prod(dim(Mirdat)) == 0){
      Mirdat <- MirRT
    } else {
      Mirdat <- rbind(Mirdat, MirRT)
    }
    
  }
  #Mirdat <- subset(Mirdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Mirdat)
}

getMirrorWashRTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getMirrorWashRTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/MIRRORWASH_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/MIRRORWASH_I_CI_ReactionTime.csv', row.names = F)
  }
}

plotNIBlockedRT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6NI_reactiontime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_NI_CI_ReactionTime.csv')
  dat2 <- read.csv(file='data/ROTATION_NI_CI_ReactionTime.csv')
  dat3 <- read.csv(file='data/ROTATIONWASH_NI_CI_ReactionTime.csv')
  dat4 <- read.csv(file='data/MIRROR_NI_CI_ReactionTime.csv')
  dat5 <- read.csv(file='data/MIRRORWASH_NI_CI_ReactionTime.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #should be the same idea as points in movement time, so may need to separate each task, then plot as usual

  X1 <- seq(1, 8,1)
  X3 <- seq(9,23,1)
  X5 <- seq(24,31,1)
  X7 <- seq(32,46,1)
  X9 <- seq(47,54,1)

  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Reaction Time (ms)', main = 'Mean Reaction Time across Blocks and Tasks',
       xlim = c(0,55), ylim = c(0,1000))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,9,24,32,47,54))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 200, 300, 400,500, 600, 700, 800, 1000),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(8.5,23.5,31.5,46.5), col = 8, lty = 2)
  
  #localization
  lines(X1,Y[1:8], col = alpha("#696969ff", 1))#aligned
  lines(X3, Y[9:23], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[24:31], col = alpha("#ff8200ff", 1))#rotwashout
  lines(X7, Y[32:46], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[47:54], col = alpha("#c400c4ff", 1))#mirwashout

  polygon(x = c(X1, rev(X1)), y = c(YLow[1:8], rev(YUp[1:8])), border=NA, col="#6969692f")
  polygon(x = c(X3, rev(X3)), y = c(YLow[9:23], rev(YUp[9:23])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[24:31], rev(YUp[24:31])), border=NA, col="#ff82002f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[32:46], rev(YUp[32:46])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[47:54], rev(YUp[47:54])), border=NA, col="#c400c42f")
  
  #add legend
  legend(48,1000,legend=c('Aligned','Rotation','Washout: ROT','Mirror Reversal','Washout: MIR'),
         col=c("#696969ff", "#e51636ff", "#ff8200ff", "#005de4ff", "#c400c4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIBlockedRT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6I_reactiontime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_I_CI_ReactionTime.csv')
  dat2 <- read.csv(file='data/ROTATION_I_CI_ReactionTime.csv')
  dat3 <- read.csv(file='data/ROTATIONWASH_I_CI_ReactionTime.csv')
  dat4 <- read.csv(file='data/MIRROR_I_CI_ReactionTime.csv')
  dat5 <- read.csv(file='data/MIRRORWASH_I_CI_ReactionTime.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #should be the same idea as points in movement time, so may need to separate each task, then plot as usual
  
  X1 <- seq(1, 8,1)
  X3 <- seq(9,23,1)
  X5 <- seq(24,31,1)
  X7 <- seq(32,46,1)
  X9 <- seq(47,54,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Reaction Time (ms)', main = 'Mean Reaction Time across Blocks and Tasks',
       xlim = c(0,55), ylim = c(0,1000))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,9,24,32,47,54))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 200, 300, 400,500, 600, 700, 800, 1000),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(8.5,23.5,31.5,46.5), col = 8, lty = 2)
  
  #localization
  lines(X1,Y[1:8], col = alpha("#696969ff", 1))#aligned
  lines(X3, Y[9:23], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[24:31], col = alpha("#ff8200ff", 1))#rotwashout
  lines(X7, Y[32:46], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[47:54], col = alpha("#c400c4ff", 1))#mirwashout
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:8], rev(YUp[1:8])), border=NA, col="#6969692f")
  polygon(x = c(X3, rev(X3)), y = c(YLow[9:23], rev(YUp[9:23])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[24:31], rev(YUp[24:31])), border=NA, col="#ff82002f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[32:46], rev(YUp[32:46])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[47:54], rev(YUp[47:54])), border=NA, col="#c400c42f")
  
  #add legend
  legend(48,1000,legend=c('Aligned','Rotation','Washout: ROT','Mirror Reversal','Washout: MIR'),
         col=c("#696969ff", "#e51636ff", "#ff8200ff", "#005de4ff", "#c400c4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotBlockedRT <- function(target='inline'){
  #need to indicate non instructed and instructed in title
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6_reactiontime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(2,1))
  
  plotNIBlockedRT()
  plotIBlockedRT()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# #then we use the function above and generate it for all tasks
# getRTTasks <- function(id){
#   
#   #if pp id is odd
#   #mirror is first than rotation
#   if (id%%2 == 1){
#     
#     out1 <- getRTTrials(id = id, task = 'aligned', taskno = 1)
#     out2 <- getRTTrials(id = id, task = 'random0', taskno = 3)
#     out3 <- getRTTrials(id = id, task = 'mirror', taskno = 5)
#     out4 <- getRTTrials(id = id, task = 'washout0', taskno = 7)
#     out5 <- getRTTrials(id = id, task = 'random1', taskno = 9)
#     out6 <- getRTTrials(id = id, task = 'rotation', taskno = 11)
#     out7 <- getRTTrials(id = id, task = 'washout1', taskno = 13)
#     
#     allout <- rbind(out1,out2,out3,out4,out5,out6,out7)
#     allout$participant <- id
#     
#   } else if (id%%2 == 0){
#     #if pp id is even
#     #rotation first then mirror
#     out1 <- getRTTrials(id = id, task = 'aligned', taskno = 1)
#     out2 <- getRTTrials(id = id, task = 'random0', taskno = 3)
#     out3 <- getRTTrials(id = id, task = 'rotation', taskno = 5)
#     out4 <- getRTTrials(id = id, task = 'washout0', taskno = 7)
#     out5 <- getRTTrials(id = id, task = 'random1', taskno = 9)
#     out6 <- getRTTrials(id = id, task = 'mirror', taskno = 11)
#     out7 <- getRTTrials(id = id, task = 'washout1', taskno = 13)
#     
#     allout <- rbind(out1,out2,out3,out4,out5,out6,out7)
#     allout$participant <- id
#   }
#   return(allout)
# }
# 
# #then we can plot every RT for every trial, across all tasks
# plotParticipantRT <- function(id){
#   
#   df <- getRTTasks(id=id)
#   full_trial <- seq(1,length(df$reaction_time),1)
#   df <- cbind(df,full_trial)
#   #yupperlim <- max(as.numeric(df$movement_time))
#   
#   X1 <- seq(1, 48,1)
#   X3 <- seq(49,96,1)
#   X5 <- seq(97,186,1)
#   X7 <- seq(187,234,1)
#   X9 <- seq(235,282,1)
#   X11 <- seq(283,372,1)
#   X13 <- seq(373,420,1)
#   
#   Y <- as.numeric(df$reaction_time)
#   
#   plot(c(1:length(df$full_trial)), Y, type = 'n', axes = FALSE,
#        xlab = '', ylab = '', main = sprintf('pp %d: Reaction Time (ms) across Trials & Tasks', id),
#        xlim = c(0,420), ylim = c(0,2000))
#   
#   #localization
#   points(X1,Y[1:48], col = alpha("#e51636ff", 0.5))#aligned
#   points(X3, Y[49:96], col = alpha("#c400c4ff", 0.5))#random0
#   points(X5, Y[97:186], col = alpha("#005de4ff", 0.5))#perturb0
#   points(X7, Y[187:234], col = alpha("#e51636ff", 0.5))#washout0
#   points(X9, Y[235:282], col = alpha("#c400c4ff", 0.5))#random1
#   points(X11, Y[283:372], col = alpha("#005de4ff", 0.5))#perturb1
#   points(X13, Y[373:420], col = alpha("#e51636ff",0.5))#washout1
#   
#   if (id%%2 == 1){
#     
#     labs <- c('1:AL','49:RDM0','97:MIR','187:WASH0','235:RDM1','283:ROT','373:WASH1','420')
#     
#   } else if (id%%2 == 0){
#     #if pp id is even
#     #rotation first then mirror
#     labs <- c('1:AL','49:RDM0','97:ROT','187:WASH0','235:RDM1','283:MIR','373:WASH1','420')
#   }
#   
#   
#   axis(side=1, at=c(1,49,97,187,235,283,373,420), labels=labs)
#   #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
#   axis(side=2, at=c(0,400,700,1000,2000),las=2)
#   
#   abline(h = c(400,700), col = 'black', lty = 2)
#   abline(v = c(49,97,187,235,283,373), col = 8, lty = 1)
#   
# }
# 
# plotAllRT <- function(maxppno, target='inline'){
#   
#   if (target=='svg') {
#     svglite(file='doc/fig/Fig1_MT.svg', width=18, height=10, pointsize=14, system_fonts=list(sans="Arial"))
#   }
#   
#   ppno <- seq(0,maxppno,1)
#   par(mfrow=c(6,2),mar=c(3,3,3,3))
#   
#   for (pp in ppno){
#     plotParticipantRT(id=pp)
#   }
#   #mtext('Movement Time (ms)', side=2, outer=TRUE, line=-1, cex=1)
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
# }

#Plots for Presentation-----

plotPTypeLearningCurves <- function(perturb = c('ROT', 'MIR'), group = 'noninstructed', target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig02_NI_learningcurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(ptype in perturb){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_%s_CI_learningcurve.csv', ptype, group))
    
    colourscheme <- getPtypeColourScheme(ptype)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[ptype]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[ptype]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (ptype in perturb) {
    # plot mean reaches for each group
    col <- colourscheme[[ptype]][['S']]
    lines(meanGroupReaches[[ptype]],col=col,lty=1)
  }
  
  #add legend
  legend(70,-150,legend=c('Rotation','Mirror Reversal'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotLC <- function(group = 'noninstructed', instmax = 15, location = 'maxvel', targetno = 6, target = 'inline'){
  
  #instmax and noninstmax will differ depending on maximum pp id number in data
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig01_NI_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(1,2))
  
  
  plotBlockedIndLC(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'ROT')
  plotBlockedIndLC(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'MIR')
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotPTypeAftereffects <- function(perturb = c('ROT', 'MIR'), group = 'noninstructed', target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig03_NI_aftereffects.svg', width=8, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of Deadaptation", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(ptype in perturb){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_%s_CI_aftereffects.csv', ptype, group))
    
    colourscheme <- getPtypeColourScheme(ptype)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[ptype]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[ptype]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (ptype in perturb) {
    # plot mean reaches for each group
    col <- colourscheme[[ptype]][['S']]
    lines(meanGroupReaches[[ptype]],col=col,lty=1)
  }
  
  #add legend
  legend(35,-100,legend=c('Rotation','Mirror Reversal'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotAE <- function(group = 'noninstructed', instmax = 15, location = 'maxvel', targetno = 6, target = 'inline'){
  
  #instmax and noninstmax will differ depending on maximum pp id number in data
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig04_NI_BlockedIndAftereffects.svg', width=9, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(1,2))
  
  
  plotBlockedIndRAE(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'ROT')
  plotBlockedIndRAE(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'MIR')
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotRT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig05_NI_reactiontime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_NI_CI_ReactionTime.csv')
  dat2 <- read.csv(file='data/ROTATION_NI_CI_ReactionTime.csv')
  dat3 <- read.csv(file='data/ROTATIONWASH_NI_CI_ReactionTime.csv')
  dat4 <- read.csv(file='data/MIRROR_NI_CI_ReactionTime.csv')
  dat5 <- read.csv(file='data/MIRRORWASH_NI_CI_ReactionTime.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #should be the same idea as points in movement time, so may need to separate each task, then plot as usual
  
  X1 <- seq(1, 8,1)
  X3 <- seq(9,23,1)
  X5 <- seq(24,31,1)
  X7 <- seq(32,46,1)
  X9 <- seq(47,54,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Reaction Time (ms)', main = 'Mean Reaction Time across Blocks and Tasks',
       xlim = c(0,55), ylim = c(0,1000))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,9,24,32,47,54))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 200, 300, 400,500, 600, 700, 800, 1000),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(8.5,23.5,31.5,46.5), col = 8, lty = 2)
  
  #localization
  lines(X1,Y[1:8], col = alpha("#696969ff", 1))#aligned
  lines(X3, Y[9:23], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[24:31], col = alpha("#ff8200ff", 1))#rotwashout
  lines(X7, Y[32:46], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[47:54], col = alpha("#c400c4ff", 1))#mirwashout
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:8], rev(YUp[1:8])), border=NA, col="#6969692f")
  polygon(x = c(X3, rev(X3)), y = c(YLow[9:23], rev(YUp[9:23])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[24:31], rev(YUp[24:31])), border=NA, col="#ff82002f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[32:46], rev(YUp[32:46])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[47:54], rev(YUp[47:54])), border=NA, col="#c400c42f")
  
  #add legend
  legend(48,1000,legend=c('Aligned','Rotation','Washout: ROT','Mirror Reversal','Washout: MIR'),
         col=c("#696969ff", "#e51636ff", "#ff8200ff", "#005de4ff", "#c400c4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


# KINE 4225 Analysis----

getALROTParticipantLearningCurve <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  

  #biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  AT<- getReachAngles(alignedTraining, starttrial=0, endtrial=47, location = location) #rotated is 90 trials; appended to end of aligned
  
  # for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
  #   
  #   target<- biases[biasno, 'targetangle'] #get corresponding target angle
  #   bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
  #   
  #   #subtract bias from reach deviation for rotated session only
  #   RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  # }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #note that rotation direction is counterbalanced (CCW and CW)
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(AT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      AT$reachdev[which(AT$targetangle == target)] <- ((AT$reachdev[which(AT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      AT$reachdev[which(AT$targetangle == target)] <- (((AT$reachdev[which(AT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  return(AT)
}

getALROTGroupLearningCurves <- function(group, maxppid = 15, location) {
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
    ppangles <- getALROTParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(0, 48)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getAlignedTrialByTrial <- function(group = 'noninstructed', maxppid = 15, location = 'maxvel'){
  
  dat <- getALROTGroupLearningCurves(group=group, maxppid=maxppid, location=location)
  
  for (trialno in dat$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    
    dat[trialno, 3:ncol(dat)] <- ndat
    dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                    "p004", "p005", "p006", "p007", "p008", "p009",
                    "p010", "p011", "p012", "p013", "p014",
                    "p015", "average", "sd")
  write.csv(dat, file='data/K4225/aligned_trial.csv', row.names = F)
}

getAlignedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/aligned_trial.csv')
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];

  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }

  block <- c(1:nrow(ndat))
  rot <- rep(0,nrow(ndat))
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/aligned_block.csv', row.names = F)
}

#now do it for ROTATED

getKINEROTParticipantLearningCurve <- function(group, id, location) {
  
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
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(RT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  #RT$reachdev <- ((RT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(RT)
}

getKINEROTGroupLearningCurves <- function(group, maxppid, location) {
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
    ppangles <- getKINEROTParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(30, 90)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

#include RAE data in as well
getKINEROTParticipantAftereffects <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    washoutTrials <- getParticipantTaskData(group, id, taskno = 13, task = 'washout1')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    washoutTrials <- getParticipantTaskData(group, id, taskno = 7, task = 'washout0')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 48 trials
  WT<- getReachAngles(washoutTrials, starttrial=0, endtrial=47, location = location) #washout is same length as aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    WT$reachdev[which(WT$targetangle == target)] <- WT$reachdev[which(WT$targetangle == target)] - bias
  }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #reachdeviations are all negative values, we just multiply by -1 to make percentage positive
  #we will still get some negative values because of some that go below 0%, but the direction of means if plotted will make more sense
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(WT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  #WT$reachdev <- ((WT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(WT)
}

getKINEROTGroupAftereffects <- function(group, maxppid, location) {
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
    ppangles <- getKINEROTParticipantAftereffects(group=group, id=participant, location = location) #for every participant, get aftereffects data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(0,48)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getRotatedTrialByTrial <- function(group = 'noninstructed', maxppid = 15, location = 'maxvel'){
  
  dat <- getKINEROTGroupLearningCurves(group=group, maxppid=maxppid, location=location)
  dat <- as.data.frame(dat)
  
  for (trialno in dat$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    
    dat[trialno, 3:ncol(dat)] <- ndat
    dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  #return(dat)
  
  #include aftereffects (append them by rows)
  dat2 <- getKINEROTGroupAftereffects(group=group, maxppid=maxppid, location=location)
  dat2 <- as.data.frame(dat2)
  
  for (trialno in dat2$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat2 <- as.numeric(dat2[trialno, 3:ncol(dat2)])
    trialmu <- mean(ndat2, na.rm = TRUE)
    trialsigma <- sd(ndat2, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat2[which(abs(ndat2) > trialclip)] <- NA
    
    dat2[trialno, 3:ncol(dat2)] <- ndat2
    dat2$average[trialno] <- mean(as.numeric(dat2[trialno, 3:18]), na.rm = TRUE)
    dat2$sd[trialno] <- sd(as.numeric(dat2[trialno, 3:18]), na.rm = TRUE)
  }
  alldat <- rbind(dat,dat2)
  colnames(alldat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(alldat, file='data/K4225/rotated_trial.csv', row.names = F)
}

getRotatedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/rotated_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rotA <- rep(30,15)
  rotB <- rep(0,8)
  rot <- c(rotA, rotB)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/rotated_block.csv', row.names = F)
}

#do it for MIRROR
#note that we use 30 degrees here, in order to simplify it and take the average of the three possible target locations: 7.5, 15, 22.5

getKINEMIRParticipantLearningCurve <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
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
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #note that rotation direction is counterbalanced (CCW and CW)
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(RT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  #RT$reachdev <- ((RT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(RT)
}

getKINEMIRGroupLearningCurves <- function(group, maxppid, location) {
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
    ppangles <- getKINEMIRParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(30, 90)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getKINEMIRParticipantAftereffects <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    washoutTrials <- getParticipantTaskData(group, id, taskno = 7, task = 'washout0')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    washoutTrials <- getParticipantTaskData(group, id, taskno = 13, task = 'washout1')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 48 trials
  WT<- getReachAngles(washoutTrials, starttrial=0, endtrial=47, location = location) #washout is same length as aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    WT$reachdev[which(WT$targetangle == target)] <- WT$reachdev[which(WT$targetangle == target)] - bias
  }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #reachdeviations are all negative values, we just multiply by -1 to make percentage positive
  #we will still get some negative values because of some that go below 0%, but the direction of means if plotted will make more sense
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(WT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  #WT$reachdev <- ((WT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(WT)
}

getKINEMIRGroupAftereffects <- function(group, maxppid, location) {
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
    ppangles <- getKINEMIRParticipantAftereffects(group=group, id=participant, location = location) #for every participant, get aftereffects data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(0,48)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getMirroredTrialByTrial <- function(group = 'noninstructed', maxppid = 15, location = 'maxvel'){
  
  dat <- getKINEMIRGroupLearningCurves(group=group, maxppid=maxppid, location=location)
  dat <- as.data.frame(dat)
  
  for (trialno in dat$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    
    dat[trialno, 3:ncol(dat)] <- ndat
    dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  #return(dat)
  
  #include aftereffects (append them by rows)
  dat2 <- getKINEMIRGroupAftereffects(group=group, maxppid=maxppid, location=location)
  dat2 <- as.data.frame(dat2)
  
  for (trialno in dat2$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat2 <- as.numeric(dat2[trialno, 3:ncol(dat2)])
    trialmu <- mean(ndat2, na.rm = TRUE)
    trialsigma <- sd(ndat2, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat2[which(abs(ndat2) > trialclip)] <- NA
    
    dat2[trialno, 3:ncol(dat2)] <- ndat2
    dat2$average[trialno] <- mean(as.numeric(dat2[trialno, 3:18]), na.rm = TRUE)
    dat2$sd[trialno] <- sd(as.numeric(dat2[trialno, 3:18]), na.rm = TRUE)
  }
  alldat <- rbind(dat,dat2)
  colnames(alldat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                        "p004", "p005", "p006", "p007", "p008", "p009",
                        "p010", "p011", "p012", "p013", "p014",
                        "p015", "average", "sd")
  write.csv(alldat, file='data/K4225/mirrored_trial.csv', row.names = F)
}

getMirroredBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/mirrored_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rotA <- rep(30,15)
  rotB <- rep(0,8)
  rot <- c(rotA, rotB)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/mirrored_block.csv', row.names = F)
}

#specify RT for each individual, trial by trial
getAlignedGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    ppRT <- getRTTrials(group = group, id=participant, task = 'aligned', taskno = 1) #for every participant, get RT data
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(0, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTAlignedTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getAlignedGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat

    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_aligned_trial.csv', row.names = F)
}

#blocked data can be based off of trial by trial data created

getRTAlignedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_aligned_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,8)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_aligned_block.csv', row.names = F)
}

# Rotated RT
getRotatedGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getRTTrials(group=group, id=participant, taskno = 11, task = 'rotation')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(group=group, id=participant, taskno = 5, task = 'rotation')
    }
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(30, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTRotatedTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getRotatedGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_rotated_trial.csv', row.names = F)
}

getRTRotatedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_rotated_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(30,15)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_rotated_block.csv', row.names = F)
}

# Mirrored RT
getMirroredGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getRTTrials(group=group, id=participant, taskno = 5, task = 'mirror')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(group=group, id=participant, taskno = 11, task = 'mirror')
    }
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(30, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTMirroredTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMirroredGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_mirrored_trial.csv', row.names = F)
}

getRTMirroredBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_mirrored_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(30,15)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_mirrored_block.csv', row.names = F)
}

#Washout Rotated RT
getRotatedWashGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getRTTrials(group=group, id=participant, taskno = 13, task = 'washout1')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(group=group, id=participant, taskno = 7, task = 'washout0')
    }
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(0, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTRotatedWashTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getRotatedWashGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_rotwash_trial.csv', row.names = F)
}

getRTRotatedWashBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_rotwash_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,8)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_rotwash_block.csv', row.names = F)
}

#Washout Mirror RT
getMirroredWashGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getRTTrials(group=group, id=participant, taskno = 7, task = 'washout0')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(group=group, id=participant, taskno = 13, task = 'washout1')
    }
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(0, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTMirroredWashTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMirroredWashGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_mirwash_trial.csv', row.names = F)
}

getRTMirroredWashBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_mirwash_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,8)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_mirwash_block.csv', row.names = F)
}