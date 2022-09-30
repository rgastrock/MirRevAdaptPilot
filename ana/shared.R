library(tidyr)
library(lattice) #for density plot tests
library(circular)
library(RColorBrewer)
library(svglite)
library(scales)
library(ez)
library(vioplot)
library(phia) #used for interactionMeans
library(emmeans)
library(afex) #used for aov_ez
library(lsr) #for cohensD

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

#below is a function that integrates the two functions above
#it does the same thing, but the one below is helpful for plotting bootstrapped means for individual data
getConfidenceInterval <- function(data, variance = var(data), conf.level = 0.95, method='t-distr', resamples=1000, FUN=mean, returndist=FALSE) {
  
  if (method %in% c('t-distr','t')) {
    
    z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = mean(data)
    sdx = sqrt(variance/length(data))
    
    return(c(xbar - z * sdx, xbar + z * sdx))
    
  }
  
  # add sample z-distribution?
  
  # for bootstrapping:
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    
    BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    mid <- .50
    
    if (returndist) {
      percentiles <- data.frame(percentile=seq(.01,.99,.01),value=quantile(BS, probs=seq(.01,.99,.01)))
      densdist <- density(BS, bw='SJ', from=min(percentiles$value), to=max(percentiles$value))  
      return(list('percentiles'=percentiles, 'density'=densdist, 'CI95'=quantile(BS, probs = c(lo,hi))))
    } else {
      return(quantile(BS, probs = c(lo,mid,hi)))
    }
    
  }
  
}

getCircularConfidenceInterval <- function(data, variance = var(data), conf.level = 0.95, method='b', resamples=1000, returndist=FALSE) {
  
  if (method %in% c('t-distr','t')) {
    
    z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = mean(data)
    sdx = sqrt(variance/length(data))
    
    return(c(xbar - z * sdx, xbar + z * sdx))
    
  }
  
  # add sample z-distribution?
  
  # for bootstrapping:
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    BS <- c()
    for(sample in 1:nrow(samplematrix)){
      submat <- samplematrix[sample,]
      submat <- as.circular(submat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      BSmean <- mean.circular(submat)
      
      BS <- c(BS, BSmean) #this would be numeric again, but we actually want to plot this
      #otherwise the values for CI would be as if they were in circle (0 to 360 degrees)
    }
    
    #BS <- as.circular(BS, type='angles', units='degrees')
    #BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    mid <- .50
    
    if (returndist) {
      percentiles <- data.frame(percentile=seq(.01,.99,.01),value=quantile(BS, probs=seq(.01,.99,.01)))
      densdist <- density(BS, bw='SJ', from=min(percentiles$value), to=max(percentiles$value))  
      return(list('percentiles'=percentiles, 'density'=densdist, 'CI95'=quantile(BS, probs = c(lo,hi))))
    } else {
      return(quantile(BS, probs = c(lo,mid,hi)))
    }
    
  }
  
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

getCtypeColourScheme <- function(conditions = c(1,2)){
  #create a list containing the colourscheme per group
  for (condition in conditions){
    colourscheme <- list()
    
    #colourscheme[['WASH0']] <- list('S'='#ff8200ff', # pure orange
    #                                     'T'='#ff82002f')    #2f gives a lighter shade of the color
    
    colourscheme[[1]] <- list('S'='#e51636ff', #vivid/york red
                              'T'='#e516362f')
    
    #colourscheme[['WASH1']] <- list('S'='#c400c4ff', #strong magenta
    #                                     'T'='#c400c42f')
    
    colourscheme[[2]] <-   list('S'='#005de4ff', #pure blue
                                'T'='#005de42f')
    
    #colourscheme[['ALIGNED']] <-   list('S'='#A9A9A9ff', #dark grey
    #                               'T'='#A9A9A92f')
    
  }
  return(colourscheme)
}

getTtypeColourScheme <- function(angles = c(15,30,45)){
  #create a list containing the colourscheme per group
  for (angle in angles){
    colourscheme <- list()
    
    colourscheme[[15]] <- list('S'='#ff8200ff', # pure orange
                               'T'='#ff82002f')    #2f gives a lighter shade of the color
    
    colourscheme[[30]] <- list('S'='#e51636ff', #vivid/york red
                               'T'='#e516362f')
    
    colourscheme[[45]] <- list('S'='#c400c4ff', #strong magenta
                                    'T'='#c400c42f')
    
    #colourscheme[[2]] <-   list('S'='#005de4ff', #pure blue
    #                            'T'='#005de42f')
    
    #colourscheme[['ALIGNED']] <-   list('S'='#A9A9A9ff', #dark grey
    #                               'T'='#A9A9A92f')
    
  }
  return(colourscheme)
}

getAlltypeColourScheme <- function(perturb = c('ROT','MIR','CMIR')){
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
    
    colourscheme[['CMIR']] <-   list('S'='#A9A9A9ff', #dark grey
                                   'T'='#A9A9A92f')
    
  }
  return(colourscheme)
}

getOnlineColourScheme <- function(groups = c('30','60')){
  #create a list containing the colourscheme per group
  for (group in groups){
    colourscheme <- list()
    
    #colourscheme[['WASH0']] <- list('S'='#ff8200ff', # pure orange
    #                                     'T'='#ff82002f')    #2f gives a lighter shade of the color
    
    colourscheme[['30']] <- list('S'='#e51636ff', #vivid/york red
                                            'T'='#e516362f')
    
    #colourscheme[['WASH1']] <- list('S'='#c400c4ff', #strong magenta
    #                                     'T'='#c400c42f')
    
    colourscheme[['60']] <-   list('S'='#005de4ff', #pure blue
                                           'T'='#005de42f')
    
    #colourscheme[['ALIGNED']] <-   list('S'='#A9A9A9ff', #dark grey
    #                               'T'='#A9A9A92f')
    
  }
  return(colourscheme)
}

getMoveThroughColourScheme <- function(moves = c('0','1')){
  #create a list containing the colourscheme per group
  for (move in moves){
    colourscheme <- list()
    
    colourscheme[['0']] <- list('S'='#ff8200ff', # pure orange
                                         'T'='#ff82002f')    #2f gives a lighter shade of the color
    
    #colourscheme[['30']] <- list('S'='#e51636ff', #vivid/york red
     #                            'T'='#e516362f')
    
    colourscheme[['1']] <- list('S'='#c400c4ff', #strong magenta
                                         'T'='#c400c42f')
    
    #colourscheme[['60']] <-   list('S'='#005de4ff', #pure blue
     #                              'T'='#005de42f')
    
    #colourscheme[['ALIGNED']] <-   list('S'='#A9A9A9ff', #dark grey
    #                               'T'='#A9A9A92f')
    
  }
  return(colourscheme)
}