#script to test circle where targets are located

df <- read.csv(file = 'data/p001-1-aligned.csv')

minX <- min(df$mousex_px)
maxX <- max(df$mousex_px)
Xval <- c(minX, maxX)

minY <- min(df$mousey_px)
maxY <- max(df$mousey_px)
Yval <- c(minY, maxY)


averageX <- mean(Xval)
averageY <- mean(Yval)

Xval <- c(Xval, averageX)
Yval <- c(Yval, averageY)
allval <- rbind(Xval,Yval)
colnames(allval) <- c('min_val','max_val', 'mean_val')
