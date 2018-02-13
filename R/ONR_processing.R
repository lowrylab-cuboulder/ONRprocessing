se <- function(x){
  sd(x)/sqrt(length(x))
}

#this is now really slow...
cleanData <- function(df){
  for(i in 5:ncol(df)){
    df[,i][df[,i]>40 | df[,i]<33] <- NA
    for(j in 1:nrow(df)){
      if(is.na(df[j,i]) & (j < 11 | j > nrow(df)-11)){
        df[j,i] <- mean(df[,i], na.rm = T)
      }
      if(is.na(df[j,i])){
        mean_index <- c(j-10:j+10)
        df[j,i] <- mean(df[mean_index,i], na.rm = T)
      }
    }
  }
  return(df)
}
groupData <- function(df,trmt){
  meta <- df[,1:4]
  a <- df[,-c(1:4)]
  return(data.frame(meta, 'avg' = rowMeans(a), 'se' = apply(a,1,se)))
}

##################################################################################
#Preping the master dataset(s)
# 1. import master.csv
# 2. remove NLD animals and activity data, remove eday -1, add weeks variable, add minutes variable

scrapeAct <- function(NLD_ids, cohort){
  v <- vector()
  cohort <- cohort[,-grep('act',names(cohort))]
  for (i in NLD_ids){
    v <- c(v,grep(i,names(cohort)))
  }
  CDR_trim <- cohort[,-v]
  CDR_trim <- data.frame("minutes" = 1:nrow(CDR_trim), "week" = floor((CDR_trim$eday)/7) + 1, CDR_trim)
  NLD_trim <- cohort[,v]
  NLD_trim <- data.frame("minutes" = 1:nrow(NLD_trim), "week" = floor((CDR_trim$eday)/7) + 1, 
                         cohort[,c(1,2)], NLD_trim)
  tempDFs <- list('CDR' = CDR_trim, 'NLD' = NLD_trim)
  return(tempDFs)
}

scrapeTemp <- function(NLD_ids, cohort){
  v <- vector()
  cohort <- cohort[,-grep('temp',names(cohort))]
  for (i in NLD_ids){
    v <- c(v,grep(i,names(cohort)))
  }
  CDR_trim <- cohort[,-v]
  CDR_trim <- data.frame("minutes" = 1:nrow(CDR_trim), "week" = floor((CDR_trim$eday)/7) + 1, CDR_trim)
  NLD_trim <- cohort[,v]
  NLD_trim <- data.frame("minutes" = 1:nrow(NLD_trim), "week" = floor((CDR_trim$eday)/7) + 1, 
                         cohort[,c(1,2)], NLD_trim)
  tempDFs <- list('CDR' = CDR_trim, 'NLD' = NLD_trim)
  return(tempDFs)
}

sepTrmt <- function(trmt,cohort){
  v <- vector()
  for (i in trmt$mva){
    v = c(v,grep(i,names(cohort)))
  }
  MvCDR <- cleanData(cbind(cohort[,1:4],cohort[,v]))
  v2 <- vector()
  for (i in trmt$veh){
    v2 = c(v2,grep(i,names(cohort)))
  }  
  vehCDR <- cleanData(cbind(cohort[,1:4],cohort[,v2]))
  
  #make long data
  mvLong <- melt(MvCDR, id = c('minutes','week','eday','zt'))
  mvLong$trmt <- 1
  vehLong <- melt(vehCDR, id = c('minutes','week','eday','zt'))
  vehLong$trmt <- 2
  dataLong <- rbind(mvLong, vehLong)
  
  #make averages data
  mvAvg <- groupData(MvCDR)
  mvAvg$trmt <- 1
  vehAvg <- groupData(vehCDR)
  vehAvg$trmt <- 2
  dataAvg <- rbind(mvAvg, vehAvg)
  
  data <- list('Mvac' = MvCDR,'Veh' = vehCDR, 'long' = dataLong, 'avg' = dataAvg)
  return(data)
}

plotAnimals <- function(df,trmt,week){
  dayMod = (week - 1)*7
  ggplot(df[df$trmt == trmt & df$week == week,], aes(x=minutes,y=value))+
    geom_point(aes(colour=factor(variable)))+
    geom_smooth(method = "loess", span = 0.1, aes(colour=factor(variable)))+
    geom_vline(xintercept=((1:7)+dayMod)*1440)
}

squeezeData<- function(df,bin){
  a <- df[,-c(1,2,3,4)]
  meta <- df[,c(1,2,3,4)]
  meta <- meta[bin*(1:(nrow(meta)/bin)-1),]
  v <- vector()
  b <- list()
  for(i in 1:ncol(a)){
    #lazy way to avoid NAs in the last bin
    for(j in 0:(floor(nrow(a)/bin)-1)){
      v[j] <- mean(a[,i][(1:bin)+bin*j])
    }
    b[[i]] <- v
  }
  c <- data.frame(matrix(nrow = length(b[[i]]), ncol = ncol(a)))
  for(i in 1:ncol(a)){
    c[,i] <- b[[i]]
  }
  names(c) <- names(a)
  c <- data.frame(meta,c)
  return(c)
}

plotSmooths <- function(df,weekn,trmt){
  df <- df[df$week == weekn & df$trmt == trmt,]
  spline1 <- data.frame(x = df$minutes, y = smooth.spline(df$avg, spar = 0.1)$y, method = "spar = 0.1")
  spline2 <- data.frame(x = df$minutes, y = smooth.spline(df$avg, spar = 0.2)$y, method = "spar = 0.2")
  spline3 <- data.frame(x = df$minutes, y = smooth.spline(df$avg, spar = 0.3)$y, method = "spar = 0.3")
  spline4 <- data.frame(x = df$minutes, y = smooth.spline(df$avg, spar = 0.4)$y, method = "spar = 0.4")
  spline5 <- data.frame(x = df$minutes, y = smooth.spline(df$avg, spar = 0.5)$y, method = "spar = 0.5")
  spline6 <- data.frame(x = df$minutes, y = smooth.spline(df$avg, spar = 0.6)$y, method = "spar = 0.6")
  spline7 <- data.frame(x = df$minutes, y = smooth.spline(df$avg, spar = 0.7)$y, method = "spar = 0.7")
  spline8 <- data.frame(x = df$minutes, y = smooth.spline(df$avg, spar = 0.8)$y, method = "spar = 0.8")
  spline9 <- data.frame(x = df$minutes, y = smooth.spline(df$avg, spar = 0.9)$y, method = "spar = 0.9")
  
  smoothPlot <- ggplot(rbind(spline1, spline2, spline3, spline4, spline5, spline6, spline7, spline8, spline9), aes(x,y))+
    geom_point(data = df, aes(minutes, avg), alpha = 0.15, col = 'red')+geom_line(col='blue')+facet_wrap(~method)+
    geom_vline(xintercept=((1:7)+((weekn-1)*7))*1440)
  splines <- list('spline1' = spline1, 'spline2' =spline2, 'spline3'=spline3, 'spline4'=spline4, 'spline5'=spline5, 'spline6'=spline6, 
                  'spline7' = spline7, 'spline8'=spline8, 'spline9'=spline9, 'plot'=smoothPlot)
  return(splines)
  
}

