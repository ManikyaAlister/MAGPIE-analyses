save_processed_file = function(data, file, dry_run = DRY_RUN) {
  if (! dry_run) {
    # create the directory if it does not exist
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    cat("saving to: ", file)
    write_csv(data, file=file)
  } else {
    cat("(dry_run) skipping saving to: ", file)
  }
}

# make a matrix of pairwise correlations
getPairWiseMatrix <- function(originalData,colourChoice="Topic") {
  myTibble <- originalData %>%
    select(Subject,ShortText,Belief) %>%
    mutate(Belief = as.numeric(Belief)) %>%
    pivot_wider(names_from=ShortText,values_from=Belief) %>%
    select(-Subject) %>%
    correlate(diagonal=1,method="pearson",use="pairwise.complete.obs")
  
  myNames <- myTibble$term
  myTibble$term <- NULL
  myMat <- as.matrix(myTibble)
  rownames(myMat) <- myNames
  return(myMat)
}


# we can also make a generic function that will take the matrix and
# name and create the figure
# longForm is TRUE if it's all the beliefs, FALSE if by topic
plotCluster <- function(originalData,conditionChoice,colourChoice="Topic",
                        longForm=TRUE,titleText="") {
  myMat <- getPairWiseMatrix(originalData,colourChoice)
  # get ordering
  myMatDist <- as.dist(myMat)
  h <- hclust(myMatDist, method = 'single')
  # put in correct form
  myMat <- as_tibble(myMat)
  myMat$rowname <- colnames(myMat)
  myMat$rowname <- ordered(myMat$rowname, levels = h$labels[h$order])
  myMatLong <- myMat %>%
    pivot_longer(-rowname,names_to="colname",values_to="sim") %>%
    mutate(label = if_else(is.na(sim), "", sprintf("%1.2f", sim)))
  myMatLong$colname <- ordered(myMatLong$colname, 
                               levels = h$labels[h$order])
  
  # set the colours  
  cols <- rep(NA,nrow(myMat))
  matNames <- levels(myMat$rowname)
  oneSubjOriginal <- originalData %>% 
    filter(Subject == originalData$Subject[1])
  
  if (colourChoice=="Topic") {
    if (longForm) {
      for (i in 1:nrow(myMat)) {
        cols[i] <- as.character(oneSubjOriginal$Topic[oneSubjOriginal$ShortText==
                                                        matNames[i]]) }
    } else {
      cols <- matNames
    }
    cols[cols=="AI"] <- "#0066CC"
    cols[cols=="SciTech"] <- "#99CCFF"
    cols[cols=="Trans"] <- "#AA3399"
    cols[cols=="Gender"] <- "#FF99FF"
    cols[cols=="Israel"] <- "#CC9900"
    cols[cols=="Foreign"] <- "#FFCC33"
    cols[cols=="Climate"] <- "#00AA66"
    cols[cols=="Environment"] <- "#88EE22"
    cols[cols=="Health"] <- "#CCCCCC"
    cols[cols=="Religion"] <- "#AAAAAA"
    cols[cols=="MoneyEcon"] <- "#555555"
    cols[cols=="CrimeGuns"] <- "#888888"
    cols[cols=="Fun"] <- "#9999FF"
    cols[cols=="Total"] <- "#CC0000"
  } else {
    for (i in 1:nrow(myMat)) {
      cols[i] <- as.character(oneSubjOriginal$Polarity[oneSubjOriginal$ShortText==
                                                         matNames[i]]) }
    cols[cols=="left"] <- "#3399CC"
    cols[cols=="neutral"] <- "#9999FF"
    cols[cols=="right"] <- "#CC0066"
  }
 
  # create the figure
  f <- myMatLong %>%
    ggplot(mapping=aes(x=fct_rev(rowname), y=colname,fill = sim)) +
    geom_tile() +
    coord_fixed() +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    scale_fill_distiller(palette = "PuOr", na.value = "white",
                         direction = 1, limits = c(-1, 1),
                         name = "Pearson\nCorr:") +
    labs(title = paste0(titleText,conditionChoice)) +
    theme(axis.text.x = element_text(angle = 60, hjust=1, colour=rev(cols)),
          axis.title.x = element_blank(), axis.text.y=element_text(colour=cols))
  return(f)
}


# calculates a robust max difference on the vector given
# i.e. the maximum distance between the furthest two items
# by calculating it on the data with up to nOutlier items removed
# each time, and then taking the mean of those
# this guarantees that it is not majorly affected a few outliers
# X is a numeric vector, nOutlier must be smaller than its length
getRobustMaxDiff <- function(x,nOutlier=1) {
  len <- length(x)
  maxDiff <- rep(NA,len*2)
  for (i in 1:(len*nOutlier)) {
    nRemove <- sample(x=1:nOutlier,size=1)
    temp <- sample(x,replace=FALSE,size=len-nRemove)
    maxDiff[i] <- max(diff(sort(temp)))
  }
  return(mean(maxDiff))
}


# calculates a robust SD on the vector given
# by calculating it on the data with up to nOutlier items removed
# each time, and then taking the mean of those
# this guarantees that it is not majorly affected a few outliers
# X is a numeric vector, nOutlier must be smaller than its length
getRobustSD <- function(x,nOutlier=1) {
  len <- length(x)
  sdVal <- rep(NA,len*2)
  for (i in 1:(len*nOutlier)) {
    nRemove <- sample(x=1:nOutlier,size=1)
    temp <- sample(x,replace=FALSE,size=len-nRemove)
    sdVal[i] <- sd(temp) 
  }
  return(mean(sdVal))
}


# calculates a robust max distance on the variable given
# by calculating it on the data with up to nOutlier items removed
# each time, and then taking the mean of those
# this guarantees that it is not majorly affected a few outliers
# X is a numeric vector, nOutlier must be smaller than its length
getRobustMaxDist <- function(x,nOutlier=1) {
  len <- length(x)
  maxDist <- rep(NA,len*2)
  for (i in 1:(len*nOutlier)) {
    nRemove <- sample(x=1:nOutlier,size=1)
    temp <- sample(x,replace=FALSE,size=len-nRemove)
    maxDist[i] <- max(dist(temp))
  }
  return(mean(maxDist))
}

# returns the maxDiff value you would get for the 
# vector of the same length and maxDist as this one
# but a uniform spread (i.e., least polarised)
# the range is based on the range of x
# (this is also robustly evaluated)
getUniformDiff <- function(x,nOutlier=1) {
  len <- length(x)
  unifDiff <- rep(NA,len)
  for (i in 1:len) {
    # get the distribution we're going to use to calculate the
    # bounds of the uniform one
    nRemove <- sample(x=1:3,size=1)
    firstTemp <- sample(x,replace=FALSE,size=len-nRemove)
    utLow <- min(firstTemp)
    utHigh <- max(dist(firstTemp))+utLow
    uTemp <- seq(from=utLow,to=utHigh,length.out=len)
    unifDiff[i] <- getRobustMaxDiff(uTemp,nOutlier)
  }
  return(mean(unifDiff))
}


# returns the SD value you would get for the 
# vector of the same length and maxDist as this one
# but a uniform spread (i.e., least polarised)
# the range is based on the range of x
# (this is also robustly evaluated)
getUniformSD <- function(x,nOutlier=1) {
  len <- length(x)
  unifSD <- rep(NA,len)
  for (i in 1:len) {
    # get the distribution we're going to use to calculate the
    # bounds of the uniform one
    nRemove <- sample(x=1:3,size=1)
    firstTemp <- sample(x,replace=FALSE,size=len-nRemove)
    utLow <- min(firstTemp)
    utHigh <- max(dist(firstTemp))+utLow
    uTemp <- seq(from=utLow,to=utHigh,length.out=len)
    unifSD[i] <- getRobustSD(uTemp,nOutlier)
  }
  return(mean(unifSD))
}


# returns the maxDiff value you would get for the 
# vector of the same length and maxDist as this one
# but a uniform spread (i.e., least polarised)
# xLow and xHigh indicate the max and min values in the comparison vector
getUniformDiffFull <- function(x,nOutlier=1,xLow=0,xHigh=100) {
  len <- length(x)
  utLow <- xLow
  utHigh <- xHigh
  unifDiff <- rep(NA,len)
  uTemp <- seq(from=utLow,to=utHigh,length.out=len)
  for (i in 1:len) {
    unifDiff[i] <- getRobustMaxDiff(uTemp,nOutlier)
  }
  return(mean(unifDiff))
}


# returns the SD value you would get for the 
# vector of the same length and maxDist as this one
# but a uniform spread (i.e., least polarised)
# xLow and xHigh indicate the max and min values in the comparison vector
getUniformSDFull <- function(x,nOutlier=1,xLow=0,xHigh=100) {
  len <- length(x)
  utLow <- xLow
  utHigh <- xHigh
  unifSD <- rep(NA,len)
  uTemp <- seq(from=utLow,to=utHigh,length.out=len)
  for (i in 1:len) {
    unifSD[i] <- getRobustSD(uTemp,nOutlier)
  }
  return(mean(unifSD))
}
