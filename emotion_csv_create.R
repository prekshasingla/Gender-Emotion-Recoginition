#packages <- c('tuneR', 'seewave', 'fftw', 'caTools', 'randomForest', 'warbleR', 'mice', 'e1071', 'rpart', 'rpart-plot', 'xgboost', 'e1071')
#if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#  install.packages(setdiff(packages, rownames(installed.packages())))  
#}
library(tuneR)
library(seewave)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(warbleR)
library(mice)
library(xgboost)
library(e1071)

specan3 <- function(X, bp = c(0,22), wl = 2048, threshold = 5){

    if(class(X) == "data.frame") {if(all(c("sound.files", "selec", 
                                         "start", "end") %in% colnames(X))) 
  {
    start <- as.numeric(unlist(X$start))
    end <- as.numeric(unlist(X$end))
    sound.files <- as.character(unlist(X$sound.files))
    selec <- as.character(unlist(X$selec))
  } else stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                        "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  } else  stop("X is not a data frame")
  
  if(any(is.na(c(end, start)))) stop("NAs found in start and/or end")  
  
  if(all(class(end) != "numeric" & class(start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  if(any(end - start<0)) stop(paste("The start is higher than the end in", length(which(end - start<0)), "case(s)"))  
  
  if(any(end - start>20)) stop(paste(length(which(end - start>20)), "selection(s) longer than 20 sec"))  
  options( show.error.messages = TRUE)
  
  if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}
  
  fs <- list.files(path = getwd(), pattern = ".wav$", ignore.case = TRUE)
  if(length(unique(sound.files[(sound.files %in% fs)])) != length(unique(sound.files))) 
    cat(paste(length(unique(sound.files))-length(unique(sound.files[(sound.files %in% fs)])), 
              ".wav file(s) not found"))
  
  d <- which(sound.files %in% fs) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    start <- start[d]
    end <- end[d]
    selec <- selec[d]
    sound.files <- sound.files[d]
  }
  
  x <- as.data.frame(lapply(1:length(start), function(i) { 
    r <- tuneR::readWave(file.path(getwd(), sound.files[i]), from = start[i], to = end[i], units = "seconds") 
    
    b<- bp 
    if(b[2] > ceiling(r@samp.rate/2000) - 1) b[2] <- ceiling(r@samp.rate/2000) - 1 
    
    
    songspec <- seewave::spec(r, f = r@samp.rate, plot = FALSE)
    analysis <- seewave::specprop(songspec, f = r@samp.rate, flim = c(0, 280/1000), plot = FALSE)
    
    meanfreq <- analysis$mean/1000
    sd <- analysis$sd/1000
    median <- analysis$median/1000
    Q25 <- analysis$Q25/1000
    Q75 <- analysis$Q75/1000
    IQR <- analysis$IQR/1000
    skew <- analysis$skewness
    kurt <- analysis$kurtosis
    sp.ent <- analysis$sh
    sfm <- analysis$sfm
    mode <- analysis$mode/1000
    centroid <- analysis$cent/1000
    
    peakf <- 0
    ff <- seewave::fund(r, f = r@samp.rate, ovlp = 50, threshold = threshold, 
                        fmax = 280, ylim=c(0, 280/1000), plot = FALSE, wl = wl)[, 2]
    meanfun<-mean(ff, na.rm = T)
    minfun<-min(ff, na.rm = T)
    maxfun<-max(ff, na.rm = T)
    
    y <- seewave::dfreq(r, f = r@samp.rate, wl = wl, ylim=c(0, 280/1000), ovlp = 0, plot = F, threshold = threshold, bandpass = b * 1000, fftw = TRUE)[, 2]
    meandom <- mean(y, na.rm = TRUE)
    mindom <- min(y, na.rm = TRUE)
    maxdom <- max(y, na.rm = TRUE)
    dfrange <- (maxdom - mindom)
    duration <- (end[i] - start[i])
    
    changes <- vector()
    for(j in which(!is.na(y))){
      change <- abs(y[j] - y[j + 1])
      changes <- append(changes, change)
    }
    if(mindom==maxdom) modindx<-0 else modindx <- mean(changes, na.rm = T)/dfrange
    
    return(c(duration, meanfreq, sd, median, Q25, Q75, IQR, skew, kurt, sp.ent, sfm, mode, 
             centroid, peakf, meanfun, minfun, maxfun, meandom, mindom, maxdom, dfrange, modindx))
  }))
  
  rownames(x) <- c("duration", "meanfreq", "sd", "median", "Q25", "Q75", "IQR", "skew", "kurt", "sp.ent", 
                   "sfm","mode", "centroid", "peakf", "meanfun", "minfun", "maxfun", "meandom", "mindom", "maxdom", "dfrange", "modindx")
  x <- data.frame(sound.files, selec, as.data.frame(t(x)))
  colnames(x)[1:2] <- c("sound.files", "selec")
  rownames(x) <- c(1:nrow(x))
  
  return(x)
}

processFolder <- function(folderName) {
  
  data <- data.frame()
  
  list <- list.files(pattern = '\\.wav')
  list
  
  for (fileName in list) {
    #print('hi')
    row <- data.frame(fileName, 0, 0, 5)
    data <- rbind(data, row)
  }
  
  names(data) <- c('sound.files', 'selec', 'start', 'end')
  
  acoustics <- specan3(data)
  
  setwd('..')
  
  acoustics
}

setwd("/Users/prekshasingla/Documents/GenderVoice/angry/")
angry <- processFolder('angry')
setwd("/Users/prekshasingla/Documents/GenderVoice/neutral/")
neutral <- processFolder('neutral')
setwd("/Users/prekshasingla/Documents/GenderVoice/sad/")
sad <- processFolder('sad')
setwd("/Users/prekshasingla/Documents/GenderVoice/fear/")
fear <- processFolder('fear')


neutral$label <- 1
angry$label <- 2
sad$label <- 3
fear$label <- 4

data <- rbind(neutral, angry, sad, fear)
data$label <- factor(data$label, labels=c('neutral','angry','sad', 'fear'))

data$duration <- NULL
data$sound.files <- NULL
data$selec <- NULL
data$peakf <- NULL

data <- data[complete.cases(data),]

write.csv(data, file='emotion.csv', sep=',', row.names=F)

set.seed(777)
spl <- sample.split(data$label, 0.7)
train <- subset(data, spl == TRUE)
test <- subset(data, spl == FALSE)

genderLog <- glm(label ~ ., data=train, family='binomial')
genderCART <- rpart(label ~ ., data=train, method='class')
prp(genderCART)
genderForest <- randomForest(label ~ ., data=train)

# Accuracy: 0.50
table(train$label)
1107 / nrow(train)

# Accuracy: 0.50
table(test$label)
475 / nrow(test)

# Accuracy: 0.72
predictLog <- predict(genderLog, type='response')
table(train$label, predictLog >= 0.5)
(814 + 777) / nrow(train)

# Accuracy: 0.71
predictLog2 <- predict(genderLog, newdata=test, type='response')
table(test$label, predictLog2 >= 0.5)
(339 + 335) / nrow(test)

# Accuracy: 0.81
predictCART <- predict(genderCART)
predictCART.prob <- predictCART[,2]
table(train$label, predictCART.prob >= 0.5)
(858 + 941) / nrow(train)

# Accuracy: 0.78
predictCART2 <- predict(genderCART, newdata=test)
predictCART2.prob <- predictCART2[,2]
table(test$label, predictCART2.prob >= 0.5)

predictForest <- predict(genderForest, newdata=train)
table(train$label, predictForest)

predictForest <- predict(genderForest, newdata=test)
table(test$label, predictForest)

set.seed(777)
genderSvm <- svm(label ~ ., data=train, gamma=0.21, cost=8)

predictSvm <- predict(genderSvm, train)
table(predictSvm, train$label)

predictSvm <- predict(genderSvm, test)
table(predictSvm, test$label)
