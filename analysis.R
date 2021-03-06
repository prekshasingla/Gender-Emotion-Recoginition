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
library(xgboost)
library(e1071)

specan3 <- function(filepath){
  
 bp<- c(0,22)
 wl<- 2048
 threshold<- 5
 start<-0
 end<-20
 currentPath <- getwd()
 fileName <- basename(filepath)
 path <- dirname(filepath)
 
 print(path)
 print(fileName)
 
 setwd(path)
 print(getwd())
 r <- tuneR::readWave(fileName ,from = start, to = end, units = "seconds")


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
 duration <- (end - start)

 changes <- vector()
 for(j in which(!is.na(y))){
  change <- abs(y[j] - y[j + 1])
  changes <- append(changes, change)
 }
if(mindom==maxdom) modindx<-0 else modindx <- mean(changes, na.rm = T)/dfrange

label<--1

x<-c(duration,meanfreq, sd, median, Q25, Q75, IQR, skew, kurt, sp.ent, sfm, mode, 
         centroid, peakf, meanfun, minfun, maxfun, meandom, mindom, maxdom, dfrange, modindx,label)

wave <<- r

names(x) <- c( "duration","meanfreq", "sd", "median", "Q25", "Q75", "IQR", "skew", "kurt", "sp.ent", 
                 "sfm","mode", "centroid", "peakf", "meanfun", "minfun", "maxfun", "meandom", "mindom", "maxdom", "dfrange", "modindx","label")

return(list(acoustics = x, wave = wave))
}


gender <- function(filepath) {
  
result <- specan3(filepath)

test1 <- result$acoustics
test1$duration <- NULL
test1$sound.files <- NULL
test1$selec <- NULL
test1$peakf <- NULL
test1 <- test1[complete.cases(test1)]

wave <- result$wave

emotion <- getEmotion(test1)

setwd("/Users/prekshasingla/Documents/GenderVoice/")

data = read.csv("voice.csv")

set.seed(777)
spl <- sample.split(data$label, 0.7)
train <- subset(data, spl == TRUE)
test <- subset(data, spl == FALSE)


genderCART <- rpart(label ~ ., data=train, method='class')
prp(genderCART)
predictCART <- predict(genderCART,test1)[,2]

set.seed(777)
genderForest <- randomForest(label ~ ., data=train)
predictForest <- predict(genderForest, newdata=test1,type = "prob")[,2]

set.seed(777)
genderSvm <- svm(label ~ ., data=train, gamma=0.21, cost=8, probability=TRUE)
newdata <- data.frame(test1)
predictSvm <- predict(genderSvm, newdata, probability=TRUE)

if(predictCART > 0.5){
  result1<-"male"
}
if(predictCART <= 0.5){
  result1<-"female"
}

if(predictForest > 0.5){
  result2<-"male"
  test1$label<-"male"
}
if(predictForest <= 0.5){
  result2<-"female"
  test1$label<-"female"
}

list(label = test1$label,cart=result1,svm=predictSvm,emotion= emotion, wave = wave)

}

getEmotion <- function(test1){
  
  setwd("/Users/prekshasingla/Documents/GenderVoice/")
  
  data = read.csv("emotion.csv")
  
  set.seed(772)
  spl <- sample.split(data$label, 0.7)
  train <- subset(data, spl == TRUE)
  test <- subset(data, spl == FALSE)
  
  genderCART <- rpart(label ~ ., data=train, method='class')
  prp(genderCART)
  predictCART <- predict(genderCART,test1)[,2]
  
  set.seed(772)
  genderForest <- randomForest(label ~ ., data=train)
  predictForest1 <- predict(genderForest, newdata=test1)
  
  set.seed(772)
  genderSvm <- svm(label ~ ., data=train, gamma=0.21, cost=8, probability=TRUE)
  newdata <- data.frame(test1)
  predictSvm <- predict(genderSvm, newdata, probability=TRUE)
  #predictForest
  return (predictForest1)
}

