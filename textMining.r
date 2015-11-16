setwd("~/workspace")
library(jiebaR)
library(stringr)
library(plyr)
library(wordcloud)

getEmotionalType <- function(x,pwords,nwords){  
  emotionType <-numeric(0)  
  xLen <-length(x)
  emotionType[1:xLen]<- 0  
  index <- 1  
  while(index <=xLen){  
    yLen <-length(x[[index]])  
    index2 <- 1  
    while(index2<= yLen){  
      if(x[[index]][[index2]] != "") {
        if(length(pwords[pwords==x[[index]][index2]]) >= 1){  
          emotionType[index] <- emotionType[index] + 1  
        }else if(length(nwords[nwords==x[[index]][index2]]) >= 1){  
          emotionType[index] <- emotionType[index] - 1  
        }
      }
      index2<- index2 + 1  
    }  
    #获取进度  
    if(index%%100==0){  
      print(round(index/xLen,3))  
    }        
    index <-index +1  
  }  
  emotionType  
}

getEmotionalScoreForOneGame <- function(folderName,fileName) {
  cutter = worker()
  inputFileName <- paste(folderName, fileName, sep = "/")
  input <- readLines(inputFileName, encoding = "UTF-8")
  positive <- readLines("ntusd-positive.txt")
  negative <- readLines("ntusd-negative.txt")
  
  f<-readLines('chinese_stopword.txt')
  stopwords<-c(NULL)
  for(i in 1:length(f))
  {
    stopwords[i]<-f[i]
  }
  
  commentSize <- length(input);
  
  emotionRank <- numeric(0)
  emotionRank[1:commentSize] <- 0
  
  commentIndex <- 1
  
  while(commentIndex <= commentSize) {
    singleComment <- input[commentIndex]
    segWords<-segment(singleComment,cutter)
    segWords<-filter_segment(segWords,stopwords)
    segWords<-gsub("[0-9a-zA-Z]+?","",segWords)
    segWords<-str_trim(segWords)
    emotionalType <- getEmotionalType(segWords,positive,negative)
    emotionRank[commentIndex] <- sum(emotionalType)
    commentIndex <- commentIndex + 1
  }
  output <- count(emotionRank)
  positiveTable <- subset(output, output$x>0)
  positivePoint <- sum(positiveTable$x*positiveTable$freq)
  negativeTable <- subset(output, output$x<0)
  negativePoint <- abs(sum(negativeTable$x*negativeTable$freq))
  positivePercentage <- positivePoint/(positivePoint + negativePoint)
  positivePercentage <- paste0(format(positivePercentage*100, digits=4), "%")
  outputFormat <- as.data.frame(positivePercentage, folderName)
  lastIndex <- nchar(fileName);
  outputName <- substring(fileName, 1, lastIndex-4)
  outputName <- paste(outputName, ".csv", sep = "")
  outputName <- paste("output", folderName, outputName, sep = "/")
  write.csv(outputFormat, file = outputName)
}


gamePlatform <- "temp"
fileNames <- list.files(gamePlatform);
for(fileName in fileNames) {
  getEmotionalScoreForOneGame(gamePlatform,fileName) 
}