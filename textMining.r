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
      if(length(pwords[pwords==x[[index]][index2]]) >= 1){  
        emotionType[index] <- emotionType[index] + 1  
      }else if(length(nwords[nwords==x[[index]][index2]]) >= 1){  
        emotionType[index] <- emotionType[index] - 1  
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

cutter = worker()

input <- readLines("360/360巴拉拉小魔仙.txt", encoding = "UTF-8")

positive <- readLines("ntusd-positive.txt")

negative <- readLines("ntusd-negative.txt")

segWords<-segment(input,cutter)


f<-readLines('chinese_stopword.txt')
stopwords<-c(NULL)
for(i in 1:length(f))
{
  stopwords[i]<-f[i]
}

segWords<-filter_segment(segWords,stopwords)

segWords<-gsub("[0-9a-zA-Z]+?","",segWords)

segWords<-str_trim(segWords)
segWords

EmotionRank <- getEmotionalType(segWords,positive,negative)
EmotionRank
# 
# commentEmotionalRank <-list(rank=EmotionRank,comment=input)
# 
# commentEmotionalRank <-as.data.frame(commentEmotionalRank)
# 
# fix(commentEmotionalRank)
# 
# commentEmotionalRank