library(syuzhet)
library(xlsx)
library(plyr)
setwd("C:/Users/Puneet/Documents/Sentiment Analysis Project/Data")
month<-c("Jan","Nov","Dec")

data<-data.frame(date=character(),sentiment=numeric())
dummy5<- character(0)
setwd("C:/Users/Puneet/Documents/Sentiment Analysis Project/Data")
  dum<-paste(12,".xlsx",sep="")
  Dummy<- read.xlsx(dum,1)
  dummy2<- as.character(Dummy[,2])
  dummy3<- as.character(Dummy[,1])
  dummy4<- gsub("@\\w+","",dummy2)
  dummy4<- gsub("[[:punct:]]","",dummy4)
  dummy4<- gsub("[[:digit:]]","",dummy4)
  dummy4<- gsub("http\\w+","",dummy4)
  dummy4<- gsub("[ \t]{2,}"," ",dummy4)
  dummy4<- gsub("^\\s+|\\s+$"," ",dummy4)
  Dummy$sentiment<-get_sentiment(char_v=dummy4,"nrc",dummy4) 
  
a<-VectorSource(dummy4)
jeopCorpus <- Corpus(a)
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus  <- tm_map(jeopCorpus, content_transformer(tolower))
jeopCorpus <- tm_map(jeopCorpus, removeWords, c("price", "oil","oilprice","prices"
                                                ,"drop","oilpric","crude" ,stopwords('english')))
#Eliminate extra white spaces
jeopCorpus <- tm_map(jeopCorpus, stripWhitespace)
jeopCorpus <- tm_map(jeopCorpus, stemDocument)

dtm <- TermDocumentMatrix(jeopCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#getting nrc sentiment
for(j in 1:length(files)) 
{
  setwd("C:/Users/Puneet/Documents/Sentiment Analysis Project/New")  
  Dummy<- read.xlsx(files[j],1)
  dummy2<- as.character(Dummy[,2])
  dummy3<- as.character(Dummy[,1])
  dummy4<- gsub("@\\w+","",dummy2)
  dummy4<- gsub("[[:punct:]]","",dummy4)
  dummy4<- gsub("http\\w+","",dummy4)
  dummy4<- gsub("[ \t]{2,}","",dummy4)
  dummy4<- gsub("^\\s+|\\s+$","",dummy4)
  dummy4[dummy4==""] <- NA
  Dummy$tweet<-dummy4
  Dummy<-Dummy[!is.na( Dummy$tweet),]
  dummy4<-Dummy$tweet
  Dummy$sentiment<-get_sentiment(char_v=dummy4,"nrc",dummy4) 
  dummy5[j]<-dummy3[2]
  data[j,2]<-mean(Dummy$sentiment)
  data[j,1]<-as.character(files[j])
  
  
  dummy6<-numeric(0)
  for(i in 1:length(dummy4))
  {
    
    abc<-annotateString(dummy4[i])
    a<-getSentiment(abc)  
    dummy6[i]<-a$sentimentValue[1]  
    
  }
  Dummy$stanford_sentiment<-dummy6
  data[j,3]<-mean(Dummy$stanford_sentiment)
  setwd("C:/Users/Puneet/Documents/Sentiment Analysis Project/Data New1")
  files1= list.files()
  
  write.xlsx(Dummy,files[j])
  
}

# write.xlsx(Dummy,dum)
# Dummy<- read.xlsx("62.xlsx",1)
# dummy2<- as.character(Dummy[,2])
# dummy4<- gsub("@\\w+","",dummy2)
# dummy4<- gsub("[[:punct:]]","",dummy4)
# dummy4<- gsub("http\\w+","",dummy4)
# dummy4<- gsub("[ \t]{2,}"," ",dummy4)
# dummy4<- gsub("^\\s+|\\s+$"," ",dummy4)
# Dummy$sentiment<-get_sentiment(char_v=dummy4,"nrc",dummy4)
# data<-data.frame(date=as.character(Dummy$Date[1]),sentiment=mean(Dummy$sentiment))
# data[2,2]<-as.character(Dummy$Date[1])
# 

# Writing Out the CSV's
write.csv(data,"data1.csv")
data$date<-dummy5


setwd("C:/Users/Puneet/Documents/Sentiment Analysis Project/Data 2")    
write.xlsx(Dummy,dum)

      
