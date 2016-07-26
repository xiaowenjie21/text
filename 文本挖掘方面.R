library(rJava)
library(Rwordseg)
library(wordcloud)
library(tmcn)
library(RMySQL)
library(tm)
library(stringr)
library(sqldf)
library(topicmodels)

con <- dbConnect(
  RMySQL::MySQL(), host = "192.168.1.249",
  user = "root", password = "root",
  dbname='meituan_comment')

dbSendQuery(con,'SET NAMES gbk')
query_sql="SELECT
DISTINCT(comment)
FROM
meituan_comment
WHERE
meituan_comment.comment_time LIKE '%2016%'
and 
meituan_comment.`comment` not like ''
and meituan_comment.`comment` not like '%用户未填写%' ORDER BY speed desc"

dbresult<-dbSendQuery(con,query_sql)
dbre<-dbFetch(dbresult,n=-1)

#分词
my_se=segmentCN(dbre$comment,returnType = 'tm')

mydoc.vec<-VectorSource(my_se)
mydoc.corpus<-Corpus(mydoc.vec) 
mydoc.corpus<-tm_map(mydoc.corpus,removeNumbers)                #删除数字
mydoc.corpus<-tm_map(mydoc.corpus,stripWhitespace)             #删除空白
myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者")
d.corpus <- tm_map(mydoc.corpus, removeWords, myStopWords)
control=list(removePunctuation=T,stopwords=stopwordsCN(),minDocFreq=5,wordLengths = c(2, Inf))
mydoc.tdm=DocumentTermMatrix(d.corpus,control)  


m1 <- as.matrix(mydoc.tdm) #转矩阵
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word, d$freq, min.freq = 10, random.order = F, ordered.colors = F, 
          colors = rainbow(length(row.names(m1))))

ctm<-CTM(mydoc.tdm[10000,],k=2)
tem<-terms(ctm,2,0.01)

dbDisconnect(con)