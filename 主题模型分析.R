library(jiebaR)
library(wordcloud)
library(tmcn)
library(RMySQL)
library(tm)
library(stringr)
library(sqldf)
#LDA 主题模型分析
#----------------------------获取数据部分-------------------
con <- dbConnect(RMySQL::MySQL(), host = "192.168.1.249",user = "root", password = "root",dbname='meituan_comment')
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
dbDisconnect(con)

#--------------------------分词与建立模型部分------------------------------------------

library(jiebaR)
library(wordcloud)
library(tmcn)
library(RMySQL)
library(tm)
library(stringr)
library(sqldf)
library(lda)
library(LDAvis)
library(servr)


cutter <- worker(bylines = T,type='mix',stop_word  = "D:/admin/Documents/stopword2.txt",encoding = "UTF-8")
#------添加停用词
# mixseg = worker(stop_word = "D:\\admin\\Documents\\stopword2.txt")
# new_user_word(mixseg, "以前", "n")

comments_seg <- cutter[dbre$comment]
# doc.list <- strsplit(comments_seg,split=" ")
# term.table <- table(unlist(doc.list)) 
doc.list<-comments_seg
term.table<-table(unlist(comments_seg))
#这里有两步，unlist用于统计每个词的词频；table把结果变成一个交叉表式的factor，原理类似python里的词典，key是词，value是词频
term.table <- sort(term.table, decreasing = TRUE) #按照词频降序排列
vocab <- names(term.table)

get.terms <- function(x) {
  index <- match(x, vocab)  # 获取词的ID
  index <- index[!is.na(index)]  #去掉没有查到的，也就是去掉了的词
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))   #生成上图结构
}
documents <- lapply(doc.list, get.terms)

K <- 12  #主题数
G <- 10000    #迭代次数
alpha <- 0.10   
eta <- 0.02
set.seed(100) 
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, num.iterations = G, alpha = alpha, eta = eta, initial = NULL, burnin = 0, compute.log.likelihood = TRUE)

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))  #文档—主题分布矩阵
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))  #主题-词语分布矩阵
term.frequency <- as.integer(term.table)   #词频
doc.length <- sapply(documents, function(x) sum(x[2, ])) #每篇文章的长度，即有多少个词

json <- createJSON(phi = phi, theta = theta, 
                   doc.length = doc.length, vocab = vocab,
                   term.frequency = term.frequency)
#json为作图需要数据，下面用servis生产html文件，通过out.dir设置保存位置
serVis(json, out.dir = './vis7', open.browser = FALSE)
writeLines(iconv(readLines("./vis7/lda.json"), from = "GBK", to = "UTF8"), 
           file("./vis7/lda.json", encoding="UTF-8"))