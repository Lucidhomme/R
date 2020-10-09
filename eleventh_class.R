# Text Mining

Sys.setenv(JAVA_HOME= 'C:\\Program Files\\Java\\jdk1.8.0_121')
library(rJava)

install.packages(c("hash", "tau", "Sejong", 
                   "RSQLite", "devtools", "bit", 
                   "rex", "lazyeval", "htmlwidgets", 
                   "crosstalk", "promises", "later", 
                   "sessioninfo", "xopen", "bit64", 
                   "blob", "DBI", "memoise", "plogr", 
                   "covr", "DT", "rcmdcheck", "rversions"), 
                 type = "binary")  

# github 버전 설치
install.packages("remotes")
# 64bit 에서만 동작합니다.
remotes::install_github('haven-jeon/KoNLP', 
                        upgrade = "never", 
                        INSTALL_opts=c("--no-multiarch"))
install.packages("wordcloud")
install.packages("tm")
install.packages("KoNLP")
install.packages("RColorBrewer")
library(KoNLP)
library(tm)
library(wordcloud)
library(RColorBrewer)

# 명사, 형용사등을 추출한 사전
useSejongDic()

text <- "최근 이슈가 되고 있는 빅데이터에 대한 이해와 활용을 위해 데이터 과학(Data Science)의 측면에서 접근한다.
빅데이터는 통계학을 비롯한 경영, IT 등의 다양한 분야들이 서로 결합되어 있고 그 정의가 다양하지만, 본 강의는 데이터 분석을
기반으로 하는 과학적 의사결정의 관점에서 바라보고자 한다. 빅데이터에 대한 이해를 위해 실제 사례들을 살펴보고,
데이터를 통해 의사결정에 유용한 정보 및 지식을 찾는 과정을 이해한다. 나아가 빅데이터 분석에서 필수적으로 언급되고 있는
R 통계프로그램을 소개하고 이를 분석에 활용할 수 있게 한다."

nouns <- extractNoun(text)

# 2자리 이상의 명사들만 꺼내오려함
# nchar()  : 캐릭터의 렝스를 리턴
nouns <- nouns[nchar(nouns) >= 2]

# 빈도표
wFreq <- table(nouns)
nouns <- gsub("빅데이터.+","빅데이터",nouns) #빅데이터로 시작하는 한자이상은 무엇이든 빅데이터로 바꾸겠다
wFreq <- table(nouns)
names(wFreq)

pal <- brewer.pal(6,"Accent")
wordcloud(words = names(wFreq),
          freq = wFreq,
          min.freq = 1,
          random.order = F,
          colors = pal)


# 텍스트 마이닝
# text -> corpus(말뭉치) -> TDM -> TM 분석 -> Matrix(DF) -> wordcloud
# service_data_I_love_mom.txt

#############################################################
data <- readLines(file.choose(), encoding = "UTF-8")

corpus <- VCorpus(VectorSource(data))  ##전처리

# TermDocumentMatrix : 행이 단어, 열 문서 행렬만듬. TDM은분포를 확인할 수 있음.  (9개의 다큐먼트에서 37개의 단어를 찾음,한번이라도 노출된 단어의 개수가 56개,가장긴 단어가 10개)
TDM <- TermDocumentMatrix(corpus)


# tm_map(TDM, function) : 말뭉치 전처리
corpus_map <- tm_map(corpus, stripWhitespace) ##여러개의 공백을 하나의 공백으로 만들어줌
corpus_map[[1]]$content
corpus_map <- tm_map(corpus, removeNumbers)  ## 숫자제거
corpus_map <- tm_map(corpus, removePunctuation) ## 특문제거
corpus_map[[9]]$content

#69~74번을 한번에 처리하는 방법
corpus_tm2 <- TermDocumentMatrix(corpus,control = list(stripWhitespace =T, removeNumbers = T, removePunctuation =T))


# stopword(불용어) 처리 방법
stopwords('en')
stopword2 <- stopwords('en')
stopword2 <- c(stopwords('en'),"and","not","but")

# 불용어 삭제
DTM <- DocumentTermMatrix(corpus_map)
corpus_map <- tm_map(corpus,removeWords,stopword2)


# 빈도 체크
TDM2 <- TermDocumentMatrix(corpus_map) 
corpus_freq <- as.matrix(TDM)


# 몇자이상 몇자이하
findFreqTerms(TDM, 2)     ##2번이상
findFreqTerms(TDM, 2, 4)  ##2자이상 4자이하

##############################################################

###위 과정 단계별로 정리
data <- readLines(file.choose(), encoding = "UTF-8")

corpus <- VCorpus(VectorSource(data))  ##말뭉치로 생성

# tm_map(TDM, function) : 말뭉치 전처리
corpus_map <- tm_map(corpus, stripWhitespace) ##여러개의 공백을 하나의 공백으로 만들어줌
corpus_map[[1]]$content
corpus_map <- tm_map(corpus_map, removeNumbers)  ## 숫자제거
corpus_map <- tm_map(corpus_map, removePunctuation) ## 특문제거
corpus_map[[9]]$content
stopwords('en')
stopword2 <- stopwords('en')  ###불용어처리    
stopword2 <- c(stopwords('en'),"and","not","but")
corpus_map <- tm_map(corpus_map,removeWords,stopword2)

# TermDocumentMatrix : 행이 단어, 열 문서 행렬만듬. TDM은분포를 확인할 수 있음.  (9개의 다큐먼트에서 37개의 단어를 찾음,한번이라도 노출된 단어의 개수가 56개,가장긴 단어가 10개)
TDM <- TermDocumentMatrix(corpus_map) 

# 빈도 혹은 몇자이상 몇자이하 찾기
?findFreqTerms
findFreqTerms(TDM,2)

# 빈도표생성
matrix <- as.matrix(TDM)
rownames(matrix)
rowSums(matrix)

wFreq <- sort(rowSums(matrix), decreasing = T)

wordcloud(words = names(wFreq),
          freq = wFreq,
          min.freq = 1,
          random.order = F,
          colors = pal)



#-----[실습] 
#-----service_data_president_text_mining.txt
data <- readLines(file.choose())

presidents <- VCorpus(VectorSource(data)) 

presidents_map <- tm_map(presidents, stripWhitespace)
presidents_map <- tm_map(presidents_map, removeNumbers)
presidents_map <- tm_map(presidents_map, removePunctuation)

##명사만추출
nounsword <- function(doc){
  d <- as.character(doc)
  extractNoun(d)
}

ff <- TermDocumentMatrix(presidents_map,control=list(tokenizse=nounsword))   ###컨트롤로 명사추출
findFreqTerms(ff,2)

stopwordkor <- c("만들겟습니다","해결하겟습니다","날아가겟습니다")
presidents_map <- tm_map(presidents_map,removeWords,stopwordkor)
ff <- TermDocumentMatrix(presidents_map,control=list(tokenizse=nounsword,wordLengths=c(2,3),stopwords=stopwordkor))
findFreqTerms(ff,2)

ff_matrix <- as.matrix(ff)
rownames(ff_matrix)
rowSums(ff_matrix)

ffFreq <- sort(rowSums(ff_matrix), decreasing = T)


wordcloud(words = names(ffFreq),
          freq = ffFreq,
          min.freq = 1,
          random.order = F,
          colors = pal)


str(wDF)
wDF <- data.frame(word = names(ffFreq), freq = ffFreq)
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(wDF,
           size = 2,
           minSize = 2)



#--------[실습]
#--------service_data_text_mining_movie.csv
#워드클라우드, 단어빈도 그래프(barplot) 그리기
#데이터 프레임생성
movies <- read.csv(file.choose())

movieCorp <- VCorpus(VectorSource(movies$Description))
movieCorp <- tm_map(movieCorp,stripWhitespace)
movieCorp <- tm_map(movieCorp,stemDocument,language = "en")
moviecorp <- tm_map(movieCorp,removePunctuation)
moviecorp <- tm_map(movieCorp, removeNumbers)
moviecorp <- tm_map(movieCorp, content_transformer(tolower))
stopword3 <- stopwords('en')
stopword3 <- c(stopwords('en'),"and","the","her","for","his","who","with","their","from","into","she","are","they","been","between","not","must","that","after","him","is","whose","he","over","more","take","become"
               )
moviecorp <- tm_map(movieCorp,removeWords,stopword3)
### 불용어 제거가 안됨...
movieTDM <- TermDocumentMatrix(movieCorp,control=list(           #아래로옵션을나열
  tokenize=words,                                              #미리만들어둔함수(보통명사추출)로문장을자름
  removeNumbers=T,                                             #숫자제거
  removePunctuation=T,                                         #문장부호제거
  wordLengths=c(1, 5)                                         #1~5음절로이루어진단어만추출
))
inspect(movieTDM)

moviemat = as.matrix(movieTDM)

movieFreq <- sort(rowSums(moviemat), decreasing = T)

wordcloud(words = names(movieFreq), freq = movieFreq, min.freq = 5, max.words = 200, random.order = F, colors = brewer.pal(8,"Dark2"))

movieDF <- data.frame(word = names(movieFreq), freq = movieFreq)

barplot(movieDF[1:10,]$freq,
        las = 2,
        names.arg = movieDF[1:10,]$word,
        col = "lightblue", main = "Most frequent words",
        ylab = "Word frequencies")

