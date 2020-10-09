
# 교차검증을 위한 데이터셋 분리 방법
# 1. sample()
# 2. K-Fold 방식
# 3. Hold-Out 방식
# 트레이닝셋과 다르게 테스트셋은 성능을 평가할때만 사용

# 1. 단순임의 추출 : 아이리스같은 데이터를 사용할때 특정종에 데이터가 몰릴수 있음
# sample()
sample(1:10, 5) # 비복원
sample(1:10, replace = T) # 복원 

# 2. K-Fold방식 (폴드해서 트레이닝과 테스트셋으로 나눔,5번폴드하면 검증도 5번해야함,폴드할때마다 정확도는 달라짐)
install.packages("cvTools")
library(cvTools)

set.seed(100)
?cvFolds
fold <- cvFolds(n = 6, K = 5, R = 1)
fold

str(fold)
fold$subsets
fold$which

fold$subsets[fold$which==1, 1] 
fold$subsets[fold$which==2, 1]
fold$subsets[fold$which==3, 1]

set.seed(200)
irisFold <- cvFolds(n = nrow(iris), K = 3, R = 1) # 10번반복(10번의 검증을 하겠다), 열의 값이 3개
irisFold$subsets[irisFold$which==1, 1]  # 트레이닝데이터가 아니라 테스트데이터로 쓰겠다
irisFold$subsets[irisFold$which==2, 1]
irisFold$subsets[irisFold$which==3, 1]

# classification - naiveBayes알고리즘 (텍스트마이닝에 많이사용)
# 목표변수가 범주형

acc <- numeric()
cnt <- 1
r <- 1
k <- 1:3
k[-1]

for(i in k){
  print(i)

for(j in 1:10){
  cat(j,"...")  
} 
 cat("\n")
   }

#### 개행안했기때문에 1 23  2 23 3 23 이런식으로 나옴
for(i in k){
  print(i)

for(j in k[-1]){
  print(j)
}
}

#예시
for(i in k) {
  idx <- irisFold$subsets[irisFold$which==i, r]
  cat("test :" ,i,"Cross Validation\n")
  print(iris[idx,])
  test <- iris[idx,]
  for(j in k[-1]){                                  
}
}

install.packages("e1071")
library(e1071)
for(i in k){
  idx <- irisFold$subsets[irisFold$which ==i, r]
  cat("test :",i,"Cross Validation\n")
  print(iris[idx,])
  test <- iris[idx,]
  
  for(j in k[-i]){
  idx <- irisFold$subsets[irisFold$which ==j, r]         #### k 1번을 수행하고 트레이닝 1제외한 2,3 k 2번수행하면 트레이닝은 2제외한 1,3 수행 이런식  
  cat("train :",j,"Training Data\n")
  train <- iris[idx,]
  cat("rows:", nrow(train),"\n")
  model <- naiveBayes(Species ~. , data = train)                
  pred <- predict(model, test)
  t <- table(pred, test$Species)
  print(t)
  acc[cnt] <- (t[1,1] + t[2,2] + t[3,3]) / sum(t)
  cnt <- cnt + 1
  }
  }
acc
mean(acc)


# 3.Hold - Out 교차검증 : 샘플의 종의 비율문제를 커버하고 있음
install.packages("caret")
library(caret)

# createDataPartition()
?createDataPartition

set.seed(300)
hold_out_train <- createDataPartition(iris$Species, p=.8)
names(hold_out_train)

table(iris[hold_out_train$Resample1,"Species"])   #80프로
table(iris[-hold_out_train$Resample1,"Species"])  #20프로만 가져온거 확인

train_iris <- iris[hold_out_train$Resample1,]
test_iris <- iris[-hold_out_train$Resample1,]

model <- naiveBayes(Species ~., data = train_iris)
pred <- predict(model, test_iris)
t <- table(pred, test_iris$Species)
print(t)    ###1개정도의 오류

acc <- (t[1,1] + t[2,2] + t[3,3]) / sum(t)  ##print(t)참고기준



#-------분류 실습(Naive Bayes Classifier)
# 텍스트분류, 문서를 여러 범주 나누어서 판단하는 알고리즘
# 조건부 확률
# 10개의 메일 중, 3개는 스팸메일이다
# 그리고 그와 상관없이 free라는 단어를 포함하는 메일이 4개다.
# 문제는 free(A)라는 메일이 와 있을때, 그것이 스팸메일(B)인지 아닌지를 구분해야 한다면?
# 공식 : P(B/A) = P(B) * P(A/B) / P(A)

 # 1. 스팸메일일 확률 : 3/10
 # 2. free를 포함하는 메일의 확률은 : 4/10
 # 3. 스팸메일 중에 포함된 FREE 포함 메일 : 2/3
 # P(SPAM/FREE) = P(SPAM) * P(FREE/SPAM) / P(FREE)


iris
install.packages("klaR")
library(klaR)

train <- sample(nrow(iris), 100)
naive_model <- NaiveBayes(Species ~. , data = iris, subset = train)  ## 종별 비율이 32,33,35퍼가 되었다 등 확인가능

pred <- predict(naive_model, iris[-train,]) #iris데이터에서 train을 빼주겠다
names(pred)

tt <- table(iris$Species[-train], predict(naive_model,iris[-train,])$class)
1- sum(tt[row(tt) == col(tt)]) /sum(tt)  ##분류율이 100%으로 나오는데 1에서 빼면 0으로 나움

library(ggplot2)
test <- iris[-train,]
test$pred <- predict(naive_model, iris[-train,])$class

ggplot(test, 
       aes(Species, pred, col = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size = 2) +
  labs(x = "Species", y = "Predict")





#---------------------------------------
# 웹크롤링 
# 웹에서 브라우저라는 프론트에 html은 문서의구조 css는 데코레이션 자바스크립트는 동작처리. 프론트에서 리퀘스트를 하면 백엔드쪽에서 리스폰스처리.
# 백엔드에서는 데이터의 동적처리를 하려면 WAS라고 하는 서버가 필요하고 서버안에 프레임워크가 들어있음.대표젹인 프레임워크가 장고
# 파이썬이라는 언어를 통해서 외부의 DBMS와 호출할 수도 있어야함

install.packages("rvest")
library(rvest)
library(dplyr)
url <- "https://dhlottery.co.kr/store.do?method=topStoreRank&rank=1&pageGubun=L645" ##크롤링하고싶은 사이트 URL
link <- read_html(url)

#노드워킹      ##웹에서 id로 나타내는것은 #으로 class는 .으로 찾기
link %>%      
  html_nodes('body') %>%                       ## 태그가 중복되지않는 경우만 태그 이름으로 찾을 수 있음
  html_nodes('.containerWrap') %>%
  html_nodes('.contentSection') %>%
  html_nodes('#article')

#직접적으로 아티클찾기
link %>%
  html_nodes('#article')        

link %>%
  html_nodes('td') %>%           #클래스나 id가 없어서 태그로 읽어옴,  td5줄 tr15개를 읽어옴
  html_text()

lotto15 <- link %>%
  html_nodes('tbody tr td') %>%
  html_text()                    ##태그와 텍스트중 텍스트만 가져오겠다

#그래도 특문이나 공백등 떄문에 전처리가 필요
library(stringr)
lotto15 <- str_replace_all(lotto15, "\t|\n|\r","")
lotto15 <- str_replace_all(lotto15, "[[:space:]]","")

#1,2,3,...인덱스숫자가 의미가없어서 뺴려고 함.그래서 나머지 값들을 하나의 로우로 만들어서 루프를 돌릴것
storeName <- NULL
cnt       <- NULL
address   <- NULL

for(idx in 1:length(lotto15) ) {
  if(idx %% 5 == 2) {            ##상호이름이 로또테이블 가로줄 5개중에 2번째에 계속 들어있음
    storeName <- c(storeName, lotto15[idx])
  }else if(idx %% 5 ==3) {
    cnt <- c(cnt, lotto15[idx])
  }else if(idx %% 5 ==4) {
    address <- c(address, lotto15[idx])
  }
}
lottoDF <- data.frame(storeName,cnt,address)
#####첫페이지처리

last <- link %>%
  html_nodes('.paginate_common') %>%
  html_nodes('a') %>%
  html_attr('onclick') %>% tail(1)    ###왜 온클릭????
?html_attr
?gregexpr
?regmatches
end <- regmatches(last , gregexpr('[0-9]',last))
end <- as.numeric(end[[1]])
end <- as.numeric(paste(end[1],end[2],end[3],sep= ""))      ##이 과정 왜하지???
end
#cmd창 열어서 cd C:\Rselenium입력한다음 java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.11.0.jar -port 4445 입력 
install.packages('RSelenium')
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = 'chrome')
remDr$open()
remDr$navigate("https://dhlottery.co.kr/store.do?method=topStoreRank&rank=1&pageGubun=L645")


# 본격적으로 크롤링
#페이지 숫자 찾아서 나머지도 호출해야함
lottoStore = c()

for(idx in 1:end) {                        ##루프출발
  front <- "selfSubmit("
  back  <- ")"
  script <- paste(front, idx, back, sep="")      ##for구문이해안됨..

pagemove <- remDr$executeScript(script, args=1:2)

# 소스 받아오기
source <- remDr$getPageSource()[[1]]
js_html <- read_html(source)

js_link <- html_nodes(js_html,'tbody')
stores <- js_link %>%
  html_nodes('tr') %>%
  html_nodes('td') %>%
  html_text()


lottoStore = c(lottoStore, stores)
}                     ##루프 끝

lottoStore <- str_replace_all(lottoStore, "\t|\n|\r","")
lottoStore <- str_replace_all(lottoStore, "[[:space:]]","")

storeName <- NULL
cnt       <- NULL
address   <- NULL

for(idx in 1:length(lottoStore) ) {
  if(idx %% 5 == 2) {            
    storeName <- c(storeName, lottoStore[idx])
  }else if(idx %% 5 ==3) {
    cnt <- c(cnt, lottoStore[idx])
  }else if(idx %% 5 ==4) {
    address <- c(address, lottoStore[idx])
  }
}
lottoDF <- data.frame(storeName,cnt,address)
write.csv(lottoDF,"lotto_store.csv", row.names = F)  ##파일생성


