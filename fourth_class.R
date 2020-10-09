# 파생변수 : 기존변수를 이용하여 새로운 변수를 추가하는 것

stock <- read.csv(file.choose())
str(stock)

head(stock)

stock$diff <- stock$High - stock$Low
head(stock)

mean(stock$diff)

# diff 변수를 이용해서 diff_result 파생변수 생성
# for 
#~ diff_result에 stock$diff의 평균보다 크면 "mean over"  작으면 "mean under"

row <- nrow(stock)
rows <- 1:row
diff_result = ""

for(idx in rows){
if(stock$diff[idx] > mean(stock$diff)){   
  diff_result[idx] <- "mean over"
}else{
  diff_result[idx] <- "mean under"
}
}
diff_result
stock$diff_result <- diff_result
head(stock)


#while(논리식 ex i<10) {
#        조건을 이용해서 탈출
#       }

idx <- 1
while(idx <= 10) {
 print(idx)
 idx <- idx +1
}


# 1 ~ 100 사이의 5의 배수만 출력하고 싶다면?
idx <- 1
while(idx <= 100){
  if(idx %% 5 == 0) {
   print(idx)  
  }
  idx <- idx + 1
}

idx <- 1
while(idx <= 100){
  if(idx %% 5 == 0) {
    cat(idx,",")                                 
  }
  idx <- idx + 1
}

# next(continues), break
# next는 조건에 만족했을때 프린트를 하지않고 다시 올라가서 다음 연산수행
# break는 조건에 만족했을때 루프를 더 돌수있지만 빠져나갈 수 있음          
idx <- 0
while(idx <= 10) {
  idx <- idx + 1
  if(idx %% 2 != 0){
   next
    }
   print(idx)
  }

idx <- 0
while(idx <= 10) {
  idx <- idx + 1
  print(idx)
  if(idx %% 2 != 0){
    break
  }
}


# NA확인
# is.na()
is.na(c(1,2,3,4,NA))

# NA개수를 확인
# data.frame
naDF <- data.frame(x = c(NA,2,3),
                   y = c(NA,4,5))
naDF
sum(is.na(naDF))

# NA처리    
NA & T 

# 문제를 해결하기 위해서 
sum(c(1,2,3,NA))
sum(c(1,2,3,NA), na.rm = T)
mean(c(1,2,3,NA), na.rm = T)

# package::caret
# na.omit(), na.pass(), na.fail()

na.omit(c(1,2,3,NA))
na.pass(c(1,2,3,NA))
na.fail(c(1,2,3,NA))

irisDF <- iris
irisDF
#임의로 결측값을 넣어보자
irisDF[4:10,3] <- NA
irisDF[1:5,4] <- NA
irisDF[60:70,5] <- NA
irisDF[97:103,5] <- NA
irisDF[133:138, 5] <- NA
irisDF[140,5] <- NA

irisDF

?heatmap
heatmap(1 * is.na(irisDF),
        Rowv = NA,
        Colv = NA,
        scale = "none",
        cexCol = .4)
        


#함수정의
#R에서는 return문을 주지않아도 마지막구문이 리턴으로 넘어옴


newSumFunc <- function(x,y) {
  result <- x + y
  #return (result)
  }

newSumFunc <- function(x,y) {
  cat("x=", x, "\n")
  cat("y=", y, "\n")
  #return (result)
}


resultSum <- newSumFunc(5,4)
#x,y를 바꿔서 호출하고 싶으면
resultSum <- newSumFunc(y=5, x=4)


#가변함수
varFunc <- function(...) {
  args <- list(...)
  result <- 0
  for(idx in args) {
    result <- result + idx              
  }
  return (result)
}


varFunc(1)
varFunc(1,2)                            
varFunc(1,2,3,4)



# 결측치 비율을 계산하는 함수를 만들어보자
# 행 및 열 별로 비율 계산

irisDF
naMissFunc <- function(x){
  sum(is.na(x)) / length(x) * 100                 
}

# 행별 결측치 비율을 계산해보자
rowMissingPer <- apply(irisDF, 1, naMissFunc)     
colMissingPer <- apply(irisDF, 2, naMissFunc)


# 조작함수
iris
mtcars

# merge() : join
# rbind() , cbind()

x <- data.frame(name = c("임정섭", "임은결", "임재원"),
                math = c(100, 60, 95))

y <- data.frame(name = c("임재원", "임은결", "임정섭"),
                english = c(100, 70, 90))

#데이터 순서가 달라도 공통요소를 기준으로 서로 데이터 병합
cbind(x,y)
rbind(x,y)
merge(x,y)


# doBy package
# summaryBy(), orderBy(), splitBy(), sampleBy()
summary(iris)


# 자료의 분포 quantile()
quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, seq(0,1,by=0.1))

install.packages("doBy")
library(doBy)

# summaryBy()
# 원하는 컬럼의 값을 특정조건에 따라 요약하는 목적
?summaryBy()
summaryBy(Sepal.Length + Sepal.Width ~ Species, iris )

# orderBy()
# 정렬을 위한 목적
 # 종별로 정렬되어있음
orderBy( ~ Species, iris)
orderBy( ~ Species + Sepal.Width, iris)

# package::base
# base order()
install.packages("base")
library(base)

# order() : 주어진 값을 정렬했을 때의 색인 순서대로 반환
order(iris$Sepal.Width)
iris[order(iris$Sepal.Width),]


# sample()
# 모집단으로부터 특정표본을 추출하는 함수
# 복원(뽑은것도 다시 넣고 뽑는거)
# 비복원(뽑은거 제외시키고 뽑는거)

sample(1:10, 10)
sample(1:10, 5, replace = T)

# iris에 sample()적용
iris[sample(nrow(iris),nrow(iris)),]

# 종별로 어떤비율만큼 데이터를 가져오겠다
?sampleBy()
test <- sampleBy( ~ Species ,frac = 0.2 ,data = iris)

# split()
# 반환값 list
# lapply, sapply
?split
split(iris, iris$Species)

 #iris 종별 Sepal.Length 평균(lapply)
 #값들을 데이터프레임에 입력
 irisList <- lapply(split(iris$Sepal.Length, iris$Species),mean)
 str(irisList)
 irisVec <- unlist(irisList)
 irisVec
 irisMat <- matrix(irisVec, ncol=3, byrow = T)
 irisMat
 irisFrame <- data.frame(irisMat) 
 irisFrame
 names(irisFrame) <- c("s_mean", "color_mean", "nica_mena") 
 class(irisFrame) 
 
 
# 조작
# package :: plyr
# filter : 로우 필터
# select : 컬럼 필터
# mutate : 열 추가
# arrange : 정렬
# summarise : 집계
# %>>$

#패키지여러개 설치
install.packages(c("plyr", "hflights"))
install.packages("dplyr")
library(plyr) 
library(hflights)
library(dplyr)

hflights
str(hflights)
as_tibble(hflights)

# filter() : 조건에 따라 행을 추출
# 1월 1일 데이터를 추출
?filter
filter(hflights,Month == 1, DayofMonth == 1)

# 1월 혹은 2월 데이터 추출
as_tibble(filter(hflights, Month ==1 | DayofMonth ==2))
tail(as_tibble(filter(hflights, Month ==1 | DayofMonth ==2)), 10)

#arrange() : 기본 오름차순정렬
#데이터를 ArrDelay, Month, Year 순으로 정렬
?arrange
arrange(hflights,ArrDelay,Month,Year)
arrange(hflights, desc(Month))

# select(), mutate() : 조건에 따라 열을 추출
# select(), :(인접한행) - (차)
select(hflights, Year, Month, DayofMonth)
select(hflights, Year:DayofMonth)
# Year부터 DayofWeek를 제외한 열을 추출
select(hflights, -(Year:DayofMonth))

# ArrDelay - DepDelay -> gain
# gain / (AriTime/60) -> gain_per_hour
# transform은 파생변수 사용X(새로운 칼럼을 정의할 수는 있지만 바로 같은 작업단에서 만들어진 칼럼을 다음 칼럼에서 사용할 수 없음 (2번째 작업참고))
flightDF <- hflights
mutate(flightDF,
       gain = ArrDelay - DepDelay,
       gain_per_hour = gain / (AirTime/60))


transform(flightDF,
          gain = ArrDelay - DepDelay,
          gain_per_hour = gain / (AirTime/60))

transform(flightDF,
          gain = ArrDelay - DepDelay)


#summarise()
#기초통계량(mean, sd, var, median) 구할 수 있다
#데이터 프레임 형식으로 반환

# 출발지연시간 평균 및 합계계산을 한다면?
mean(flightDF$DepDelay)
sum(flightDF$DepDelay)

sum(is.na(flightDF$DepDelay))
summarise(flightDF,
          mean = mean(flightDF$DepDelay, na.rm =T),
          sum = sum(flightDF$DepDelay, na.rm = T))

# hflights 데이터셋에서 비행편수 20편 이상, 평균 비행거리 2000마일 이상인 항공사별 평균 연착시간을 계산하시오
# TailNum : 항공기 일련번호
planes <- group_by(hflights, TailNum)
head(planes)

#비행편수, 평균거리, 평균연착시간
delay <- summarise(planes, 
                   count = n(),
                   dist = mean(Distance, na.rm = T),
                   delay = mean(ArrDelay, na.rm = T) )

delay <- filter(delay,count > 20, dist >= 2000)                   

library(ggplot2)
ggplot(delay, aes(dist, delay)) + 
       geom_point(aes(size=count), alpha = 1/2) 


# chain() 함수
# %>%
chain01 <- group_by(hflights, Year, Month, DayofMonth)
chain02 <- select(chain01, Year:DayofMonth, ArrDelay, DepDelay)
chain03 <- summarise(chain02, arrival = mean(ArrDelay, na.rm = T), 
                              depart  = mean(DepDelay, na.rm = T))
# 평균 출도착지연 시간이 30분 이상인 데이터 출력
result <- filter(chain03, arrival >= 30 | depart >= 30 )
hflights %>%
  group_by(Year, Month, DayofMonth) %>%
   select(Year:DayofMonth, ArrDelay, DepDelay) %>%
    summarise(arrival = mean(ArrDelay, na.rm = T),
              depart = mean(DepDelay, na.rm = T)) %>%
      filter(arrival >= 30 | depart >= 30)

# adply() : 3가지 작업 한번에 함, data.frame 리턴
# 데이터 분할(split)
# apply
# combine
?adply

iris
# Sepal.Length가 5.0이상이고, Species가 setosa인지 여부를 확인한 다음, 그 결과를 새로운 컬럼 V1에 기록한다면?
adply(iris,1,function(row) {
      data.frame(v1 = c(row$Sepal.Length >= 5.0 & row$Species == 'setosa')
)
} )
# Petal.Length가 3.0이상이고, Species가 virginica인지 여부 확인하고, 결과를 새로운 컬럼 v1에 기록한다
adply(iris,1,function(row) {
  data.frame(v1 = c(row$Petal.Length >= 3.0 || row$Species == 'virginica')
             )
}
)

g <- matrix(1:9, ncol=3)
