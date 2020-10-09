#시각화 마무리 & EDA
library(ggplot2)

seoul_subway <- read.csv(file.choose())
# seoul_subway <- read.csv(file.choose(),sep="",header = T)
head(seoul_subway)
str(seoul_subway)

# x축을 평균일 승차인원으로 설정하고, y축을 각 노선의 운행횟수로 설정해서, 평균혼잡도로 산점도를 그려보자
ggplot(data = seoul_subway,
       aes(x=AVG_ONEDAY, y=RUNNINGTIMES_WEEKDAYS)) +
  geom_point(aes(col = LINE, size = AVG_CROWDEDNESS))
 

# x축을 각 노선으로 하는 일 평균 막대그래프를 만들어보자
ggplot(data = seoul_subway,
       aes(x = LINE,y =  AVG_ONEDAY)) +
  geom_bar(stat = "identity",aes(fill = LINE),col="black" )



# mtcars
# 차량별 연비를 가지고 시각화 하려고한다.
# challenge point : 차량종류가 피처로 나와있지않고 인덱스로 나와있음
# rownames()
mtcars
str(mtcars)
head(mtcars)

mtcars$type <- rownames(mtcars)
mtcars

# 차량별 연비를 이용해서 막대그래프(aes축을 바에서 설정해도됨)
ggplot(data =  mtcars,
       aes(x=type, y=mpg)) +
  geom_bar(stat = "identity", aes(fill = mpg),col="black",width=.5)


# reorder(type, mtcars$mpg) : 오름차순 배열 내림차순은 -mtcars$mpg
ggplot(data = mtcars, aes(x = reorder(type, -mtcars$mpg), y = mpg)) +
  geom_bar(stat = 'identity',aes(fill = type)) +
  theme(axis.text.x = element_text(angle = 90))


## 축회전  
ggplot(data = mtcars, aes(x = reorder(type, -mtcars$mpg), y = mpg)) +
  geom_bar(stat = 'identity',aes(fill = type)) +
  coord_flip()
  
carMpg <- ggplot(data = mtcars, aes(x = reorder(type, -mtcars$mpg), y = mpg)) +
          geom_bar(stat = 'identity') +
          coord_flip()
carMpg + labs(x='',y='',title = '차량별 연비')




# EDA(Exploratory Data Analysis)
# 1. 데이터 탐색
# 2. 결측치(NA) 처리
# 3. 이상치(outlier) 발견 처리
# 4. 리코딩(코딩 변경)
# 5. 파생변수, 가변수
# 6. 시각화
# 7. 의사결정

# service_data_dataset_eda.csv
svcdata <- read.csv(file.choose())
head(svcdata)
str(svcdata)
dim(svcdata)
nrow(svcdata)
ncol(svcdata)
length(svcdata)

names(svcdata)
summary(svcdata)
summary(svcdata$price)
is.na(svcdata$price)

table(is.na(svcdata$price))

# 결측치 처리하는 방법
# caret : na.omit() ,결측치 행 자체를 제거
svcdata_new <- na.omit(svcdata)
table(is.na(svcdata_new))
str(svcdata_new)

# 결측치 대체
price <- svcdata$price

 # 0 대체
price <- ifelse(is.na(price), 0 , price)

 # 평균 대체
mean(svcdata$price, na.rm = T)

svcdata$price <- ifelse(is.na(svcdata$price), mean(svcdata$price,na.rm = T), svcdata$price)

 # 통계적 방법(분석하는 사람의 기준에 따라 만들어지는 가변수)
priceAvg <- mean(svcdata$price, na.rm = T)
str(svcdata)
svcdata$type <- rep(1:3,100)
head(svcdata)


 # type : 1,2,3 (1번 15%, 2번 10%, 3번 5% 인상)
 # 300명의 고객유형을 판단하여 새로운 가변수 priceSta 만들어 처리된 값을 넣어보자
svcdata
str(svcdata)
svcdata$price <- ifelse(is.na(svcdata$price), mean(svcdata$price,na.rm = T), svcdata$price)


temp = 0
for(i in 1:nrow(svcdata)){
    if(svcdata$type[i] == 1){
      temp[i] = price[i] * 1.15
    }else if(svcdata$type[i] ==2){
      temp[i] = price[i] * 1.1
    }else{
      temp[i] = price[i] * 1.05
    }
  }                  

svcdata$priceSta <- temp  
str(svcdata)

####경린이 답
priceSta=""

size<-nrow(service)

for(idx in 1:size){
  
  if(service$type[idx] == 1){
    
    priceSta[idx]=service$price*1.5
    
  }else if(service$type[idx] == 2){
    
    priceSta[idx]=service$price*1.0
    
  } else{
    
    priceSta[idx]=service$price*0.5
    
  }
  
}

service$priceSta<-priceSta


gender <- svcdata$gender
range(gender)                            
table(gender)
str(gender)
#subset을 활용하여 이상치를 제거한 후 gender를 범주형으로 변환해보자
?subset
gender <- subset(gender, gender == 2 | gender == 1)
gender <- factor(gender)
class(gender)

svcdata$gender <- factor(svcdata$gender)
class(svcdata$gender)
table(svcdata$gender)
svcdata <- subset(svcdata,gender ==1 | gender ==2)

#변수의 유형이 연속변수라면 어떻게 이상치를 제거할까요? 
seqPrice <- svcdata$price
length(seqPrice)
summary(seqPrice)
 # IQR(3사분위값 - 1사분위값) 6.2 - 4.6 = 1.6
 # 2.3은 lower whisker, 7.9는 upper whisker
outlier <- boxplot(seqPrice)
svcdata <- subset(svcdata, seqPrice >= 2.3 & seqPrice <= 7.9)
nrow(svcdata)
boxplot(svcdata$price)

# AGE체크
summary(svcdata$age)
str(svcdata$age)
head(svcdata$age)
### 아닌듯 ifelse(is.na(svcdata$age), mean(svcdata$age,na.rm = T), svcdata$age)

# 결측치 제거 후 시각화
ages <- subset(svcdata,is.na(age)==F)
boxplot(ages$age)
summary(ages$age)
str(ages)
ages$age
sum(is.na(ages$age))
head(svcdata)

### 아닌듯? ggplot(data = svcdata, aes(x=age, y=price)) +
###          geom_bar(stat = "identity")


# 리코딩 - 데이터의 가독성을 위해서
# 연속형 -> 범주형

# 형식) svcdata$컬럼[조건식] <- 추가할 값
# 1 : 서울, 2: 부산, 3: 광주, 4: 대전, 5: 대구
svcdata$resident_new[svcdata$resident == 1] <- "서울"
svcdata$resident_new[svcdata$resident == 2] <- "부산"
svcdata$resident_new[svcdata$resident == 3] <- "광주"
svcdata$resident_new[svcdata$resident == 4] <- "대전"
svcdata$resident_new[svcdata$resident == 5] <- "대구"

# 주거지의 NA값을 행정수도인 대전으로 대체하고 범주형으로
str(svcdata)
head(svcdata)
svcdata$resident <- ifelse(is.na(svcdata$resident),"4", svcdata$resident)
svcdata$resident <- factor(svcdata$resident)

# job
# 1 : 분석가, 2 : 데이터과학자, 3 : 개발자
svcdata$job_new[svcdata$job == 1] <- "분석가"
svcdata$job_new[svcdata$job == 2] <- "데이터과학자"
svcdata$job_new[svcdata$job == 3] <- "개발자"
svcdata$job_new[svcdata$job == 4] <- "취준생"

# job NA값 다른걸로 대체하고 범주형으로
svcdata$job <- ifelse(is.na(svcdata$job),"4", svcdata$job)
svcdata$job <- factor(svcdata$job)


# --- 간단 분석
url <- "https://www.dropbox.com/s/0djexymb42zd1e2/example_salary.csv?dl=1"
salary_data_eda <- read.csv(url, stringsAsFactors = F, na = "-")   ###여기서는 결측치가 "-"표시로 나옴
head(salary_data_eda)
str(salary_data_eda)

test_salary <- salary_data_eda ##데이터
# 1. 컬럼명을 영문으로 변경
names(test_salary) <- c("age", "salary","incentive","laborhour","howmany","experience","gender")

# 2. 각 피쳐별 결측값 확인
is.na(test_salary)

##강사님답
sum(is.na(test_salary$age))

# 3. 임금 평균 확인
mean(test_salary$salary, na.rm = T)

# 4. 임금 중앙값 확인
median(test_salary$salary, na.rm = T)

# 5. 임금 범위 구해보기(최저, 최고)
range(test_salary$salary, na.rm = T)
min(test_salary$salary, na.rm = T)
max(test_salary$salary, na.rm = T)

# 6. 임금에 대한 사분위수(quantile())구하기
quantile(test_salary$salary, na.rm = T)

# 7. 성별에 따른 임금 격차 확인해보기
ggplot(data = test_salary, aes(x=gender, y=salary)) +
  geom_bar(stat = "identity", width = .3)

##강사님 답
salary.gender.sal <- tapply(test_salary$salary,test_salary$gender,mean,na.rm = T)

# 8. 분석된 데이터를 가지고 원하는 시각화 진행
 #나는 노동시간에 따른 임금을 바형식으로 보고
ggplot(data = test_salary, aes(x=laborhour, y=salary)) +
  geom_line(size = .5, col = "red")

##강사님 답
salary.melt <- melt(salary.gender.sal)
ggplot(salary.melt, aes(x = Var1 , y = value, fill = Var1))+
  geom_bar(stat = "identity")

 #사람들의 임금을 산점도로 보고싶다
x <- test_salary$howmany
x <- ifelse(is.na(test_salary$howmany),mean(test_salary$howmany,na.rm = T), test_salary$howmany)
dotchart(x)


# 9. 성별에 따른 임금 표준편차 구하기
aggregate(test_salary$salary,list(test_salary$gender),sd)
test_salary$salary <- ifelse(is.na(test_salary$salary),mean(test_salary$salary,na.rm = T),test_salary$salary)

##강사님답 
tapply(test_salary$salary, 
       test_salary$gender, 
       range, 
       na.rm = T)

# 10. 경력별 임금 평균치
aggregate(test_salary$salary,list(test_salary$experience),mean)


##강사님 답
career.melt <- tapply(test_salary$salary, 
       test_salary$experience, 
       mean, 
       na.rm = T)

# 11. 경력별 임금 평균치 시각화
ggplot(data = test_salary, aes(x=experience, y=salary)) +
  geom_bar(stat = "identity",width = .5)


##강사님 답
career.melt <- melt(career.melt)

ggplot(career.melt, aes(x = Var1, y = value, group = 1)) + 
  geom_line(color = "skyblue2",
            size = 2) +
  coord_polar() +
  ylim(0 , max(career.melt$value))
  


#실습2!!!
#서대문구에 통닭집이 많은 동을 시각화 해보자
install.packages("readxl")
library(readxl)
#dataload : service_data_chicken_store_eda_visualization
ck <- read_xlsx(file.choose())
head(ck)
str(ck)


# substr()함수를 이용하여 소재지전체주소에 동만 가져오기
# 실행결과 동이름이 3글자인 경우와 4글자인 경우가 있으므로 지정한 자리만큼 글자를 추출하면 3글자인 동은 숫자와 포함된다.
# 공백과 숫자를 제거하자
pattern <- regexpr("[[:alpha:]]{1,}동", ck$소재지전체주소)
dong <- regmatches(ck$소재지전체주소, pattern)
head(dong)

##책 정답
ck1 <- substr(ck$소재지전체주소,12,16)
ck2 <- gsub("[0-9]","",ck1)
ck3 <- gsub(" ","",ck2)
head(ck3)

install.packages("dplyr")
library(dplyr)


# 동별 도수분포표 만들어보기
# table()함수를 이용해서 숫자 세기, 변수가 한개일때 도수분포표를 만들어줌
# 도수분포표란 항목별 개수를 나타낸 것이다.

freq <- table(ck3)
tableDF <- as.data.frame(freq)


install.packages("treemap")
library(treemap)


# 트리맵은 옵션으로 데이터프레임을 입력받는다
# treemap(데이터, index=구분 열, vSize=분포 열, vColor=컬러, title=제목)
?treemap
treemap(dtf = tableDF, index="ck3", vSize="Freq", vColor="black", title="서대문구 치킨")

