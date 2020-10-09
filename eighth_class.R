#service_data_new_data_eda.csv
dataset <- read.csv(file.choose())
View(dataset)
str(dataset)

# 범주형 vs 범주형을 가지고 데이터의 분포를 확인한다면?
# 1. resident2, gender2를 범주형으로 변환
dataset$resident2 <- factor(dataset$resident2)
dataset$gender2 <- factor(dataset$gender2)
levels(dataset$resident2)
levels(dataset$gender2)

# 2. 두 변수를 table()함수를 이용하여 분포를 확인해보자
resident_gender <- table(dataset$resident2, dataset$gender2)

#막대 가로로 horiz이용
barplot(resident_gender,horiz = T)
#한 막대로 누적되어있는거를 멀티바로 나열
barplot(resident_gender,horiz = T,beside = T)
#범례 추가
barplot(resident_gender,horiz = T,beside = T, legend = row.names(resident_gender))
#색 지정
barplot(resident_gender,horiz = T,beside = T, legend = row.names(resident_gender), col = rainbow(5))

###위 그래프랑 비슷하게 ggplot이용

resident_gender_df <- data.frame(resident_gender)
###그룹은 박스플롯할때 특정그룹으로 묶어지는데 여기서는 fill자체에서 그룹화되어짐
ggplot(data = resident_gender_df, aes(x=Freq, y = Var2, fill = Var1, group = Var1)) +
  geom_bar(stat = "identity", position = 'dodge')


#모자이크플롯
mosaicplot(resident_gender, col = rainbow(2))


# 직업유형(job2) vs 나이(age2)
dataset$job2 <- factor(dataset$job2)
dataset$age2 <- factor(dataset$age2)
jobage <- table(dataset$job2, dataset$age2)

barplot(jobage, beside = T,legend = row.names(jobage), col = rainbow(3))

jobage_df <- data.frame(jobage)

ggplot(data = jobage_df, aes(x=Freq, y=Var2, fill = Var1, group = Var1)) + 
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()



# 숫자형 vs 범주형
# 직업유형에 따른 나이 비율

# 카테고리 유형별 시각화
install.packages("lattice")
library(lattice)
?densityplot
#density는 x축값 먼저정의하고 데이터정의, 근데 팩터형이어야함
densityplot(dataset$age2, dataset)
str(dataset)
#auto.key = T 로 범례 설정
densityplot(dataset$age, dataset, group = dataset$job2, auto.key = T)

ggplot(data = dataset, aes(x= age, fill = job2)) +
  geom_bar(width = .5, position = "dodge")



##실습3!!!
# 데이터 프레임의 복사본 생성하기
library(ggplot2)
midwest
midwest_raw <- data.frame(midwest)
midwest_new <- midwest_raw
str(midwest_new)
head(midwest_new)

# [문제]
# poptotal(전체인구) 변수를 total로, 
# popasian(아시안 인구) 변수를 asian으로 수정하세요.
midwest_new <- rename(midwest_new, "total" = "poptotal")
midwest_new <- rename(midwest_new, "asian" = "popasian")
library(dplyr)
# [문제]
# total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' percasian 파생변수를 만들고,
# 히스토그램을 만들어 도시들이 어떻게 분포하는지 살펴보세요.

midwest_new$"percasian2" <- (midwest_new$asian / midwest_new$total)*100
hist(midwest_new$percasian2) 


# [문제]
# 아시아 인구 백분율 전체 평균을 구하고, 
# 평균을 초과하면 "large", 
# 그 외에는 "small"을 부여하는 mean 파생변수를 만들어 보세요.
mean(midwest_new$percasian2)

midwest_new$mean_percasian2 <- ifelse(midwest_new$percasian2 > mean(midwest_new$percasian2),"large","small")  

# [문제]
# "large"와 "small"에 해당하는 지역이 얼마나 되는지 빈도표와 빈도 막대 그래프를 만들어 확인해 보세요.
densityplot(midwest_new$percasian2, midwest_new, group = midwest_new$country, auto.key = T)

ggplot(data = midwest_new, 
       aes(x=percasian2,y=county)) +
  geom_bar(stat = "identity",width = .1, position = "dodge") +
  geom_point() 



teacher <- as.data.frame(table(midwest_new$mean_percasian2))
ggplot(teacher, 
       aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity", width = .2)


# ggplot2의 midwest 데이터를 사용하여 데이터 분석을 실습하는 문제 입니다.
# popadults는 해당 지역의 성인 인구, poptotal은 전체 인구를 나타냅니다. 

# 1번 문제
# midwest 데이터에 '전체 인구 대비 미성년 인구 백분율' 변수를 추가하세요.

midwest_new$"전체 인구 대비 미성년 인구 백분율" <-  ((midwest_new$total - midwest_new$popadults) / midwest_new$total)*100

##강사님 답
midwest_new <- 	midwest_new %>% 
  mutate(percyouth = (poptotal - popadults)/poptotal * 100)

# 2번 문제
# 미성년 인구 백분율이 가장 높은 상위 5개 county(지역)의 미성년 인구 백분율을 출력하시오.
tail(sort(midwest_new$"전체 인구 대비 미성년 인구 백분율"), n=5)

##강사님 답
midwest_new %>% arrange(desc(percyouth)) %>% 
  dplyr::select(county, percyouth) %>% 
  head(5)


# 3번 문제
# 다음과 같은 분류표의 기준에 따라 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의 지역이 있는지 알아보세요.

# 분류     기준
# large    40%이상
# middle   30 ~ 40미만
# small    30미만

midwest_new$"미성년 비율 등급" <- ifelse("전체 인구 대비 미성년 인구 백분율" >= 40,"large",ifelse("전체 인구 대비 미성년 인구 백분율" >= 30,"middle","small"))

##강사님 답
midwest_new <- midwest_new %>% 
  mutate(gradeyouth = ifelse(percyouth >= 40 ,"large",
                             ifelse(percyouth >= 30,"middle","small")))
table(midwest_new$gradeyouth)


# 4번 문제
# popasian은 해당 지역의 아시아인 인구를 나타냅니다. 
# '전체 인구 대비 아시아인 인구 백분율' 변수를 추가하고 하위 10개 지역의 state(주), county(지역), 아시아인 인구 백분율을 출력하세요.

midwest_new$"전체 인구 대비 아시아인 인구 백분율" <-  (midwest_new$asian / midwest_new$total)*100
head(sort(midwest_new$state), n=10)
head(sort(midwest_new$county), n=10)
head(sort(midwest_new$"전체 인구 대비 아시아인 인구 백분율"), n=10)


##강사님 답
midwest_new <- midwest_new %>% 
  mutate(ratio_asian = popasian/poptotal*100)

midwest_new %>% arrange(ratio_asian) %>% 
  dplyr::select(state, county, ratio_asian) %>% 
  head(10)

# 시계열(time series)
# 변수간의 상관성

# iris 시계열 데이터 만들기
iris
seq <- as.integer( rownames(iris) )
?cbind
irisDF <- cbind(seq = as.integer(rownames(iris)),iris)

 # x축은 seq
 # y축은 -Species

 #랜덤하게 4색 추출
colsColor <- topo.colors(4,alpha = .4)
 #2열부터 5열까지 이름부여
names(colsColor) <- names(irisDF)[2:5]
 #melt함수 이용해서 기준seq, species
 #나머지 컬럼을 variable해서 wide -> long
irisDF
library(reshape2)

iris_melt <- melt(irisDF, id = c("seq","Species"))
 ##버젼떄문에 오류가 나면 id대신 id.vars로 표현

library(ggplot2)
g <- ggplot(data = iris_melt,
       aes(x=seq, y=value, col = variable)) +
  geom_line(cex = 0.8, show.legend = T)

 #추가적으로 선의 색상과 범례 라벨링
g <- g + scale_color_manual(
  name = "",
  values = colsColor[iris_melt$variable],
  labels = c("꽃받침 길이", "꽃받침 너비", "꽃잎 길이", "꽃잎 너비")
  )


# 날짜
# 문자변수를 날짜변수 변환

# R의 날짜 데이터 타입 "POSIXct"
# as.POSIXct()

str_date <- "200730 13:40"
as.POSIXct(str_date, format = "%y%m%d %H:%M")

#2020이라Y 20이면y
str_date <- "2020-07-30 13:40:01 PM"
as.POSIXct(str_date, format = "%Y-%m-%d %H:%M:%S")

str_date <- "07/30/20 13:40:01"
as.POSIXct(str_date, format = "%m/%d/%y %H:%M:%S")



cospi <- read.csv(file.choose())
#날짜별 주가 만들기(시간이 있어서 따로 시계열 만들필요 없음)
cospi_test <- cospi
cospi_melt <- melt(cospi_test, id = c("Date","Volume"))
ggplot(data = cospi_melt, aes(x= Date,y=value,col = variable, group = variable)) +
  geom_line(cex = 0.5, show.legend = T)

##실습4!!!
spanish_train <- read.csv(file.choose())
str(spanish_train)
# 1.
# 데이터 내에 결측치 여부를 확인한다. 
# NA값이 310681개 있는 것을 확인할 수 있다.

 #결측치 확인방법
spanish_train[!complete.cases(spanish_train),]
str(spanish_train[!complete.cases(spanish_train),])

 #결측치 제거
test_renfe2 <- spanish_train[complete.cases(spanish_train),]
str(test_renfe2)
# 2.
# filter와 !is.na함수를 통해 결측치를 모두 제거했다.


# 3.
# 마드리드 출발
# 마드리드에서 출발하는 열차 데이터만을 떼어내 madrid_origin이라는 변수로 저장하고 
# 우선, 마드리드에서 출발하는 열차 데이터만을 이용해 비교해보기로 한다.
madrid_origin <- filter(test_renfe2, test_renfe2$origin == "MADRID")
str(madrid_origin)

# 4.
# summary함수를 통해 일반적 데이터 정보를 다시 확인한다.
summary(test_renfe2)

# 5.
# 마드리드 출발 열차의 빈도 수
# 마드리드를 출발하는 기차의 도착 도시별 운행빈도 수를 바형태로 나타내보자
 ggplot(data = madrid_origin, 
 aes(x=destination,fill=origin)) +
 geom_bar(width = .5, position = "dodge")


# 6.
# 마드리드발 도착지별 가격 박스플롯으로 티켓가격의 높은 순을 확인해보자
boxplot(price ~ destination, data=madrid_origin)
str(madrid_origin)

# 7.
# AVE의좌석 등급별 가격박스플롯이 시각화 
# 똑같은 열차와 똑같은 좌석등급, 똑같은 도착지라 하더라도 가격이 차이가 나는 것을 확인할 수 있다.
AVE_MADRID <- filter(madrid_origin, madrid_origin$train_type == "AVE")
boxplot(price ~ destination,data= AVE_MADRID)

# 8. 
# 이 차이를 이해하고 싶어 시계열로 데이터를 만들어보았다.


# 9.
#날짜 데이터 변환. as.POSIXct는 factor형식의 날짜 사용가능


# 10.
# 컬럼이름지정
# colnames(a_b) <- c("preferente","Turista")


# 11.
# 도착지별, 트레인 클래스별로 가격을 박스플롯형태로 나타낼 수도 있다.

