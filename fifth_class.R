# ddply(데이터, .(그룹지을 변수명이나 처리 조건), 처리함수) , 데이터프레임을 받아서 데이터프레임으로 리턴
?ddply
library(plyr)
# iris 데이터에서 종별 Sepal.Length 평균을 계산한다면?
# 처리조건으로 Sepal.Length >= 4.0 추가한다면?
iris

ddply(iris,
      .(Species),
      function(x){
        data.frame(sepal.length.mean = mean(x$Sepal.Length))
      }
      )

ddply(iris,
      .(Species, Sepal.Length >= 4.0),
      function(x){
      data.frame(sepal.length.mean = mean(x$Sepal.Length))
      }
      )




data(baseball)
baseball
head(baseball)
str(baseball)
names(baseball)

#id가 ansonca01인 선수의 데이터만 확인하고 싶다면??
filter(baseball, id == "ansonca01")
subset(baseball, id == "ansonca01")

#ddply()함수를 이용하여 각 선수별 출전한 게임수의 평균을 구한다면?
ddply(baseball,
      .(id),
      function(x){
      data.frame(g.mean = mean(x$g))  
      }
      )

#각 선수별 최대 게임을 플레이한 해의 기록을 구한다면?
ddply(baseball,
      .(id),
      subset,
      g == max(g))           

# reshape 패키지 
# 변환
# melt(컬럼을 로우로 바꿈), cast(dcast, acast) 동일한결과를 데이터프레임으로 리턴하느냐 어레이로 리턴하느냐

# melt(데이터를 구분하는 식별자, 측정대상변수, 측정치)
install.packages("reshape2")
library(reshape2)
data(french_fries)
french_fries
head(french_fries)
str(french_fries)


fries_melt <- melt(id=1:4, french_fries)    
head(fries_melt,20)                            
tail(fries_melt,20)


#cast()
fries_d <- dcast(fries_melt, time + treatment + subject + rep ~ variable)
fries_d <- dcast(fries_melt, time + treatment + subject + rep ~ ...)

#data.table 패키지
library(data.table)
install.packages("data.table")

iris_table <- data.table(iris)
iris_table

iris_table[1,]
iris_table[iris_table$Species == "setosa",]
 #임의의 2개의 피쳐만 출력한다면?
iris_table[1,c(1,2)]
iris_table[1,c(Sepal.Length, Sepal.Width)]
iris_table[1, list(Sepal.Length, Species)]
iris_table[51, c(Sepal.Length, Species)]

nlevels(iris$Species)
levels(iris$Species)

# iris데이터에서 Sepal.Length의 평균값을 종별로 구한다면?
iris_table[ , mean(Sepal.Length), Species]
aggregate(iris$Sepal.Length,list(iris$Species),mean)



###데이터가공
#외부파일 읽어들이는 패키지 readxl
install.packages("readxl")
library("readxl")

#read_excel(), read.table(), read.csv()
excel_data_sample <- read_excel(file.choose())
View(excel_data_sample) 

#txt
#read.table
#option
#header
#skip
#nrows
#sep
#col.names = c(컬럼이름)

#헤더유무 입력
txt_data_sample <- read.table(file.choose(), header = T)
txt_data_sample

#콤마입력
# service_data_tap_ex1.txt
txt_data_sample02 <- read.table(file.choose(), header = T, sep = ",")
txt_data_sample02

#열이름설정
# service_data_tap_ex2.txt
colNames <- c("ID","SEX","AGE","AREA")
txt_data_sample03 <- read.table(file.choose(), header = T, sep = ",", col.names = colNames)
txt_data_sample03


# service_data_excel_sample.xlsx
excel_data_sample <- read_excel(file.choose())
class(excel_data_sample)
str(excel_data_sample)

excel_data_sample$SEX <- factor(excel_data_sample$SEX)
excel_data_sample$AREA <- factor(excel_data_sample$AREA)                            

levels(excel_data_sample$SEX)
levels(excel_data_sample$AREA)
str(excel_data_sample)

#성별에 따른 17_AMT 평균이용 금액을 확인하고 싶다면?
aggregate(excel_data_sample$AMT17,list(excel_data_sample$SEX),mean)

##강사님답
excel_data_sample %>%
  group_by(SEX) %>%
  dplyr::summarise(cnt = n(), mean = mean(AMT17))

sapply(split(excel_data_sample$AMT17, excel_data_sample$SEX),mean,na.rm = TRUE)

#지역에 따른 Y17_CNT 이용건수의 합을 확인하고 싶다면?
aggregate(excel_data_sample$Y17_CNT,list(excel_data_sample$AREA),sum)


str(excel_data_sample)
#names는 일정한 순서가 나오는데 ls는 일정한 순서가 없음
class(names(excel_data_sample))
class(ls(excel_data_sample))

#변수명 변경하기
#dplyr::rename()
str(excel_data_sample)
colRename <- rename(excel_data_sample, Y17_AMT = AMT17, Y16_AMT = AMT16)

#파생변수 : 기존변수조합해서 새로운 변수만들어낸것 ,가변수: 변수를 0이나 1로 임의로 변환한것
# 파생변수
colRename$AMT <- colRename$Y16_AMT + colRename$Y17_AMT
colRename$CNT <- colRename$Y16_CNT + colRename$Y17_CNT


#실습1!
# 1.colRename 데이터세트에서 ID변수만 추출
select(colRename,ID)
# 2.colRename 데이터세트에서 ID, AREA, Y17_CNT 변수 추출
select(colRename,ID,AREA,Y17_CNT)
# 3.colRename 데이터세트에서 AREA 변수만 제외하고 추출
select(colRename,-AREA)
# 4.colRename 데이터세트에서 AREA,Y17_CNT변수를 제외하고 추출
select(colRename,-AREA,-Y17_CNT)
# 5.colRename 데이터세트에 AREA(지역)가 서울인 경우만 추출
subset(colRename,AREA == "서울") 
# 6.colRename 데이터 세트에서 AREA(지역)가 서울이면서 Y17_CNT(17년 이용건수)가 10건 이상인 경우 추출
subset(colRename,AREA == "서울" & Y17_CNT >= 10)
# 7.colRename 데이터세트의 AGE 변수를 오름차순 정렬
arrange(colRename,AGE)
# 8.colRename 데이터세트의 Y17_AMT변수를 내림차순 정렬
arrange(colRename,desc(Y17_AMT))
 #정렬중첩
# 9.colRename 데이터세트의 AGE변수는 오름차순, Y17_AMT 변수는 내림차순 정렬해서 데이터요약하기
arrange(colRename,AGE,desc(Y17_AMT))
#10.colRename 데이터세트의 Y17_AMT(17년 이용금액) 변수값 합계를 TOT_Y17_AMT변수로 도출
TOT_Y17_AMT <- sum(colRename$Y17_AMT)
#11.colRename 데이터세트의 AREA(지역) 변수값별로 Y17_AMT(17년 이용금액)을 더해 SUM_Y17_AMT 변수로 도출
SUM_Y17_AMT <- aggregate(colRename$Y17_AMT,list(colRename$AREA),sum)
#12.colRename 데이터세트의 AMT를 CNT로 나눈값을 colRename데이터 세트의 AVG_AMT로 생성
AVG_AMT <- colRename$AMT / colRename$CNT
#13.colRename 데이터세트에서 AGE변수값이 50이상이면 "Y", 50미만이면 "N"값으로 colRename 데이터세트에 AGE50_YN 변수 생성
AGE50_YN <- ifelse(colRename$AGE >= 50,"Y","N")
 #나이분류
#14.colRename 데이터세트의 AGE값이 50이상이면 "50++", 40이상이면 "4049", 30이상이면 "3039", 20이상이면 "2029", 나머지는 "0019"를 값으로 하는 AGE_GR10변수생성
AGE_GR10 <- ifelse(colRename$AGE >= 50, "50++",ifelse(colRename$AGE >= 40, "4049",ifelse(colRename$AGE >= 30,"3039",ifelse(colRename$AGE >= 20,"2029","0019"))))




#데이터 결합
library(readxl)
#service_data_excel_m_history.xlsx
male_hist <- read_excel(file.choose())

#service_data_excel_f_history.xlsx
female_hist <- read_excel(file.choose())

#세로결합
#변수명 기준으로 결합,피쳐들이 동일해야함
#bind_rows()
m_f_bind_join <- bind_rows(male_hist,female_hist)

#가로결합
#left_join :  지정한변수와 데이터세트1을 기준으로 데이터세트2에 있는 나머지 변수 결합 
#inner_join : 데이터세트1과 데이터세트2에서 기준으로 지정한 변수값이 동일할 때만 결합
#full_join :  전체를 결합
#피쳐들이 다를 수 있음

#service_data_jeju_y17_history.xlsx
#service_data_jeju_y16_history.xlsx
#위 두개의 파일을 읽어보자
jeju_y17 <- read_excel(file.choose())
jeju_y16 <- read_excel(file.choose())

# ID를 기준으로 jeju_y17_history 데이터 세트를 기준으로 결합
?left_join
bind_left <- left_join(jeju_y17,jeju_y16,by = "ID")

bind_inner <- inner_join(jeju_y17,jeju_y16,by = "ID")

bind_full <- full_join(jeju_y17, jeju_y16, by = "ID")

bind_right <- right_join(jeju_y17, jeju_y16, by = "ID")

# service_data_excel_sample.xlsx
sample_excel <- read_excel(file.choose())

# 특정 값이 얼마나 반복되는지 분석(빈도 분석)하고 싶다면?
# 시각화 유무는 plot = T,F로 입력구분
# main은 타이틀
descr::freq()
install.packages("descr")
library(descr)

freqArea <- freq(sample_excel$AREA, plot = T,main = "지역별 빈도")

#성별에 따른 빈도분석을 한다면?
freqGender <- freq(sample_excel$SEX, plot = T,main = "성별 빈도")


# 시각화
# 변수구분(이산 vs 연속)

# 이산형 변수 : 변수가 가질 수 있는 값이 끊어진 변수
# -명목변수
# -순위변수
# 막대, 점, 파이

char_data <- c(380,520,330,390,320,460,300,405)
names(char_data) <- c("2018 1Q","2018 2Q","2018 3Q","2018 4Q","2019 1Q","2019 2Q","2019 3Q","2019 4Q")
range(char_data)
max(char_data)
length(char_data)

 #막대차트::barplot()
 barplot(char_data)
 barplot(char_data, ylim = c(0,600), col = rainbow(2), main = "2018 vs 2019 분기매출")  ##가로막대
 barplot(char_data, xlim = c(0,600), col = rainbow(2), main = "2018 vs 2019 분기매출", horiz = T)  ##세로막대 
 barplot(char_data, xlim = c(0,600), col = rainbow(2), main = "2018 vs 2019 분기매출", horiz = T, ylab = "연도별 분기", xlab = "매출액")
 
 # dot chart
 ?dotchar
 dotchart(char_data)
 dotchart(char_data, color = c("green","red"), xlab = "매출액", ylab = "년도별 분기", pch = 1:2, cex = 0.8, lcolor = "blue")
 
 # pie chart
 pie(char_data)
 par(mfrow=c(2,2))   #차트를 한 화면에 나타낼때 사용
 pie(char_data, border = 'blue', col = rainbow(8))

 
#iris species는 수치형이 아니라 테이블을 이용해 구현
data(iris)  
table(iris$Species)
class(table(iris$Species))
pie(table(iris$Species))

# 연속변수(상자그래프,히스토그램,산점도)
# 상관분석 : 상관변수간 관계를 분석하는 것, 회귀분석 : 독립-종속변수간 유의도를 분석하는 것
# = 변수가 연속된 구간을 갖는다는 뜻
# 간격변수, 비율변수
# boxplot(), hist(), plot()

# 시각화
# VADeaths
data(VADeaths)
str(VADeaths)
summary(VADeaths)
 
 #이상치의 값을 찾거나 표준화,정규화할때 박스플롯사용
 boxplot(VADeaths)

 # attach(), detach()
 attach(iris)
 mean(Sepal.Length)
 detach(iris) 
 mean(Sepal.Length)
 
 #히스토 그램
 data(iris)
 iris 
 summary(iris) 
 hist(iris$Sepal.Length) 
 hist(iris$Sepal.Length, xlab = "꽃받침 길이", col = "green", main = "iris SL", xlim = c(4.0, 8.0)) 
 hist(iris$Sepal.Width, xlab = "꽃받침 넓이", col = "Yellow", main = "iris SW", xlim = c(2.0, 4.0), freq = F)
 lines(density(iris$Sepal.Width), col = "red")

 #산점도(plot)
 x <- runif(5,min = 0, max = 1)
 y <- x^2
 plot(x, y)
 
 price <- runif(10, min = 2, max = 8)
 plot(price, type = "l")
 plot(price, type = "o")
 plot(price, type = "h")
 plot(price, type = "s")
 
 #pch는 1~25까지
 plot(price, type = "o", pch = 25)
 
 #iris - scatter matrix(산점도 매트릭스)
 #pairs()
 iris
 pairs(iris[1:4])
 
 #3차원 산점도
 install.packages("scatterplot3d")
 library(scatterplot3d)
 
 #종별로 분류해서 변수에 담아보세요
 iris_setosa <- filter(iris,iris$Species == "setosa")
 iris_versicolor <- filter(iris,iris$Species == "versicolor")
 iris_virginica <- filter(iris,iris$Species == "virginica")
  
 iris3D <- scatterplot3d(iris$Petal.Length, iris$Sepal.Length, iris$Sepal.Width, type = 'n')
 iris3D$points3d(iris_setosa$Petal.Length, 
                 iris_setosa$Sepal.Length, 
                 iris_setosa$Sepal.Width, bg = "green", pch = 21) 
 
 iris3D$points3d(iris_versicolor$Petal.Length, 
                 iris_versicolor$Sepal.Length, 
                 iris_versicolor$Sepal.Width, bg = "orange", pch = 21)
 
 iris3D$points3d(iris_virginica$Petal.Length, 
                 iris_virginica$Sepal.Length, 
                 iris_virginica$Sepal.Width, bg = "blue", pch = 21)
 