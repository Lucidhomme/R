# 시각화 알아보기
install.packages("mlbench")
library(mlbench)

data(Ozone)
str(Ozone)
?Ozone

# 산점도
head(Ozone)

# 숫자형 데이터인경우 산점도 
plot(Ozone$V8, Ozone$V9)

# 축 이름(xlab, ylab)
plot(Ozone$V8, Ozone$V9, xlab = "Sandburg Temp", ylab = "EI Temp")

# title(main)
plot(Ozone$V8, Ozone$V9, xlab = "Sandburg Temp", ylab = "EI Temp", main = "Region Ozone")

# 점의 종류(pch)
plot(Ozone$V8, Ozone$V9, xlab = "Sandburg Temp", ylab = "EI Temp", pch = "+")

# 점 크기(cex)
plot(Ozone$V8, Ozone$V9, xlab = "Sandburg Temp", ylab = "EI Temp", pch = "+", cex = .5)

# 색상(col)
??color
plot(Ozone$V8, Ozone$V9, xlab = "Sandburg Temp", ylab = "EI Temp", pch = "+", cex = .5, col = "blue")

# 좌표축의 범위(xlim, ylim)
plot(Ozone$V8, Ozone$V9, xlab = "Sandburg Temp", ylab = "EI Temp", pch = "+", cex = .5, col = "blue", xlim = c(20,100), ylim = c(30,80))
range(Ozone$V8, na.rm = T)
range(Ozone$V9, na.rm = T)

#jitter() : 같은값을 가지는 데이터들을 조금 움직여 여러번 겹치는것을 방지
plot(jitter(Ozone$V6),jitter(Ozone$V7))

data(cars)
str(cars)
head(cars)
?cars

# type 
plot(cars$speed, cars$dist)
plot(cars, type = "l")
plot(cars, type = "o")

# 직선(abline)
# b0 : 절편, b1 : 기울기
# y = b0 + b1x + e

# dist = -5 + 3.5 * speed
abline(a = -5, b = 3.5, col = "red")

# 그래프에서 speed와 dist 평균까지 abline 표시한다면?
abline(h=mean(cars$dist) , lty=2, col = "blue")
abline(v=mean(cars$speed) , lty=2, col = "green")

# 선형회귀(lm)
# lm(종속변수 ~ 독립변수, data = )

# dist = -5 + 3.5*speed
# dist = -17.579 + 3.932 * 4
# 스피드가 4일때 식에 넣어보면 실제 distance는 -1.851로 데이터상의 2와는 잔차가 발생
abs(-1.851)
head(cars)
car_model <- lm(dist ~ speed, data = cars)
abline(car_model)

# 점(points)
# iris, Sepal.Width, Sepal.Length -> plot
data(iris)
plot(iris$Sepal.Width, iris$Sepal.Length, main = "iris", xlab = "width", ylab = "length", pch=20, cex=.5)
points(iris$Petal.Width, iris$Petal.Length, cex = .5, pch = "+", col = "#FF0000")

# 2개의 데이터들을 한꺼번에 with함수로 시각화
# legend() : 범례
with(iris, 
    {
      plot(Sepal.Width, Sepal.Length, main = "iris", xlab = "width", ylab = "length", pch=20, cex=.5)
      points(Petal.Width, Petal.Length, cex = .5, pch = "+", col = "#FF0000")
      legend("topright", legend = c("Sepal", "petal"),
             pch = c(5,7),
             cex = .8,
             col = c("black", "red"),
             bg = "gray")
      }
    )

# boxplot
summary(iris$Sepal.Width)
boxplot(iris$Sepal.Width)

# IQR(3사분위수 - 1사분위수)
# 1사분위수 2.8
# 3사분위수 3.3
# 중위수 3.0
# whisker 값을 계산한다면
# Q1 - 1.5 *IQR(lower whisker)
# Q3 + 1.5 *IQR(upper whisker)
2.8 - 1.5*(3.3-2.8)
3.3 + 1.5*(3.3-2.8)

# 여기서 벗어나는 값을 아웃라이어(이상치)라고 한다

boxplotStats <- boxplot(iris$Sepal.Width,horizontal = T)
boxplotStats
# 이상치의 값을 텍스트로 뿌려라
text(boxplotStats$out,
     rep(1, NROW(boxplotStats$out)),
     pos = 1, cex = .5)


# iris의 setosa종과 versicolor 종의
# Sepal.Width에 대한 상자 그림을 그려보자

# 팩터를 2개로 변환한 다음 박스플롯으로 구성
sw <- subset(iris, Species == "setosa" | Species == "versicolor")
str(sw)
sw$Species <- factor(sw$Species)
levels(sw$Species)

boxplot(Sepal.Width ~ Species, data = sw)

# hist(빈도수 기반, 밀도 기반)
# service_data_visualization_region_weather.csv
weather <- read.csv(file.choose())
str(weather)
head(weather)
avgTemp <- weather$평균기온
hist(avgTemp)
hist(avgTemp, breaks = 24)



# ggplot
# - ggplot() : 틀
# - geom_그래프 계열
# - geom_도형 계열
# - coord_계열, labs : 데코레이션 

library(ggplot2)
library(dplyr)


# ggplot 필수함수로 데이터와 축 지정
iris

# geom_point()
# geom_line()
# geom_boxplot()
# geom_historgram()
# geom_bar()

# point()
# 종에따라 색상, 모양, 크기를 다르게 한다면?
ggplot(data = iris,
       aes(x = Sepal.Length, 
           y = Sepal.Width)) +
      geom_point(pch = c(2,4,6)[iris$Species],
                 size = c(.8,1,1.2)[iris$Species],
                 col = c('red','blue','black')[iris$Species])

# 도형계열
# Species 별 Sepal.Length, Sepal.Width 최대, 최소값 구하라
library(plyr)
aesXY <- ddply(iris, .(Species), summarise,
               min_x = min(Sepal.Length),
               max_x = max(Sepal.Length),
               min_y = min(Sepal.Width),
               max_y = max(Sepal.Width))

aesX_start <- max(aesXY$min_x)
aesX_end   <- min(aesXY$max_x)
aesY_start <- max(aesXY$min_y)
aesY_end   <- min(aesXY$max_y)

# annotate()
irisG <- ggplot(data = iris, 
                aes(x = Sepal.Length,
                    y = Sepal.Width)) +
  geom_point(pch = c(2,4,6)[iris$Species],
             size = c(.8,1,1.2)[iris$Species],
             col = c('red','blue','black')[iris$Species])

irisG + 
  annotate(geom = "rect",
           xmin = aesX_start,
           ymin = aesY_start,
           xmax = aesX_end,
           ymax = aesY_end,
           fill = 'red',
           alpha = .1,
           col = 'black',
           lty = 2)


# 외부옵션을 활용한 - coord_계열, labs
# 축 변환 - coord_flip()
# 축 범위 - coord_cartesian()
irisG
irisG + coord_flip()

# 라벨링 -labs()
irisG + labs(title = "제목",
             x = "x 이름",
             y = "y 이름")

# ggplot
sampleDF <- data.frame(
  years = c(2015, 2016, 2017, 2018, 2019, 2020),
  gdp   = c(300, 350, 400, 450, 500, 550)
)

# 1. 틀 생성
ggplot(data = sampleDF,
       aes(x = years, y = gdp)) +
    geom_point() +
    geom_line(linetype = "dashed")


install.packages("gcookbook")
library(gcookbook)
data(uspopage)
str(uspopage)
head(uspopage)

# year, thousands 가지고 기본적 ggplot() 만들어보세요
ggplot(data = uspopage,
       aes(x = Year, y = Thousands, fill = AgeGroup)) +
  geom_area(alpha = .7, col = 'black')

# geom_bar(), 보통 축이 1개일때 사용하고 축이 2개일때는 geom_col()사용(축이 여러개일때도 geom_bar를 사용하게 하는 방법은 있다)
korMovies <- c('강철비2', '반도', '그놈이그놈이다', '킹덤','살아있다')
cntMovies <- c(5, 11, 3, 34, 23)

moviesDF <- data.frame(moviesName = korMovies , 
                       moviesCnt = cntMovies)
ggplot(data = moviesDF,
       aes(x = moviesName, y = moviesCnt)) +
  geom_col(col = 'black', width = .4, fill = 'orange') 


library(MASS)
str(Cars93)
head(Cars93)

ggplot(data = Cars93,
       aes(x = Type)) +
  geom_bar(fill = 'yellow', col = 'black') +
  ggtitle("Bar Chart by Type")


#SQL형식으로 테이블을 불러올 수도 있음
install.packages("sqldf")
library("sqldf")
# 자동차의 유형별로 집계(count())해서 막대그래프를 표현한다면?
type_cnt <- sqldf('select type, count(*) as cnt
                  from Cars93
                  group by Type
                  order by Type')

#축 여러개를 1개인 것처럼 geom_bar 이용가능(stat = 'identity'이용)
ggplot(data = type_cnt,
       aes(x = Type, y = cnt)) +
  geom_bar(stat = 'identity', fill = 'blue', col='black') +
  ggtitle("Bar chart by CarType")



#기준값이 여러개인데 그래프를 만들어야 한다면?
maleStu   <- c(20,35,30,35,27,35)
femaleStu <- c(25,30,32,29,32,29)
classNum  <- c(1,2,3,4,5,6)

stuDF <- data.frame(maleStu, femaleStu, classNum)

library(reshape2)
stuMelt <- melt(stuDF, id="classNum")

ggplot(data = stuMelt,
       aes(x = classNum, y = value, fill = variable)) +
  geom_bar(stat = 'identity', width = .4)

# multi bar    
ggplot(data = stuMelt,
       aes(x = classNum, y = value, fill = variable)) +
  geom_bar(stat = 'identity', width = .4, position = position_dodge(width=.5))


# Cars93 데이터를 이용하여 차종(Type)별 제조국(Origin)별 자동차 수를 가지고 막대그래프를 그려보시오
ggplot(Cars93,aes(x=Type, fill=Origin)) +
  geom_bar(position=position_dodge(width=.5),
           col="black") +
  ggtitle("Bar Chart of Frequency by Car Type & Origin")




#워드클라우드 그리기
install.packages("wordcloud2")
library(wordcloud2)

head(demoFreq,10)
wordcloud2(demoFreq,color = 'random-light',
           backgroundColor = 'black')

install.packages("webshot")
install.packages("htmlwidgets")
library(webshot)
library(htmlwidgets)

wImg <- wordcloud2(demoFreq,
                   color = 'random-light',
                   backgroundColor = 'black')
saveWidget(wImg, "wImg.html", selfcontained = F)


webshot("wImg.html",
        "wImg.pdf",
        vwidth = 480,
        vheight = 480,
        delay = 5)
webshot::install_phantomjs()



# 시각화 실습
# data - airquality
?airquality
str(airquality)
head(airquality)


# 피처의 이름을 소문자로 변경
names(airquality) <- c("ozone","solar.r","wind","temp","month","day")

# 1. x축은 day, y축은 temp를 만들기
ggplot(data = airquality,
       aes(x=day, y=temp))

# 2. 만들어진 틀에 산점도 그리기
ggplot(data = airquality,
       aes(x=day, y=temp)) +
  geom_point()

# 3. 크기를 3, 색상을 빨간색으로 적용한 산점도 그리기
ggplot(data = airquality,
       aes(x=day, y=temp)) +
  geom_point(size = 3, col = 'red')

# 4. 만들어진 틀에 꺾은선 그래프와 산점도 그리기
ggplot(data = airquality,
       aes(x=day, y=temp)) +
  geom_point(size = 3, col = 'red') +
  geom_line()

# 5. 꺽은선 그래프 색상을 빨간색과 산점도 크기를 3으로 변경하고 겹쳐 그리기
ggplot(data = airquality,
       aes(x=day, y=temp)) +
  geom_point(size = 3, col = 'red') +
  geom_line(col = 'red')

# 6. day열을 그룹지어, 날짜별 온도 상자그림(boxplot) 그리기
library(dplyr)
ss <- group_by(airquality,day)
str(ss)
boxplot(temp ~ day, data = ss)

# 7. temp 히스토그램 그리기
hist(airquality$temp)
 
 # 아래부터는 mtcar 데이터 이용
mtcars
str(mtcars)
head(mtcars)
# 1. cyl 종류별 빈도수 확인 geom_bar
cyl_cnt <- sqldf('select cyl, count(*) as cnt
                  from mtcars
                  group by cyl
                  order by cyl')
ggplot(data = cyl_cnt,
       aes(x = cyl, y = cnt)) +
  geom_bar(stat = 'identity', width=.4, fill = 'red') +
  ggtitle("cyl 종류별 빈도수")

# 2. 빈 범주를 제외하고 cyl 종류별 빈도수 확인
ggplot(data = cyl_cnt,
       aes(x = factor(cyl), y = cnt)) +
  geom_bar(stat = 'identity', width=.4, fill = 'green') +
  ggtitle("cyl 종류별 빈도수")

# 3. 원 그래프 그리기
pie(cyl_cnt$cyl)

# 4. cyl 종류별 gear빈도 누적 막대 그래프
ggplot(mtcars,aes(x=factor(cyl))) +
  geom_bar(aes(fill = factor(gear))) +
  ggtitle("cyl종류별 gear빈도 누적")

# 5. cyl열을 x축으로 지정하여 cyl별 gear빈도 파악 선버스트 차트 그리기
ggplot(mtcars,aes(x=factor(cyl))) +
  geom_bar(aes(fill = factor(gear))) +
  ggtitle("cyl종류별 gear빈도 누적") +
  coord_polar()

# 6. cyl열을 x축으로 지정하여 cyl별 gear빈도 파악 원그래프 그리기
ggplot(mtcars,aes(x=factor(cyl))) +
  geom_bar(aes(fill = factor(gear))) +
  ggtitle("cyl종류별 gear빈도 누적") +
  coord_polar(theta = "y")
