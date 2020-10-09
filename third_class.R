# lapply()
# sapply()

iris
#list형식
class(lapply(iris[,1:4],mean))
#numeric형식
class(sapply(iris[,1:4],mean))
#데이터 프레임으로 변환
irisVec <- sapply(iris[,1:4],mean)
class(as.data.frame(irisVec))



iris[1,1] = NA
head(iris)

#데이터 처리 중 특정값을 평균이나 중위값으로 그냥 처리하고 싶을때는
#먼저 특정조건((ex)species에따라 분류)확인하여 값을 구한다

# split(feature,분류기준)
# 데이터를 트레이닝과 구분할때 사용하는데 자주 사용,
#ifelse구문의 의미 : true면 콤마 다음값 , false면 sepal.length 반환
#일단 종별로 구분
split(iris$Sepal.Length, iris$Species)
#종별 중위값을 확인,# na.ra = T (결측값을 TRUE를 주면 제거하고 계산)
sapply(split(iris$Sepal.Length, iris$Species),
       median, na.rm = T)

irissplit <- sapply(split(iris$Sepal.Length, iris$Species),median,na.rm =T)
class(irissplit)
irissplit[iris$species]

#결측값 NA를 중위수로 바꾸겠다
irisSplit <- within(iris,
                    {
                     Sepal.Length <- ifelse(is.na(Sepal.Length),irissplit[iris$Species],Sepal.Length)
                    })


#강사님답안
irisSlMedian <- sapply(split(iris$Sepal.Length, iris$Species),median, na.rm = T)
class(irisSlMedian)
irisSlMedian[iris$Species]
iris <- within(iris,
               {
                 sepal.Length <- ifelse(is.na(Sepal.Length),
                                        irisSlMedian[iris$Species],Sepal.Length)
               })



#subset(리턴타입은 데이터 프레임,자주사용됨)

x <- 1:5
y <- 6:10
z <- letters[1:5]
exampleDF <- data.frame(x,y,z)
str(exampleDF)

# x의 값이 3이상인 결과를 새로운 데이터 프레임으로 만들어 보자
subDF01 <- subset(exampleDF, x >= 3)
subDF01
# y의 값이 8이하인 결과를 새로운 데이터 프레임으로 만들어 보자
subDF02 <- subset(exampleDF, y <= 8 )
# x의 값이 2이상이고 y의 값이 8이하인 결과를 새로운 데이터 프레임으로 만들어 보자
subDF03 <- subset(exampleDF, x>=2 & y <= 8)
subDF03

#subset() 컬럼선택가능
subDF04 <- subset(exampleDF, select=c(x,y))
subDF04 <- subset(exampleDF, x >=3 ,select=c(x,y))

iris
str(iris)
names(iris)

# Petal.Length 평균구하기
mean(iris$Petal.Length)

#Sepal.Length , Petal.Length , Species 3개 컬럼만 가져오는 데이터 프레임 만들면서
#새로생성한 프레임에 조건으로 Petal.Length가 평균이상인 데이터만 조회
newiris <- subset(iris, Petal.Length >= mean(iris$Petal.Length),select=c(Sepal.Length, Petal.Length, Species))
newiris
str(newiris)


# Factor (오라클의 체크제약조건과 비슷)
# 범주형 변수
gender <- factor("m", c("m","f"))
#레벨의 개수확인
nlevels(gender)
#범주형 변수의 레벨값들
levels(gender)[1]
levels(gender)[2]

blood.type <- factor(c("A","A","AB","O","B"))
blood.type[6] <- "D"
str(blood.type)
is.factor(blood.type)  

#캐릭터 타입을 FACTOR범주형으로 변환(집단은 오름차순으로 정렬) 
lettersVec <- c("a","b","c","c","a","a")
class(lettersVec)
lettersVec.fac <- as.factor(lettersVec)

#팩터 생성하는 법( lettervec에서 레벨과 라벨값을 명시)
lettersVec.fac <- factor(lettersVec, 
                         levels = c("a","b","c"), 
                         labels = c("best", "middle", "low"))
lettersVec.fac
levels(lettersVec.fac)



id <- c(1,2,3,4,5)
gender <- c("F","M","F","M","F")
data <- data.frame(id,gender)
data <- data.frame(idx = id, gender = gender)
str(data)
#젠더를 팩터로 변경
data$gender <- factor(data$gender)
str(data)

#레벨을 변경
levels(data$gender) <- c("female", 'male')
str(data)

#그냥 평균
mean()
x <- c(15, 18, 2, 36, 12, 78, 5, 6, 8)
mean(x)
#group by 통해서 그룹별 산술평균을 구하기  
height <- c(180, 165, 172, 165, 177, 162, 181, 175, 190)
gender <- c("M", "F", "M", "F", "M", "F", "M", "F", "M")

height_gender_frm <- data.frame(height, gender)


# aggregate 
# 성별로 키의 평균을 구한다면?
height_gender_frm$height
height_gender_frm$gender
 # aggregate사용시, 데이터 타입이 리스트가 되어야해서 리스트로 변경
gendergroupmean <- aggregate(height_gender_frm$height,
                             list(height_gender_frm$gender),
                             mean)
str(height_gender_frm)
# 예제 데이터 mtcars
mtcars
str(mtcars)
head(mtcars)

# cyl 컬럼을 기준으로 나머지 컬럼의 평균값 구하기
aggregate(mtcars,list(mtcars$cyl),mean)
aggregate(mtcars, list(cylStandard = mtcars$cyl),mean)
# disp 컬럼이 120이상인 조건 추가
aggregate(mtcars,
          list(cylStandard = mtcars$cyl,
               dispHigh = mtcars[,'disp']>=120),mean)

aggregate(mtcars,list(mtcars$cyl,dispHigh = mtcars[,'disp']>=120),mean) 


# cyl 컬럼을 기준으로 wt 컬럼의 평균만 구하기
aggregate(mtcars$wt, list(mtcars$cyl),mean)
aggregate(mtcars[,6], list(cyl = mtcars$cyl),mean)
aggregate(wt ~ cyl, data = mtcars, mean)


#carb, gear 컬럼 두가지를 기준으로 wt 평균 구하기
aggregate(wt ~ carb + gear, data = mtcars, mean)
aggregate(mtcars$wt,list(mtcars$carb, mtcars$gear) ,mean)
#gear 기준으로 disp, wt 평균 구하기
  #aggregate(wt + disp ~ gear, data = mtcars, mean) (X), wt + disp 식으로 못쓰나봄
aggregate(cbind(disp, wt) ~ gear,data = mtcars,mean)
  #aggregate(do.call(mtcars$wt,mtcars$disp),list(mtcars$gear),mean) (X) disp가 리스트가 아님
aggregate(cbind(mtcars$disp,mtcars$wt),list(mtcars$gear),mean)
#carb, gear 컬럼 기준으로 disp, wt 평균 구하기
aggregate(cbind(disp, wt)~ carb + gear,data=mtcars,mean)
aggregate(cbind(mtcars$disp,mtcars$wt),list(mtcars$carb,mtcars$gear),mean)
#cyl제외한 다른 모든 컬럼을 기준으로 cyl의 평균을 구하기
aggregate(cyl ~ . , data = mtcars, mean)
## aggregate(mtcars$cyl,list(-(mtcars$cyl)),mean)  왜 안될까???

# tapply(데이터, 색인, 함수)
# 색인 : 범주형(factor)
# 주로 그룹지을때 사용

tapply(1:10, rep(1,10), sum)
#짝수 홀수 별로 그룹되어 리턴
tapply(1:10, 1:10 %% 2 == 0, sum)
class(tapply(1:10, 1:10 %% 2 ==0, sum))

#iris에서 종별로 Sepal.Length 평균
 #tapply 활용
tapply(iris$Sepal.Length, iris$Species, mean)
class(tapply(iris$Sepal.Length, iris$Species, mean))

m <- matrix(1:8, 
            ncol = 2,
            dimnames = list( c("spring","summer","fall","winter"),c("male","female") )
            )
# 반기별 남성 셀의 값의 합과 여성셀의 합을 구해보자
# 계절을 반기에 따라 1,2로 구분, 성별을 1,2로 구분하여 별도 색인구성
tapply(m, list(c(1,1,2,2,1,1,2,2),c(1,1,1,1,2,2,2,2)), sum)

library(MASS)
Cars93

head(Cars93)
str(Cars93)
levels(Cars93$Type)

# 타입별 고속도로연비 평균?
tapply(Cars93$MPG.highway,Cars93$Type,mean)

install.packages("ggplot2")
library(ggplot2)

qplot(MPG.highway,
      data = Cars93,
      facets = Type ~. ,
      binwidth=2)


# 프로그램의 흐름을 제어하는 제어문, 연산자, 함수
# +, -, *, /
# %%, 
# ^

# 관계연산자
# ==, !=, <=, >=, <, >

# 논리연산자
# &(AND), |(OR)
# TRUE, FALSE | T, F

# 제어문
# if, switch

if(T) {
      print("true")
} else {
      print("else")
}     

score <- 55
if(score >= 60) {
  print("pass")
} else {
  print("fail")
}

#scan() ,콘솔로부터 데이터를 받는 함수
score <- scan()
score

#scan()함수를 이용하여 키보드로부터 점수를 입력받고
#점수에 따른 학점등급을 출력하라!!
#cat() 함수를 이용하여 한줄로 출력

grade <- ""
if(score >= 90) {
  grade <- "A"
} else if(score >= 80) {
  grade <- "B"  
} else if(score >= 70) {
  grade <- "C"  
} else if(score >= 60) {
  grade <- "D"
} else {
  grade <- "F"
}
cat("당신의 점수는",score, "점이고, 당신의 학점은", grade)  


# 주민번호 14자리를 scan() 입력받아 남자/여자를 구분하는
# if ~ else를 구현하라
jumin <- "730910-1xxxxxx"

library(stringr)
jumin2 <- scan(what="")
gender <- str_sub(jumin2,8,8)

if(gender == "1" | gender == "3"){
   print("남자")
}else if(gender == "2" | gender == "4"){
   print("여자")
} else{
   print("주민번호 형식 틀림")
} 


# if ~ else 한번에 적용(벡터로 입력받아 벡터로 출력)
# ifelse(조건식, true, false)
x <- c(1,2,3,4,5,6,7,8,9)
ifelse( x %% 2 ==0, "even", "odd")

x <- c(80,65,90,95)
ifelse( x >= 70, "pass", "fail")

#평균구할때 na리턴되니까 결측값 na제거
naVec <- c(80,65,90,NA,95,80,NA,100)
mean(naVec, na.rm = T)
is.na(naVec)

#결측값을 평균으로 바꾸기
ifelse(is.na(naVec),mean(naVec, na.rm =T), naVec)

testCsv <- read.csv(file.choose())
str(testCsv)

num5 <- testCsv$q5
num6 <- ifelse(num5 >= 3,"bigger","smaller")
testCsv$q6 <- num6
head(testCsv)

table(testCsv$q6)
str(testCsv)

testCsv$q6 <- factor(testCsv$q6)
str(testCsv)
levels(testCsv$q6)

# q6별 q5의 합은?
aggregate(testCsv$q5,list(testCsv$q6),sum)
with(testCsv, tapply(q5, q6, sum))

# service_data_html_cont.csv
html <- read.csv(file.choose())
str(html)
head(html)

 # hawaii주에 대한 행만 출력
html$State
html[13,]

# 위와같은 방식 대신 which함수이용 , which() : 조건에 만족하는 index 반환
x <- c(2,3,4,5,6,7)
#x에서 6이 들어있는 인덱스 찾기
which(x == 6)
x[5]

# 한줄로 하와이 찾기
html[which(html$State == 'Hawaii'),]

# for, if
# for(루핑을 위한 값) {
#   if(){

#        }
#                     }

i <- 1:10
length(i)
for(idx in i)   {
   cat("idx -> ", idx, "\n")
   print(idx * 2)
}


i <- 1:10
length(i)
for(idx in i)   {
  if(idx %% 2 != 0) {
  cat("idx -> ", idx, "\n")
}
}

# 문) 1~100까지 홀수/짝수의 합을 출력하라
even <- 0
odd <- 0

q <- 1:100
for(x in q) {
  if(x %% 2 != 0) {
   odd <- odd + x  
  }
   else{
    even <- even + x
   }   

}
cat("짝수의 합 =",even,"홀수의 합 =",odd)


# 다음 데이터를 이용하여 프레임을 만들어 serviceStu 에 저장
name <- c("임정섭", "김정수", "최호진")
subject.kor <- c(81, 95, 70)
subject.eng <- c(75, 88, 78)
subject.mat <- c(78, 99, 66)

serviceStu <- data.frame(name, subject.kor, subject.eng, subject.mat)
str(serviceStu)

 #총점과 평균을 구해서 각각 subject.sum, subject.avg 컬럼을 추가하여 저장
#1

subject.sum = apply(serviceStu[2:4],1,sum)
subject.avg = apply(serviceStu[2:4],1,mean)

serviceStu$subject.sum <- subject.sum
serviceStu$subject.avg <- subject.avg
head(serviceStu)

#2
serviceStuSum <- cbind(serviceStu,
                       subject.sum = apply(serviceStu[2:4],1,sum))
serviceStuAvg <- cbind(serviceStuSum,
                       subject.avg = apply(serviceStu[2:4],1,mean))



# subject.grade 컬럼추가, ifelse 조건문으로 등급별 추가,  데이터 프레임에 저장
subject.grade = ""
size <- nrow(name)
for(idx in 1:size){
    if(serviceStu$subject.avg[idx] >= 90) {
       subject.grade[idx] <- "A"
    }else if(serviceStu$subject.avg[idx] >= 80) {
       subject.grade[idx] <- "B"
    }else if(serviceStu$subject.avg[idx] >= 70) {
       subject.grade[idx] <- "C"
    }else if(serviceStu$subject.avg[idx] >= 60) {
       subject.grade[idx] <- "D"
    }else{
       subject.grade[idx] <- "F"
    }
  }
serviceStu$subject.grade <- subject.grade


