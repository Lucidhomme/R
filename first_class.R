#R 대소문자 구분함
# AA.BB .표시는 소유가 아니라 변수값이름
?letters
print(letters)
letters
month.abb
month.name
?print

# PACKAGE란? 함수(FUNCTION) + 데이터셋(DATASET)

install.packages("stringr")
library(stringr)
# .libPaths()   패키지버젼이 꼬일때사용
# library()

# 디버깅용도 print(), paste(), sprintf(), cat()
print("섭이와 함께하는 기초 R 실습")

# sprintf()
 # %d 정수값을 출력할때 지정
 # %f 실수값 "
 # %s 문자 "

sprintf("%d",123)
sprintf("Number ; %d", 100)
sprintf("Number : %d, String : %s", 100, 'jslim')

sprintf("%.2f",123.456)
sprintf("%5d",123)
sprintf("%5d",12345)
sprintf("%5d",123456)

# cat(), print()
print('섭섭이')
print('네 이놈')

name <- 'jslim' ;
name

#CAT과 print의 차이
myFunc <- function() {
  total <- 0
  cat("append ...")
  for(i in 1:10) {
    total <- total + i
    cat(i,"...")
  }
  cat("End!!","\n")
  return(total)
}

myfunc()

myFunc <- function() {
  total <- 0
  #cat("append ...")
  for(i in 1:10) {
    total <- total + i
    print(i)
  }
  #cat("End!!","\n")
  return(total)
}

myFunc()


#변수(알파벳,숫자,_,. 로 구성된다 (단 첫 글자는 반드시 문자 또는 .))
 #단일형 : vector, matrix, array
 #복수형 : list, data.frame


# Vector == python에서는 Vector를 list타입으로 취급한다.vector값 나타내는 2가지 방법
 # 1. C()
 # 2. 1:10

sample_vec <- c(1,2,3,4,5)
class(sample_vec)
typeof(sample_vec)
mode(sample_vec)

#예시
x <- 1:10
x
y <- x^2
y
 #x와 y의 관계를 알 수 있는 함수
plot(x,y)

#수치형
sum(sample_vec) / length(sample_vec)
mean(sample_vec)

#논리형
boolean_vec <- c(TRUE,FALSE,TRUE,FALSE)
boolean_vec <- c(T,F,T,F)

#문자형
string_vec <- c('임정섭','박수진','임은결','임재원')

sample_null <- NA
 #NULL은 그냥 데이터로 인식
sample_null <- NULL

sample_na <- NA
is.na(sample_na)
!is.na(sample_na)

sample_null <- NULL
is.null(sample_null)

over_vec <- c(1,2,3,c(4,5,6))
# start:end
seq_vec <- 1:10

 #변수의 타입을 체크할때 사용하는 함수(str은 구조를 보여주는 함수기도해서 가장중요)
str(boolean_vec)
class(boolean_vec)
typeof(boolean_vec)
mode(boolean_vec)

#rep()
rep(1:10,5)
rep(1:10,each=5)

#seq(from, to, by)
seq(1, 10, 2)
seq(2, 10, 2)
seq(1, 10, length.out = 5)

seq_vec02 <- seq(1, 100, by=3)
length(seq_vec02)

# indexing[]
seq_vec02[5]
seq_vec02[length(seq_vec02)-4]
seq_vec02[30]
 # 인덱싱에서는 조건식을 활용할 수 있다.
 # AND = & , OR = |

  #데이터가 30이하인 데이터만 출력하려면?
  seq_vec02[seq_vec02 <= 30]
  #데이터가 10이상 30이하인 데이터만 출력하려면?
  seq_vec02[seq_vec02 >= 10 & seq_vec02 <= 30]
  #데이터가 10이상이거나 30이하인 데이터만 출력하려면?
  seq_vec02[seq_vec02 >= 10 | seq_vec02 <= 30]
  #인덱스가 홀수인 데이터만 출력하려면?
  seq_vec02_odd <- seq_vec02[seq(1,length(seq_vec02),2)]
  seq_vec02_odd

#round()
 round_vc <- c(10.234, 11.3467)
 round(round_vc,2)
 
 round_vc02 <- 123.234
 round(round_vc02,-1)

 
#names()
data_x <- c(1,2,3)
cols <- c('lim', 'park', 'cho')

names(data_x) <- cols
data_x

names(data_x)
names(data_x)[3]

#하나가져올때도 C를 입혀놓으면 나중에 여러값들을 가져올때 오류안뜸
data_x[c("lim")]
data_x[c("lim", "park")]

#Vector Indexing( 인덱스는 1 )
#벡터내의 데이터 접근 방법
index_vec <- c(1,3,5,7,9)
index_vec[2]
index_vec[1:3]
index_vec[length(index_vec) :3]

index_vec[c(1,3)]

#특정요소만을 제외한다면
index_vec
index_vec[-1]
index_vec[c(-1,-3)]

#길이 
#행렬에서 nrow를 NROW라는 2차원의 벡터로 가져올 수 있음
length(index_vec)
nrow(index_vec)
NROW(index_vec)

#%in% 연산자
bool <- "a" %in% c("A","b","c")
bool <- "A" %in% c("A","b","c")

#setdiff() 차집함, union() 합집함, intersect() 교집함
setdiff( c("a","b","c"), c("a","b"))
union(c("a","b","c"), c("a","b"))
intersect(c("a","b","c"), c("a","b"))

#집합간의 비교 setequal()
setequal(c("a","b","c"), c("a","b"))
setequal(c("a","b","c"), c("a","b","C"))         


 #100에서 200으로 구성된 벡터 sampleVec를 생성한 다음
 #각 문제를 수행하는 코드를 작성하고 답을 구하시오!!
sampleVec <- c(100:200)

 #HINT : head(), tail()
 #문제1 10번째 값을 출력
 sampleVec[c(10)]
 #문제2 끝에서 10개의 값을 잘라내어 출력
 tail(sampleVec,10)
 #문제3 홀수만 출력
 sampleVec[seq(2,length(sampleVec),2)]
 #강사님거 sampleVec[sampleVec %%2 ==1]
 #문제4 3의 배수만 출력
 sampleVec[seq(3,length(sampleVec),3)]
 #강사님거 sampleVec[sampleVec %%3 ==0]
 #문제5 앞에서 20개의 값을 잘라내고 sampleVec.head 변수에 저장하고 출력
 sampleVec.head <- head(sampleVec,20)
 #문제6 sampleVec.head 변수에서 5번째 값을 제외하고 출력
 sampleVec.head[c(-5)]
 #문제7 sampleVec.head 변수에서 5,7,9번째 값을 제외하고 출력
 sampleVec.head[c(-5,-7,-9)]
 
 #월별 결석생 수 통계가 다음과 같을때 이 자료를 absent 벡터에 저장하시오
 #(결석생 수를 값으로 하고, 월 이름을 값의 이름으로 한다)
 ?month.name
 absent <-c(10,8,14,15,9,10,15,12,9,7,8,7)
 names(absent) <- month.name
 
 #문제1 5월(MAY)의 결석생 수를 출력하시오
 absent[c(5)]
 absent["May"]
 #문제2 7월(JUL), 9월(SEP)의 결석생 수를 출력하시오
 absent[c(7,9)]
 #문제3 상반기(1~6월)의 결석생 수의 합계를 출력하시오
 sum(absent[c(1:6)])
 #문제4 하반기(7~12월)의 결석생 수의 평균을 출력하시오
 mean(absent[c(7:12)])
 
#논리형 벡터, 문자형 벡터
 c(T,F,TRUE,FALSE)
 c(T,F,T) | c(TRUE, TRUE, FALSE)
 !c(T,F,T)
 #xor : 둘중하나만 거짓일 경우 True
 xor(c(T,F,T) , c(TRUE, TRUE, FALSE))

#실행과 동시에 결과보고 싶으면 ()이용
#난수 3개를 선정
  (randomNum <- runif(3))
 (0.25 <= randomNum) & (randomNum <= 0.75) 
 any(randomNum > 0.8)
 all(randomNum < 0.8)
#문자열비교 
 c("a","b","c","d","e")
 strVec <- c("H","S","T","N","O")
 strVec[1] > strVec[5]
 strVec[3] > strVec[5]

#paste
 paste("May I","help you?")
 
 ?month.abb
 month.abb
 paste(month.abb, 1:12)
 paste(month.abb, 1:12, c("st","nd","rd",rep("th",9)))  
 
 paste("/user", "local","bin",sep = "")
 
 (seqVec <- paste(1:4))
 class(seqVec)
 
 paste(seqVec, collapse = "jslim")
 paste(seqVec, collapse = "") 

# 정규표현식 함수()
# grep(pattern, data, ignore.case, value)
# 이그노어 케이스=대소문자 구분없다는 표현
 ?grep
  
grepValue <- c("gender","name","age","hEIght","wEIght","tall","EIght")

# 문1) 'ei'로 시작되는 요소가 있는지 
grep('^ei',grepValue, ignore.case = T, value = T)

# 문2) 'ei'문자열을 포함하는 요소가 있는지
grep('ei',grepValue, ignore.case = T, value = T)


grepTxt <- c("Bigdata","Bigdata","bigdata","Data","dataMining","textMining","campus6","campus5")

# 문) b로 시작하는 하나이상의 문자 패턴을 확인하고 싶다면?
grep('^b+',grepTxt,value = T, ignore.case = T)


# gsub(pattern, replacement, data, ignore.case), sub()
# 문자열에서 문자를 바꾸는 기능
grepTxt

# 문) big이라는 단어를 bigger라는 단어로 바꾸고자한다면?
gsub("big","bigger",grepTxt, ignore.case = T)

# 문) grepTxt에서 숫자를 제거하고자 한다면?
gsub("[[:digit:]]","",grepTxt)
gsub("[0-9]","",grepTxt)

sub("[[:digit:]]","",grepTxt)
sub("[0-9]","",grepTxt)

grepTxt
nchar(grepTxt)
str_length(grepTxt)

#strsplit(data,split) : 문자열을 쪼개는 함수이다
#substr(data,start,stop)
#습관적으로 class()로 타입체크할 것
greetingMsg <- "Hi, Bigdata is very important"
strsplit(greetingMsg, "")
substr(greetingMsg,5,11)

#str_extract("대상","[타입]{반복횟수}")
str_extract("abc123def456","[0-9]{3}")
str_extract_all("abc123def456","[0-9]{3}")
str_extract("abc123def456","[a-z]{3}")
str_extract("abc123def456","[a-zA-Z]{3}")

stringDumy <- "임정섭jslim48섭섭해seop34유관순임꺽정홍길동30"
str_extract_all(stringDumy, "[a-z]{3}")
 #3자리 이상을 의미할떄
 str_extract_all(stringDumy, "[a-z]{3,}")
 #범위를 줄떄
 str_extract_all(stringDumy,"[a-z]{3,5}")
 
#문) 연속된 한글 3자 이상 추출
str_extract_all(stringDumy,"^[가-힣]{3,}")
seqName
class(seqName)
#문) 나이추출
age <- str_extract_all(stringDumy,"[0-9]{2}")
age
class(age)
#문) 숫자를 제외 
escapeNum <- str_extract_all(stringDumy,"[^0-9]{3,}")
escapeNum
class(escapeNum)
#문) 한글이름 추출(영문제외)
names <- str_extract_all(escapeNum[[1]],"[^a-z]{3,}")
names
class(names)

# 단어와 숫자에 관련된 메타문자
# 단어(word) : \\w
# 숫자(digit) : \\d
# 엔터키,탭키(행을 바꾸는 것, 간격을 띄우는 것) : \n, \t

ssn <- "730910-1234567"
str_extract_all(ssn,"[0-9]{6}-[1-4]{1}[0-9]{6}") 
str_extract_all(ssn,"\\d{6}-[1-4]\\d{6}")


email <- "jslim9413@naver.com"
# ID는 4자리 이상 @뒤에는 영어로 3자리 이상 .뒤에는 영어로 2자리이상
str_extract_all(email, "\\w{4,}@[a-z]\\w{3,}.[a-z]{2,}")
                          

stringLength <- "우리는 달려간다~이상한 나라로~섭섭이가 잡혀있는 마왕의 소굴로"
#1번 케이스는 ""를 1개의 값으로 인식(1차원)
length(stringLength)
str_length(stringLength)

stringc <- c("a","b","c")
length(stringc)

#문자열 위치
str_locate_all(stringLength ,'섭섭')
class(str_locate_all(stringLength ,'섭섭'))

# 특수문자 제외
num <- "$123,466"
tmp <- str_replace_all(num,"\\$|\\,","")
class(tmp)

?str_replace_all

# 형변환
# as.numeric(temp)
data <- as.numeric(tmp)
data = 2
class(data)

data<- as.character(tmp)
data = "2"
class(data)
