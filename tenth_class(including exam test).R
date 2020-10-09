#데이터 전처리 및 탐색적 분석

# 1)데이터 로드 및 라이브러리 로드

library(readxl)
library(dplyr)

row_data <- read_excel(file.choose())
head(row_data)
str(row_data)
View(row_data)


# 로드한 데이터로부터 성북구와 중구의 미세먼지를 비교하고 시각화 해 보려고 한다
# 1. 비교할 지역 데이터만 추출하여 미세먼지 농도차이가 있는지 확인해보자
dust_district   <- row_data %>%
  filter(area %in% c("성북구","중구"))
str(dust_district)
View(dust_district)


# 2. 수치를 시각적으로 확인해보자
library(ggplot2)
ggplot(data = dust_district, aes(x=yyyymmdd, y=finedust, col = area, group = area)) +
  geom_point()+
  geom_line()


# 3. 현황 파악하기
# yyyymmdd에 따른 데이터 수 파악(내림차순 정렬)
# count()함수 이용
?count
count(dust_district, yyyymmdd) 
## 하나마나임 지금은 %>% arrange(desc(n))

# area 에 따른 데이터 수 파악(내림차순 정렬)
count(dust_district, area) 
## 하나마나임 지금은 %>% arrange(desc(n))


# 4. 지역(성북구, 중구)에 따른 데이터를 변수에 각각 할당해보자
dust_district_sb <- filter(dust_district, area == "성북구")
dust_district_jg <- filter(dust_district, area == "중구")

summary(dust_district_sb$finedust)
summary(dust_district_jg$finedust)

# 5. boxplot을 이용하여 시각화 해보자
# 지역에 따른 미세먼지 농도에 대한 차이가 있는지를 확인
boxplot(dust_district_sb$finedust,
        dust_district_jg$finedust,
        main = "finedust_compare",
        xlab = "AREA",
        ylab = "FINEDUST",
        names = c("성북구","중구"),
        col = c("skyblue","orange") )

# 만약 가설에 대한 검정을 한다면
t.test(data = dust_district,
       finedust ~ area,
       var.equal = T)
###p-value(유의수준이) 보편적으로 95%인 0.05보다 작아야 귀무가설을 기각한다고 할 수 있다.(결론 농도차이가 있다, 귀무가설 = 농도차이가 없다 대립가설 = 농도차이가 있다)




# R과 DBMS 연동을 통한 정형 데이터 처리방법

install.packages("rJava")
# 인터페이스
install.packages("DBI")
# jdbc 함수 제공
install.packages("RJDBC")

Sys.setenv(JAVA_HOME= 'C:\\Program Files\\Java\\jdk1.8.0_121')
library(rJava)
library(DBI)
library(RJDBC)

# DB 연동을 위한 순서는 Driver loading, Connection(hr,hr), Query 수행, 결과 집합 확인하는 과정으로 나뉜다
# (표준의 API를 사용하게되면 벤더사의 제품에 상관없이 코드수정없이도 접근가능함.일단 벤더사 제품의 드라이버를 메모리에 올리고 DB에 접속정보를 입력하여야 DB와 커넥션됨.)

# Driver loading
driver <- JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
               classPath = "C:\\oraclexe\\app\\oracle\\product\\11.2.0\\server\\jdbc\\lib\\ojdbc6.jar")


# Connection
conn <- dbConnect(driver,"jdbc:oracle:thin:@localhost:1521:xe",
                  "hr","hr")


selectEmpQuery <- "select * from employee"
# dbGetQuery() : select
empTable <- dbGetQuery(conn, selectEmpQuery)
## dim과 str함수의 차이 시험에 나옴!!!!  dim은 행과열의 개수만 리턴, str은 데이터타입과 컬럼이름,타입까지 리턴
str(empTable)
dim(empTable)

# table생성
createTempTable <- "create table r_tbl(
                    id varchar2(20) primary key,
                    pwd varchar2(20),
                    username varchar2(50),
                    upoint number default 1000)"

# dbSendUpdate() : DML(insert,update,delete), DDL(create, drop, alter) 
dbSendUpdate(conn, createTempTable)


# insert(dumy data)
insertSQL <- "insert into r_tbl values('jslim','jslim','섭섭이',2000)"
dbSendUpdate(conn,insertSQL)
selectDumySQL <- "select * from r_tbl"
dumyTbl <- dbGetQuery(conn,selectDumySQL)

# update 섭섭이 -> 관리자
updateSQL <- "update r_tbl
              set   username = '관리자'
              where id = 'jslim'"
dbSendUpdate(conn, updateSQL)
dumyTbl <- dbGetQuery(conn, selectDumySQL)

# delete id가 jslim인 데이터
deleteSQL <- "delete from r_tbl
              where id = 'jslim'"
dbSendUpdate(conn,deleteSQL)
dumyTbl <- dbGetQuery(conn, selectDumySQL)

## dbSendUpdate(conn,"delete from r_tbl where id = 'jslim'")
## dumyTbl <- dbGetQuery(conn, selectDumySQL)    이런식으로 직접해도됨

# table 제거
dropSQL <- "drop table r_tbl"
dbSendUpdate(conn,dropSQL)
dumytbl <- dbGetQuery(conn, selectDumySQL)


# 성별에 따른 임금 평균확인과 시각화
str(empTable)


## 강사님답
#1
size <- NROW(empTable$EMP_NO)
gender = NULL
for(idx in 1:size) {
  if(str_sub(empTable$EMP_NO[idx],8,8) ==1) {
    gender[idx] = "M"
  } else {
    gender[idx] = "F"
  }
}
gender

#2
empTable$gender <- NULL
empTable$gender <- ifelse(str_sub(empTable$EMP_NO,8,8) == 1 |
                            str_sub(empTable$EMP_NO,8,8) ==3, "M", "F")

genderGroupMean <- aggregate(empTable$SALARY, list(empTable$gender),mean)
genderGroupMean

ggplot(genderGroupMean, aes(x= Group.1, y=x,fill = Group.1)) +
  geom_bar(stat = "identity")


###강사님거로 하면 박스플롯을 못만드니 전체테이블에서 박스플롯으로 해보자(젠더데이터만들고 샐러리는NA전처리해서 시각화)
empTable$gender <- NULL
empTable$gender <- ifelse(str_sub(empTable$EMP_NO,8,8) == 1 |
                            str_sub(empTable$EMP_NO,8,8) ==3, "M", "F")
empTable$SALARY <- ifelse(is.na(empTable$SALARY),mean(empTable$SALARY,na.rm = T),empTable$SALARY)
boxplot(SALARY ~ gender, data = empTable)

# DB종료
dbDisconnect(conn)


# 비정형 데이터 처리(텍스트 마이닝)
# 단어 빈도를 나타내는 시각화(wordcloud, koNLP, tm)

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


# 감성분석
# service_data_facebook_bigdata.txt
# 비정형데이터는 read.table로 불러들일때 구분이 필요함 그래서 file함수사용
# 비정형데이터는 줄단위로 한줄한줄 읽어들여야함
# 워드클라우드에서는 전체내용에서 특정한 문자를 거르고 추출하여 보통 사용해야함


# 1. 파일로딩
fbook <- read.table(file.choose())

fbook <- file(file.choose(), encoding = "UTF-8")
head(fbook)
fbook_read <- readLines(fbook)
head(fbook_read)
str(fbook_read)

# 2. 전처리(정규표현식을 필요로 한다)
# 문장부호 제거 [[:punct:]] 하는 정규표현식을 활용한다면?
# 특수문자 제거 [[:cntrl:]]
# 숫자 제거 [0-9] , \\d+
# \\w+, \\s+ (공백1개이상), \n (줄바꿈), \t


# gsub()함수를 이용해서
s <- gsub('[[:punct:]]','',fbook_read)
s[1]
s <- gsub('[[:cntrl:]]','',s)
s[1]
s <- gsub('\\d+','',s)
s[1]
s <- tolower(s)
s[1]
head(fbook_read,1)
class(str_split(s, "\\s+"))
wordList <- str_split(s, "\\s+")
wordVec <- unlist(wordList)

# 제공되는 단어사전으로부터 파일을 읽어와보자
# service_data_pos_pol_word.txt
positiveDic <- file(file.choose(), encoding = "UTF-8")
pDic <- readLines(positiveDic)
head(pDic)
str(pDic)

# service_data_neg_pol_word.txt
negativeDic <- file(file.choose(), encoding = "UTF-8")
nDic <- readLines(negativeDic)
head(nDic)
str(nDic)

# 단어를 더 추가하고 싶다면?
pDic <- c(pDic,"긍정의 씨앗")
str(pDic)
nDic <- c(nDic,"부정의 씨앗")
str(nDic)






# 분석을 위한 함수 정의
# 분석된 단어(wordVec) vs 사전 단어(pDic, nDic)에 매치가 되는지를 검사
# match()

pMatch <- match(wordVec,pDic)
nMatch <- match(wordVec,nDic)

# 사전에 등록된 단어 추출을 한다면?
pMatch <- !is.na(pMatch)
nMatch <- !is.na(nMatch)

##지금은 스코어가 전부 31로 나오지만 데이터 프레임을 만들기 전 루프구문을 통해서 긍정과 부정을 나눠야하고 그걸 바탕으로 차트생성 
scores <- sum(pMatch) - sum(nMatch)
scoresDF <- data.frame(score = scores, text = wordVec)
head(scoresDF)


###강사님 답
library(stringr)
library(plyr)
?laply

# 이 함수를 정의하세요 (전체벡터를 루프 )
resultS <- function(words , positive , negative) {
  scores = laply(words, function(words, positive, negative) {
    pMatch = match(words, positive) 
    nMatch = match(words, negative)       ##워드와 포지티브, 워드와 네버티브간 매치 
    
    pMatch = !is.na(pMatch)               ## 결측값없는것만 출력
    nMatch = !is.na(nMatch)
    
    score = sum(pMatch) - sum(nMatch)    ##세미불린이어서 -1,0,1로 반환
    return(score)
  }, positive, negative)      ##positive, negative 패스
  
  scores.df = data.frame(score=scores , text=words)
  return(scores.df)
}
  
resultTbl <- resultS(wordVec,pDic,nDic)

str(resultTbl)
resultTbl$text
resultTbl$score
resultTbl$remark[resultTbl$score >= 1] <- "긍정"
resultTbl$remark[resultTbl$score == 0 ] <- "중립"
resultTbl$remark[resultTbl$score < 0 ] <- "부정"

pieResult <- table(resultTbl$remark)

pie(pieResult, labels = names(pieResult), col = c('orange','green', 'blue'),
    radius = 1.2)



##머신러닝 부분답
# 회귀분석 적절한정도를 평가하는데 사용하는 개념 : 결정계수
# 다중회귀분석에서 독립변수사이가 너무 밀집되어있어 결과가 잘안나올때 :  다중공선성 
# 트레인셋과 테스트셋으로 나눌수 있는방법 아닌 것 : 교차분석
# 카이제곱 검정에서 그룹간 비율이 독립적인지 확인하는데 독립적이란 말의 의미가 아닌것 : 

    