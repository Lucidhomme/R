# http: // www.localdata.kr
# 데이터를 확인해보자
#"service_data_seoul_coffee_eda.xlsx"
library(readxl)
install.packages("readxl")
coffee <- read_xlsx(file.choose())
View(coffee)
str(coffee)
table(coffee)
# 1. 데이터 전처리
# select와 filter를 통해 아래컬럼만 뽑고 주소지가 서울특별시인 데이터만 추출하여 확인해보자
# 번호, 사업장명, 소재지전체주소, 업태구분명, 시설총규모, 인허가일자, 폐업일자, 소재지면적, 상세영업상태명, 영업상태구분코드
library(dplyr)
test_coffee_select <- coffee %>%
  select(번호, 사업장명, 소재지전체주소, 업태구분명, 시설총규모, 인허가일자, 폐업일자, 소재지면적, 상세영업상태명, 영업상태구분코드) %>%
  filter(str_detect(소재지전체주소,"서울특별시"))
head(test_coffee_select)

#연습예시) 과일들 중 t로 시작하는 과일들 찾고싶다
library(stringr)
fVector <- c("tomato","pear","apple","banana", "mato")
# str_detect(vector, "문자")
str_detect(fVector, "t")  ##이렇게 하면 mato도 조회되니까 시작하는게 안됨
str_detect(fVector, "^t") # t로 시작하는 과일
str_detect(fVector,"o$") # o로 끝나는 과일
str_detect(fVector,"[pe]") # p또는 e를 담고있는 과일
letters
str_detect(letters, "[acd]")



test_coffee_select$업태구분명 %>% table()
#커피숍 업태만 선택하기
cafe <- coffee %>%
         select(번호, 사업장명, 소재지전체주소, 업태구분명, 시설총규모, 인허가일자, 폐업일자, 소재지면적, 상세영업상태명, 영업상태구분코드) %>%
         filter(업태구분명 == "커피숍")
View(cafe)

#폐업하지않고 현재 영업중인 카페찾기
cafe_on <- coffee %>%
           select(번호, 사업장명, 소재지전체주소, 업태구분명, 시설총규모, 인허가일자, 폐업일자, 소재지면적, 상세영업상태명, 영업상태구분코드) %>%
           filter(업태구분명 == "커피숍" & 상세영업상태명 == "영업")
           

#지역구별로 데이터 나누기(서대문, 영등포, 동대문) 3개의 구만 추출(시각화로 사용할 예정)
head(cafe_on)

cafe_gu <- filter(cafe_on,str_detect(소재지전체주소,"서대문구") | str_detect(소재지전체주소,"영등포구") | str_detect(소재지전체주소,"동대문구"))
str(cafe_gu)

#소재지전체주소에서 구만 뽑아서 새로운 파생변수 생성
cafe_gu$"지역구" <- substr(cafe_gu$소재지전체주소,7,10)


###강사님답 
seoul_coffee_select<-seoul_coffee_select %>%
  mutate(지역구 = ifelse( substr(seoul_coffee_select$소재지전체주소,6,9)=="서대문", 
                       substr(seoul_coffee_select$소재지전체주소,6,10),
                       ifelse(substr(seoul_coffee_select$소재지전체주소,6,9)=="영등포", 
                              substr(seoul_coffee_select$소재지전체주소,6,10),
                              ifelse(substr(seoul_coffee_select$소재지전체주소,6,9)=="동대문",
                                     substr(seoul_coffee_select$소재지전체주소,6,10),
                                     substr(seoul_coffee_select$소재지전체주소,7,9)))))


# 인허가일자와 폐업일자의 데이터 형식이 chr와 logic으로 되어있는 것을 확인할 수 있다
# ymd함수를 통해 chr와 logic형식으로 되어있는 데이터형식을 Date로 바꾼다
install.packages("lubridate")
library(lubridate)
str(cafe_gu$인허가일자)
str(cafe_gu$폐업일자)
cafe_gu$인허가일자 <- ymd(cafe_gu$인허가일자)
cafe_gu$폐업일자 <- ymd(cafe_gu$폐업일자)


# Date로 바꾼 인허가 일자 데이터를 바탕으로 인허가 year, month, day를 각각 추출해 가변수를 만들어보자
cafe_gu$년도 <- year(cafe_gu$인허가일자)
cafe_gu$월 <- month(cafe_gu$인허가일자)
cafe_gu$일 <- day(cafe_gu$인허가일자)




# 데이터형식 전처리
# 시설총규모 타입 확인 후 문자형 -> 수치형
# 시설총규모에 따라 이를 구분지어 초소형, 소형, 중형, 대형, 초대형으로 구분지어보려고 한다면?
# 구분은 다음코드와 같이 임의로 지정 3제곱미터 이하는 초소형, 30제곱미터 이하는 소형, 70제곱미터 이하는 중형, 300제곱미터 이하는 대형, 그 이상은 초대형

str(cafe_gu$시설총규모) #문자형을 수치형으로 바꿔야됨
cafe_gu$시설총규모 <- as.numeric(cafe_gu$시설총규모)
cafe_gu$구분 <- ifelse(cafe_gu$시설총규모 <= 3,"초소형",ifelse(cafe_gu$시설총규모 <= 30,"소형", ifelse(cafe_gu$시설총규모 <= 70,"중형", ifelse(cafe_gu$시설총규모 <= 300,"대형","초대형"))))    #규모별로 구분지어서 변수생성

 #강사님 답
cafe_gu <- cafe_gu %>%
  mutate(규모 = ifelse(시설총규모 <= 3,"초소형",
                          ifelse(시설총규모 >3 & 시설총규모 <= 30,"소형",
                                      ifelse(시설총규모 >30 & 시설총규모 <= 70, "중형",
                                                  ifelse(시설총규모 > 70 & 시설총규모 <= 300,"대형",
                                                              ifelse(시설총규모 > 300, "초대형",""))))))
str(cafe_gu)
# 규모별 커피숍 수 확인하기
# 영업중이면서 인허가 일자가 2000년 이후 인 커피숍 수를 규모별로 확인해 본다면?
cafe_gu2000 <- filter(cafe_gu,year(cafe_gu$인허가일자) >= 2000)
str(cafe_gu2000)

 #강사님 답
            cafe_on %>%
            filter(상세영업상태명 == "영업" & 인허가일자 >= "2000-01-01") %>%
            group_by(시설총규모) %>%
            summarize(n=n()) %>%
            arrange(desc(n))

summary(cafe_gu2000)
# 가장 큰 규모의 카페는?
max(cafe_gu2000$시설총규모)
filter(cafe_gu2000,시설총규모 == 810.95)          

 #강사님 답
 which.max(cafe_gu2000$시설총규모)
 cafe_gu2000[which.max(cafe_gu2000$시설총규모),]

 
# 가장 작은 규모의 카페는?
 which.min(cafe_gu2000$시설총규모)
 cafe_gu2000[which.min(cafe_gu2000$시설총규모),]

 
 
 
# 시설총규모를 히스토그램으로 시각화한다면?
 hist(cafe_gu2000$시설총규모)
 library(ggplot2)
 ggplot(data = cafe_gu2000, aes(x=시설총규모,fill=구분)) +
   geom_histogram() +
   scale_x_continuous(breaks = c(100,200,300,400,500,600))
   
# 현재영업중인 카페의 인허가연도 히스토그램 
ggplot(data = cafe_gu2000, aes(x=년도,fill=시설총규모)) +
  geom_histogram(alpha = .5, color = "black", bins = 20)
 
 

# 현재영업과 폐업한 카페의 인허가연도 히스토그램
cafe_test <- cafe
cafe_test <- filter(cafe,str_detect(소재지전체주소,"서울특별시"))
cafe_test$인허가일자 <- ymd(cafe_test$인허가일자)
cafe_test$폐업일자 <- ymd(cafe_test$폐업일자)
cafe_test$년도 <- year(cafe_test$인허가일자)
cafe_test$월 <- month(cafe_test$인허가일자)
cafe_test$일 <- day(cafe_test$인허가일자)
cafe_test$시설총규모 <- as.numeric(cafe_test$시설총규모)
cafe_test$구분 <- ifelse(cafe_test$시설총규모 <= 3,"초소형",ifelse(cafe_test$시설총규모 <= 30,"소형", ifelse(cafe_test$시설총규모 <= 70,"중형", ifelse(cafe_test$시설총규모 <= 300,"대형","초대형"))))

ggplot(data = cafe_test, aes(x=년도,fill=상세영업상태명)) +
   geom_histogram(color = "black")


# 서울소재 커피숍의 인허가 년도별 숫자
# 정보확인 후 데이터 프레임으로 만드세요~~
d1 <- cafe_test %>%
            filter(인허가일자 >= "2000-01-01") %>%
             group_by(년도) %>%
               summarise(n= n())
d1 <- as.data.frame(d1)

# 서울소재 커피숍의 인허가 년도별 숫자와 현재 영업중인 정보확인
# 정보확인 후 데이터 프레임으로 만드세요
d2 <- cafe_test %>%
            filter(상세영업상태명=="영업" & 인허가일자 >= "2000-01-01") %>%
              group_by(년도) %>%
              summarise(n=n())
d2 <- as.data.frame(d2)



d3 <- merge(d1,d2, by = "년도")

d3 <- d3 %>%
      mutate(prob = (n.y)/(n.x))


# 생존율 시각화
# geom_line, geom_point
ggplot(data = d3, aes(x=년도, y=prob)) +
  geom_line() +
  geom_point() +
  ggtitle("서울소재 커피숍의 인허가 연도별 생존율")


# 2001년도의 생존율이 유난히 높아서 해당 지역구에 대한 자료 확인
cafe_test$"지역구" <- substr(cafe_test$소재지전체주소,7,10)


View(cafe_test %>%
  filter(년도 == 2001) %>%
  group_by(지역구) )



# 2001년도 시설총규모에 따른 영업구분을 히스토그램으로 시각화
cafe_2001<-  cafe_test %>%
             filter(년도 == 2001) %>%
             group_by(지역구) 

ggplot(data = cafe_2001, aes(x=시설총규모,fill=상세영업상태명)) +
  geom_histogram(color = "black")


# 2000년도 이후 지역구에 따른 년도별 커피숍 인허가 정보를 요약하고 데이터프레임을 만들어보자
cafe_2000 <- cafe_test %>%
             filter(인허가일자 >= "2000-01-01") %>%
             group_by(년도, 지역구) %>%
             summarise(n=n())

cafe_2000 <- as.data.frame(cafe_2000)
  

  
# 2000년도 이후 지역구에 따른 년도별 커피숍 인허가 정보와 현재 영업중인 정보를 요약하고 데이터프레임을 만들어보자  
cafe_2000s <- as.data.frame(
              cafe_test %>%
                filter(상세영업상태명=="영업" & 인허가일자 >= "2000-01-01") %>%
                group_by(년도,지역구) %>%
                summarise(n=n())
)


cafe_20000 <- merge(cafe_2000,cafe_2000s, by = c("년도","지역구"))
cafe_20000 <- cafe_20000 %>%
              mutate(prob = (n.y)/(n.x))

ggplot(data = cafe_20000, aes(x=년도, y=prob, fill= 지역구)) +
  geom_point()

