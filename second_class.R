
# 행렬(MATRIX)
# matrix(), rbind(), cbind()
# apply(data, margin, function)
 #일부 벡터와 행렬을 받아 사용가능
x<- c(1,2,3,4,5,6,7,8,9)
mat <- matrix(x)
class(mat)

matrix(x , nrow = 3)
matrix(x , ncol = 3)
matrix(x , nrow = 3 , ncol = 3, byrow = T)

matrix(0 , nrow = 2 , ncol = 3)
#diag함수는 대각선 값만 가져옴
matD <- diag(0, 3)
class(matD)

x <- matrix(c(1,2,3,4,5,6), 2, 3)

#전치행렬
t(x)

#데이터 접근([행 인덱스, 열 인덱스])
x <- matrix(x, 3, 3)

#행이나 열의 값들이 인덱스의 값들로 변경됨
row(x)
col(x)

#x,1행1열, 3행3열, 1행2열과 2행2열가져오기
x
x[1, 1]
x[3, 3]
tmp <- x[c(1,2),2]
tmp
class(tmp)       

#x의 3행을 제외한 1,2행의 모든값
tmp <- x[-3, ]
tmp <- x[c(1,2),c(1,2,3)]
tmp <- x[c(1,2) , ]
tmp


#1행 출력안하고 1,3번쨰 열을 가져오고 싶다
x[-1,c(1,3)]
x[-1,c(T,F,T)]

#1,3열을 제외한 행렬을 인덱싱 해보자
x
tmp <- x[,-c(1,3)]
tmp
class(tmp)

#x1,x2 2개라서 matrix함수로는 하나의 행렬못만듬,이때 bind함수사용(대신 행과 열의 개수가 맞아야함)
x1 <- c(1,2,3)
x2 <- c(4,5,6,7)

tmpMatrix <- rbind(x1, x2)
tmpMatrix

x1 <- c(1,2,3)
x2 <- c(4,5,6)

tmpMatrix <- rbind(x1,x2)
tmpMatrix
class(tmpMatrix)

tmpMatrix <- cbind(x1,x2)
tmpMatrix
class(tmpMatrix)

tmpMatrix <- rbind(1:3, c(4,5,6), 7:9)
tmpMatrix

# matrix()함수에 dimnames 옵션을 활용하면 행이름, 열이름을 지정할 수 있고 이를 활용하여 인덱싱이 가능하다.

nameMatrix <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, dimnames = list(c("idx1","idx2","idx3"),c("feature1","feature2","feature3")))
class(nameMatrix)

nameMatrix[-c(1,3),]
nameMatrix[,-c(1,3)]
nameMatrix[1:2,-c(1,3)]

##내가 한번 벡터값들을 데이터프레임으로 변경해봄
o1 <- c("40","50","60")
o2 <- c("갑","을","병")
o3 <- rbind(o2,o1)
colnames(o3) <- c("1","2","3")
rownames(o3) <- c("사람", "체육점수")
o4 <- data.frame(o3)

#행렬에 연산이 가능하다
nameMatrix * 2
nameMatrix / 2

# apply()
# margin 1 = row, 2 = col
(x <- matrix(1:4,2,2))
 
 #열의 합 
 colSums(x)
 sumApply <- apply(x,2,sum)  
 class(sumApply)
 
 #행의 합
 rowSums(x) 
 apply(x,1,sum)
 
 iris
str(iris) 

#행의 개수를 지정하지않으면 head는 6개가 나옴
head(iris,5) 
tail(iris)

 #apply()함수를 적용해서 컬럼의 요약정보를 확인해보세요
 sl <- iris[,1]
 sl
 str(sl) 
 apply(iris[,1:4],2,sum)
 apply(iris[,1:4],2,mean)
 apply(iris[,1:4],2,median)
 apply(iris[,1:4],2,max)
 apply(iris[,1:4],2,min) 
 
 summary(iris)
#각 열의 평균값
 colMeans(iris[,1:4])
 
# 특정행 또는 열을 기준으로 정렬
 (x <- matrix(runif(4),2,2))

# order(),정렬의 기준이 되는 인덱스를 리턴(순서값에 대한 디폴트는 오름차순으로) 
 order(x[,2], decreasing = T) 
 x[order(x[,2])] 
 

#배열(Array)
#array(), dim()

m <- matrix(1:12, ncol=4)
m
class(m)

arr <- array(1:12, dim=c(3,4,3))
arr
class(arr)

# 배열에 대한 접근
# 행렬과 유사한 방식으로 각 요소에 접근할 수 있다
# apply 2번째에 c(x,x)들어간거 신경x 
arr[1,2,1]
arr[1,2,3]
arr[-1,2,3]

arrD <- array(1:16 ,dim=c(2,4,2))

apply(arrD, c(1,2), mean)

iris3
apply(iris3,2, mean)
apply(iris3,2,median)
apply(iris3,3,sum)
apply(iris3, c(1,3), mean)


# list
# 타입반환 주의
# (키, 값) 형태의 데이터를 담는 연관배열이다.
# list()
# apply -> (lapply() *key밸류사용(필요에 따라 키값 생략가능), sapply() *밸류 사용)

list <- list()
exList <- list(name = "jslim", height = 177)

exList$name
exList$height

list <- list()
exList <- list(name = "jslim", height = c(1,3,5))

#키값을 생략한 상태로 키값주면 [[1]]표시 안나옴
simpleList <- list(1:4, rep(3:5), "cat")

#리스트는 똑같은 리스트를 중첩할 수 있다
newList <- c(list(1,2,simpleList),c(3,4))

list(a = list(c(1,2,3)),
     b = list(c(1,2,3,4)) )

testList <- list(a = list(c(1,2,3)),
                 b = list(c(1,2,3,4)) )
#리스트가 리스트를 중첩할때 접근방식
testList$a[[1]][2]
testList$b[[1]][3]

#리스트 단일요소 접근방식
member <- list(
               name = "jslim",
               address = "seoul",
               tel = "010-4603-2283",
               age = 48,
               married = T)
member
class(member$name)

 #키로접근하면 밸류만 리턴
 member$name

 #인덱스로 접근하면 키와 밸류값 리턴
 member[1]
 member[[1]][1]


 #데이터를 벡터로 입력
 member <- list(
   name = c("섭섭해","임섭순"),
   address = c("seoul","gwangu"),
   age = c(48,29),
   married = c("남자","여자")
 )
 member 

 member$age
 member$id <- NULL
 member[2]
 member$age[1] <- 38 
 member$id <- c("jslim","admin") 
 member$address[2] <- "seoul"  

 #서로 다른 자료구조(vector, matrix, array) 
 multiList <- list(
 one = c("one","two", "three"),
 second = matrix(1:9, nrow = 3),
 third = array(1:12, dim=c(2,3,2))
 )
 
 multiList
 multiList$third
 multiList[3]
 multiList$one <- c("1","two","three")
 multiList[1] <- "one"
 multiList[[3]][2]
 
 #괄호2개와 1개차이
 # [x]는 리스트에접근(층), [[x]]는 값에 접근
 
# unlist() list -> vector (리스트를 벡터로 만드는 방법)
 vec <- unlist(x)
 vec
 class(vec)
 
#list -> matrix
 matrixList <- list(
 row1 = list(1,2,3),
 row2 = list(10,20,30),
 row3 = list(100,200,300)
 )
 
 matrixList
 class(matrixList) 

# do.call(func, data)
 row_mat <- do.call(rbind, matrixList)
 row_mat <- do.call(cbind, matrixList) 
 
  
 row_mat <- do.call(matrix, matrixList)
 listLength <- list(1:5, list("this is My First tiem R",c(T,F,T)))
 listLength
 length(listLength)
 length(listLength[[1]])
 length(listLength[[2]])
 length(listLength[[2]][[1]])
 
 library(stringr)
 str_length(listLength[[2]][1])
 
 # list 처리 함수
 # lapply : list, key=value
 # sapply : list, value
 
 x <- list(1:5)
 y <- list(6:10)
 
 lapply(c(x,y), FUN = sum)
 sapply(c(x,y), FUN = sum)        

 lapply(c(x,y), FUN = mean)
 sapply(c(x,y), FUN = mean)
 
 
 
iris 
class(lapply(iris[,1:4],mean))

irisList <- lapply(iris[,1:4],mean)
class(irisList)

# 1.unlist()이용하여 리스트를 벡터로 변환
irisVec <- unlist(irisList)
class(irisVec)

# 2.matrix함수를 사용해서 벡터를 행렬로 변환
irisMat <- matrix(irisVec, ncol = 4, byrow = T) 
class(irisMat)       

# 3.as.data.frame()함수를 사용해서 행렬을 데이터프레임으로 변환
irisFrame <- as.data.frame(irisMat)
class(irisFrame)

# 4.names()사용해서 리스트로부터 변수명을 얻어와 데이터 프레임의 각 열에 이름부여
names(irisFrame) <- names(iris[,1:4])
class(irisFrame)

#데이터프레임으로 한번에 만들기
data.frame(do.call(cbind,lapply(iris[,1:4],mean)))


# data.frame
# 행렬과 유사, but 다양한 변수(관측값이 숫자, 문자, 범주등)으로 표현된다
# 각 열에 대한 접근은 $를 이용하여 접근할 수 있다
# 인덱스를 활용하는 방법도 있다
# 데이터 프레임은 class()보다 str()사용하는게 좋음

x <- c(1,3,5,7,9)
y <- c(2,4,6,8,10)

exampleDF <-data.frame(x,y)
str(exampleDF)

exampleDF[-1,]
exampleDF[ , c("x")]
class(exampleDF[ ,c("x")])

#colnames(), rownames()
colnames(exampleDF) <- c("val01","val02")
exampleDF

colist<- names(exampleDF)
class(colist)

# 문자열 벡터, 숫자형 벡터, 문자열 벡터
# data.frame

stuName <- c("조동균","한소연", "박수진", "최가은")
subject.eng <- c(100, 100, 100, 70)
subject.math <- c(80, 70, 100, 100)
subject.kor <- c(100, 100, 100, 100)
score.grade <- c("A","B","A","C")

schDF <- data.frame(stuName, subject.eng, subject.math, subject.kor, score.grade)
str(schDF)
colnames(schDF)<- c("학생이름","영어점수","수학점수","국어점수","학점")

#행의 개수
nrow(schDF)
#열의 개수
ncol(schDF)

#열 추가
#학번을 열로 추가해보세요
학번 <- c("012571", "056280", "096493", "032567")
scondschDF <- cbind(schDF, 학번)

# 더미데이터를 이용해서 행 추가
cc <- c("임정섭", "50","50","50","B","000000")
schDF <- rbind(schDF,cc)

schDF$영어점수
schDF[[1]]
str(schDF)

#형변환(bind사용하다보면 character로 변경이되서 apply를 이용하기 위해서 숫자형이 필요함)
schDF$영어점수 <- as.numeric(schDF$영어점수)
str(schDF)

lapply(schDF[2], mean)

# with(data, expression)
# within(data, expression)
#with 함수는 데이터 프레임 또는 리스트 내 필드를 필드 이름만으로 접근할 수 있게 해주는 함수
#within은 with함수의 기능에 더해서 데이터를 수정하는 기능까지 제공



data(iris)
iris

mean(iris$Sepal.Length)
mean(iris$Sepal.Width)

with(iris, 
     {
      print(mean(Sepal.Length))
      print(mean(Sepal.Width))
      }
     )

x <- data.frame(val=c(1,2,3,4,NA,5,NA))

#is로 시작하는 함수는 대체로 true or false반환
x <- within(x, 
        {
          val <- ifelse(is.na(val), mean(val, na.rm = T), val)
        })

