##<프로젝트 하나는 R로 분석, 다른하나는 파이썬으로 분석>

#데이터 마이닝(예측과 분류에 사용,기계가 학습)
 # 지도학습 : 정답(레이블)을 알려주고 학습을 시키는것, 데이터에 결정값이 들어있다,
 ## 분류모델(classification) : 변수값이 범주형이어야함,알고리즘 : logistic regression,KNN, SVM, D-TREE, RandomForest etc.. 
 ## 예측(prediction,estimation) : 변수값이 숫자형이어야함,알고리즘 : regression 


 # 비지도학습 : 레이블이 없음
 ## 군집분석(clustering)  
 ## 연관규칙(association rule)
 ## 연속규칙(sequence rule)

 # 강화학습 : 딥러닝



#1. 단순회귀분석
# 독립변수가 종속변수에 미치는 영향을 분석. 변수간 인과관계를 보려면 변수간 관련성을 먼저 살펴보아야함.즉 선형적관계인지 상관관계를 먼저 보아야함
# 상관분석 vs 회귀분석
# 상관분석 : 하나의 변수와 다른 변수와의 밀접한 관련성을 분석하는 기법
# cor() 
# 회귀분석 : 두 변수간에 원인과 결과의 인과관계가 있는지를 분석하는 기법
# lm()


height <- c(100,120,130,140,150,160,170,180,190)
foot <- c(200,205,210,220,230,240,250,270,290)
plot(height, foot,
     xlab = "키",
     ylab = "발크기")
cor(height, foot)
abline(h = mean(foot), lty = 2)   
abline(v = mean(height), lty = 2)
## 데이터가 회귀직선보다 멀어진다 = 오차가 많이 난다



airquality
str(airquality)
#1.변수들 간 상관관계를 분석하려는데 관련없는 변수들을 제거하고 싶다

air01 <- airquality[,c(1:4)]
install.packages("psych")
library(psych)

pairs.panels(air01)  ##ozone과 Temp의 상관관계가 높아보임(0.7)
plot(air01$Ozone,air01$Temp)
cor(air01$Ozone, air01$Temp)  ##결측치를 제거해야할듯
cor(air01)

summary(air01)

# NA가 있는 행 찾기
air01[!complete.cases(air01),]

air02 <- air01[complete.cases(air01),]
air02[!complete.cases(air02),]

# 결측값을 포함하는 행을 삭제하는 다른방법 : na.omit(air01)
str(air02)
# -1 <= r <= 1
cor(air02) #선형관계수치 확인

install.packages("corrplot")
library(corrplot)

air.cor <- cor(air02)

# 상관계수를 시각화를 통해서 표현해 본다면?
# circle, square, ellipse, shade, color, pie
corrplot(air.cor , method = "ellipse",tl.col = "black", tl.srt = 0)

# ----- [실습]
df <- read.csv("http://goo.gl/HKnl74")
str(df)
##전체만족도에 영향을 주는 변수를 찾고싶을때 상관분석을 먼저 수행

colSums(is.na(df)) #결측값 없다

# 놀이기구의 만족도가 높으면 전체 만족도 또한 높지않을까 예상해보자
plot(df$overall - df$rides)
cor(df$overall, df$rides) # 양적선형관계를 가지고 있구나

cor.test(df$overall, df$rides) #가설에 대한 검증을 하는 단계, 피어슨 켄달 스피어만 방식 중 피어슨 방식 사용
# 상관계수의 값이 95퍼센트의 값이 0.52~0.64안에 들어와있는상태. 그리고 유의수준(p-value)이 0.05보다 작으면 귀무가설(부정적인거) 기각하고 대립가설을 채택.상관관계가 있다


# ---- [실습] iris PART01

iris
colSums(is.na(iris)) # 결측값 없다

# 가설
# 꽃받침의 길이가 길 수록 꽃잎의 넓이도 크다.
# Sepal : 꽃받침
# Petal : 꽃잎

iris
cor(iris[,c(1:4)]) #종을 제외하고 상관관계를 보니 petal.length와 petal.width간 강한 상관관계가 있는 것도 확인
cor.test(iris$Sepal.Length, iris$Petal.Width)
# 유의한것을 확인. 

symnum(cor(iris[,1:4])) #petal.length와 petal.widht가 강한 양의 상관관계를 보이고 있다
install.packages("corrgram")
library(corrgram)
corrgram(cor(iris[,1:4]), type = "corr", upper.panel = panel.conf) #파란색 양적, 빨간색 음적 상관관계 나타냄



# 선형 회귀분석
# 예측 모델에서 사용하는 알고리즘
# 인과 관계를 분석하는 방법

# 1. 조건
# x가 변할 때 y도 변한다

# 2. 조건
# 시각적으로 선행 되어야한다.

# 3. 조건
# 외생변수를 통제할 수 있어야함(다른 요인을 통제하고 인과관계를 분석하고 다항회귀일때는 다중공선성을 제거하고 회귀분석을 해야함)

# 종속변수, 목표변수
# 영향을 받는 변수

# 독립변수
# 영향을 주는 변수

# model <- lm()
# plot(model)
# summary()
# abline()
# abline(intercept, slope)

# y = b0 + b1x (+ e)
# b0 = 절편
# b1 = 기울기
# e(epsilon) : 오차

women #인치,파운드일듯
str(women) 
cor(women)  # 상관관계 크네
# weight = b0 + b1*height
head(women)
fit_model <- lm(weight ~ height,data = women) # 학습모델
-87.52 +3.45*58  ## 58인치일때 웨이트가 학습모델에 대입한 112.58이 아닌 실제 웨이트가 115임. 이때 수치의 차이를 오차라고 함
plot(weight ~ height,data = women)
abline(h = mean(women$weight), lty = 2)
abline(v = mean(women$height), lty = 2)
abline(fit_model, lty =2, col = 'red')  ##모델에서 만들어진 절편과 기울기를 기반으로 회귀직선을 그림


fitted(fit_model)[1] ##모델 예측치 구하는 함수
# 모델 예측치
y_pred <- -87.52 +3.45*58
y_pred

# 오차
err <- 115 - 112.58
err
residuals(fit_model)[1]  ##오차구하는 함수

summary(fit_model) ##R-squared가 신뢰도이다(결정계수). 수정 신뢰도와 멀티플 신뢰도 차이가 크면 모델링을 다시해야함.즉 귀무가설을 기각하고 대립가설을 채택한다.차이가 있다
cor.test(women$weight,women$height)


predict(fit_model, newdata = data.frame(height = 78)) ##예측모델을 만들어 78인치에 따른 몸무게 181파운드를 예측할 수 있는 함수도 있음

##데이터를 가져왔을때 필요한 데이터만을 추출하기위해서 트레이닝셋과 테스트셋을 갈라놓아야하는데 이때 샘플함수,kfolder,hold out방식 중 하나로 수행?


# 예측모델 평가지표
# ME(Mean of Errors, 잔차의 평균, 큰 의미는 없음)
# MSE(Mean Squared Error, 제곱의 평균)
# RMSE(Root Mean of Squared Error, MSE의 제곱근으로 작으면 작을수록 신뢰도가 높은 모델)
# MAE(Mean of Absolute Error)
# MPE(Mean of Percentage Error)
install.packages("forecast")
library(forecast)

swiss
str(swiss)
#2개의 모델을 만들어보자, 전부 수치형이므로 lm사용

model01 <- lm(Fertility ~. ,data = swiss)    #다항회귀분석
model02 <- lm(Fertility ~ Agriculture, data = swiss)

plot(swiss$Fertility)
lines(model01$fitted.values, col = 'red')  #빨간선이 파란선보다 데이터를 잘 따라가고있음
lines(model02$fitted.values, col = 'blue')

accuracy(model01)  #RMSE 수치가 크면 클수록 예측력이 떨어짐
accuracy(model02)

accuracy(fit_model)


# ----------- 선형회귀분석 PART02
# service_datasets_product_regression.csv
linear <- read.csv(file.choose())
# 제품만족도에 영향을 주는 독립변수가 적절성이 맞는지 친밀도가 맞는지 확인
colSums(is.na(linear))
pairs.panels(linear)
plot(linear$제품_친밀도,linear$제품_만족도)
plot(linear$제품_적절성,linear$제품_만족도)
linear_cor <- cor(linear)
corrplot(linear_cor, method = "ellipse",tl.col = "black", tl.srt = 0)
cor.test(linear$제품_적절성,linear$제품_만족도)
cor.test(linear$제품_친밀도,linear$제품_만족도)


pmodel01 <- lm(제품_만족도 ~ 제품_적절성,data = linear) 
pmodel02 <- lm(제품_만족도 ~ 제품_친밀도,data = linear)

summary(pmodel01)  ###데이터에 대한 신뢰도가 낮으므로 선형회귀모델을 적용할 수 없고 모델링을 다시해야함 
summary(pmodel02)

plot(제품_만족도 ~ 제품_적절성,data = linear)
abline(h = mean(linear$제품_적절성), lty = 2)
abline(v = mean(linear$제품_만족도), lty = 2)
abline(pmodel01, lty =2, col = 'red')

plot(제품_만족도 ~ 제품_친밀도,data = linear)
abline(h = mean(linear$제품_친밀도), lty = 2)
abline(v = mean(linear$제품_만족도), lty = 2)
abline(pmodel02, lty =2, col = 'red')


lines(pmodel01$fitted.values, col = 'blue')
lines(pmodel02$fitted.values, col = 'green')
accuracy(pmodel01)
accuracy(pmodel02)



# --- [실습] PART03
# linear regression
# https://www.kaggle.com/andonians/random-linear-regression
# service_datasets_train_ml.csv
train <- read.csv(file.choose())
str(train)

# 1.결측치 확인 및 제거
colSums(is.na(train))
train[!complete.cases(train),]
train <- train[complete.cases(train),]


# 2.상관분석
cor(train)
pairs.panels(train)  ## 1이나왔으므로 정규분포를 정확히 따르고 있음을 알 수 있음
plot(train)
abline(h = mean(train$y), lty =2, col= "blue")  ##회귀선이 x축,y축의 중앙을 지나고 있어야함 
abline(v = mean(train$x), lty =2, col= "red")


# 3.이상치확인
boxplot(train$y,main = "y")
boxplot(train$x,main = "x")

# 4.회귀적합모델 생성
train_model <- lm(y ~ x, data = train)
head(train,1) #첫행 케이스로 확인

y_pred <- -0.1073 + (24 * 1.0007) #y는 intercept + (x케이스*x)
y_pred ###다음 값이 실제 y값과의 오차확인가능


fitted(train_model)[1] ##y_pred 값 확인가능

err <- 21.54945 - 23.9095 ## 오차 확인
err

residuals(train_model)[1]  ## 오차 확인

summary(train_model) ## 99프로 신뢰도를 가짐.귀무가설이 아닌 대립가설 채택

# 분석 결과를 시각화
library(ggplot2)

ggplot(data = train, aes(x,y))+
  geom_point(col = 'red')+
  geom_line(aes(x=train$x, y=predict(train_model, newdata = train)),col = 'blue')


# 정확도를 계산
y_predict <- predict(train_model,newdata = test)  ##아래 테스트 데이터이용
head(test,1)
y_predict[1] 

ggplot(data = test, aes(x,y))+
  geom_point(col = 'red')+
  geom_line(aes(x=test$x, y=predict(train_model, newdata = test)),col = 'blue') +
  ggtitle('X vs Y') +
  xlab('X')+
  ylab('Y')


compare <- cbind(actual = test$y, y_predict) ##실제 79, y_predict 예측 76.94로 흡사
accuracy(train_model)


# service_datasets_train_test_ml.csv
test <- read.csv(file.choose())
str(test)

#PT팁
#1. 데이터 찾기
#2. 주제선정(타당성과 근거)
#3. 향후 어떻게 접목시킬 것인가


# 다중(다항) 선형회귀
iris
# Sepal.Length를 예측하고 싶다 <- 종속변수
# 나머지는 독립변수

iris_model <- lm(Sepal.Length ~ .-Species, data = iris) # 1증가할때 Sepal.Width는 0.65증가함
summary(iris_model)


# 회귀직선을 그려보려고한다.(abline)
# abline() 첫번째 인자는 절편, 두번째 인자는 기울기, 선 스타일

# service_datasets_insurance_ml.csv
insu_train <- read.csv(file.choose())
str(insu_train) 
 #다항분석시에는 독립변수들간 적절한 것을 선택할 수 있는 방식이 있긴함.


# 종속변수 : charges
# 독립변수 : age, bmi, children

# 상관계수 확인해보자
charge.test <- select(insu_train,age,bmi,children,charges)
insu.cor <- cor(charge.test)
pairs.panels(charge.test) 
corrplot(insu.cor,method ="number")


insu_model <- lm(charges ~ age + bmi + children, data = insu_train)
age <- insu_model$coefficients[2]
bmi <- insu_model$coefficients[3]
chilrdren <- insu_model$coefficients[4]
head(insu_train,1)
# y = (19*age) + (27.9*bmi) + (0*children)

lm(charges ~ age + bmi + children + smoker + region, data = insu_train)



install.packages("car")
library(car)
Prestige
str(Prestige)
# type을 제외하고 다항분석
# 종속변수 : income
# 독립변수 : education, women, prestige

# 상관분석
income.test <- select(Prestige,income,education,women,prestige)
income.cor <- cor(income.test)
pairs.panels(income.test)
corrplot(income.cor, method = "number")          

# 회귀모델 만들기
prestige_model <- lm(income ~ .,data = income.test)
# y = (a1*x1) + (a2*x2) + (a3*x3)
predict_y <- -253.8 + (177.2*13.11) + (-50.9*11.16) + (141.4*68.8)  ##인컴 11229
head(income.test,1) ##인컴 12351으로 모델식과 차이는 있음

summary(prestige_model) ##이 서머리에서 별3개짜리가 없는 에듀케이션은 상관관계가 없다

# education 제거 후 재모델링했으나 별 차이가 없다
income.test2 <- select(Prestige,income,women,prestige)
income.cor2 <- cor(income.test2)
pairs.panels(income.test2)
corrplot(income.cor2, method = "number")

prestige_model2 <- lm(income ~ .,data = income.test2)
head(income.test2,1)
predict_y2 <- 431.57+(-48.38*11.16)+(165.87*68.8)         
summary(prestige_model2)


