######################## Logistic Regression #########################
rm(list=ls())

library(glmnet) ### glmnet 함수를 사용하기 위한 패키지, Ridge, lasso
library(dplyr) 
library(ggplot2)
library(caret) ## "classification and regression training"수, confusionMatrix
library(ROCR) ## ROC커브
library(SDMTools) ## accuracy 함수
library(glmnet)

#### 1.  데이터 불러오기 #### 

  ### UCL machine learning 사이트에 있는 adult data. 
  ### 연봉이 50000달러 이상인지 이하인지를 분류
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",sep = ",")
  
  ### 변수명이 설정되어 있지 않아 변수명을 넣어준다.
names(data) <- c("age", "workclass", "fnlwgt", "education", "education_num",
                 "marital_status", "occupation","relationship", "race", "sex",
                 "capital_gain", "capital_loss", "hours_per-week", "native_country", "wage")

getwd()
setwd("C:/Users/kimhs/OneDrive/바탕 화면")
write.csv(data,"adult.csv", row.names = F)

head(data)
glimpse(data)

levels(data$wage) ### y값이 되는 wage변수 확인
x <- model.matrix(~ . -wage, data) ## 실제로 만들어 지는 독립변수(x값)의 행렬(model matrix)
dim(x)  

  ## 각 factor들의 level수를 알아보자 ## 
levels(data$workclass) %>% length() ## 9
levels(data$education) %>% length() ## 16
levels(data$marital_status) %>% length() ## 7
levels(data$occupation) %>% length() ## 15
levels(data$relationship) %>% length() ## 6
levels(data$race) %>% length() ## 5
levels(data$sex) %>% length() ## 2
levels(data$native_country) %>% length() ## 42

dim(x)
8 + 15 + 6 + 14 + 5 + 4 + 1 + 41 + 6 + 1 
  ## 범주형 변수의 level이 k개일 때 (k-1)개의 더미변수가 생긴다(다중 공선성 때문에)
  ## 6은 연속형 변수의 수, 1은 상수항을 나타낸다
  ## 변수는 13개이지만 실제로는 101개가 된다. 모형이 복잡하다

#### 2. Data split, training data & test data ####
set.seed(18906)
n <- nrow(data) 
idx <- 1:n

train_idx <- sample(idx, n*0.7)
test_idx <- setdiff(idx, train_idx) ### 전체 index에서 training data의 index를 제외시킨다.

train <- data[train_idx,]
test <- data[test_idx,]


##### 3. 시각화 #####

train %>% ggplot(aes(age, fill = wage)) + geom_density(alpha = .5)
  ## 20~30대는 연소득이 5만달라 이하가 많고, 40 ~ 50대에 연소득이 5만달라 이상인 사람이 많다. 60대 이후로는 비슷해 진다.
train %>% ggplot(aes(education_num, fill = wage)) + geom_bar()
  ## 교육기간이 길어질수록 연소득이 5만달러 이상의 비율이 높아진다.

#### 4. Logistic Regression Model ####

data$wage <- ifelse(data$wage == " >50K", 1, 0) ## wage가 50000달러가 넘으면 1, 50000이하면 0으로 바꿔준다.
logistic <- glm(wage ~. ,data = train, family = "binomial")
  ## glm함수는 generalized linear model의 약자, family는 모델의 세부 사항을 지정.
  ## ?family를 통해서 family에 지정할수 있는 값을 볼 수 있다.
  
summary(logistic)

predict_test <- predict(logistic, newdata = test, type = "response")


  ## threshold 결정
i <- 1
acc <- NULL
for(i in 1:100){
  a <- accuracy(test$wage, predict_test, threshold = i/100)
  acc <- rbind(a,acc)
}
acc

j <- 1
F1score <- NULL
for(j in 1:100) {
  a <- ifelse(predict_test > j/100, 1, 0)
  a <- as.vector(a)
  cm <- confusionMatrix(as.factor(a), as.factor(test$wage))
  precision <- cm$table[4]/(cm$table[4] + cm$table[3])
  recall <- cm$table[4]/(cm$table[4] + cm$table[2])
  f1 <- 2*(precision*recall)/ (precision + recall)
  F1score <- append(F1score, f1)
}

F1score <- data.frame((1:100)/100,F1score); F1score


  ## 일반적으로 0.5를 사용한다.
predict_test <- ifelse(predict_test > 0.5, 1, 0)
predict_test <- as.vector(predict_test)


#### 5. 모델 평가 ####
test$wage <- ifelse(test$wage == " >50K", 1, 0)
confusionMatrix(as.factor(predict_test), as.factor(test$wage))
pred <- prediction(predict_test, test$wage)
  ## prediction(predictions, labels, label.ordering = NULL)

pred_perf <- performance(pred,"tpr","fpr")
plot(pred_perf)
auc1 <- performance(pred, measure = "auc")
auc1 <- auc1@y.values[[1]]; auc1
  ## tpr : True Positive Rate
  ## fpr : False Positive Rate



##################### LASSO & RIDGE(Regularization) #########################
  
  ## 모델이 복잡하다.(101개의 변수) 복잡한 모델을 더 간결하게 하기 위한 방법이 regularization이다.
  ## 차원을 축소하기 위한 방법. Lasso와 Ridge가 있다.

#### 1. Lasso & Ridge ####
xx <- model.matrix(wage~.-1 , data) ## 상수항제거, regularization이 상수항에는 적용되지 않는다
x <- xx[train_idx,] ## training data를 x에 저장
dim(x)


lasso_model <- glmnet(x, as.numeric(train$wage), family = "binomial", alpha = 1) ## alpha = 1이면 lasso
ridge_model <- glmnet(x, as.numeric(train$wage), family = "binomial", alpha = 0) ## alpha = 0이면 ridge

plot(lasso_model) 
plot(ridge_model)
  ## 상단에 있는 수는 선택된 변수의 수, y값은 coefficients(beta값들)
  ## L1 norm은 coefficients 제곱합의 square root 값

lasso_model
ridge_model
  ## Df는 자유도(변수의 수), Lambda는 lambda값
coef(lasso_model, s = c(0.1735, 0.1581, 0.1441, 0.1313)) ## 위에 있는 lambda값 몇개를 가져옴
  ## 예를들어, lambda가 0.1735일 때, 
  ## 모델은 wage = marital_status Married-civ-spouse * 0.1853001 - 1.2330569 가 된다.

#### 2. cv.glmnet 교차검증한 Lasso & Ridge ####
lasso_model_cv <- cv.glmnet(x, train$wage, family = "binomial")
  ## lambda값에 따라서 모형이 달라진다. cv.glmnet함수는 교차검증을 해 준다

plot(lasso_model_cv)
  ## x축은 lambda값이고 상단의 숫자는 선택된 변수의 수를 나타낸다. 왼쪽으로 갈수록 더 복잡한 모형이다.
  ## 각 lambda값에서, k-fold 교차검증은 k개의 테스트 오차값을 산출하고 그 값들의 표준편차 값이 오차범위로 나타나 있다.
  ## 빨간 점은 주어진 lambda에서의 k개의 교차검증의 평균값이다.
  ## 최적의 lambda값은 교차검증 오차의 평균값이 최소일 때, 또는 교차검증 오차의 평균값이 최소값으로 부터 1 표준편차 이상 
  ## 떨어지지 않은 모델 중 가장 간단한 모델

log(lasso_model_cv$lambda.min) ## plot에서 점선과 만나는 점
log(lasso_model_cv$lambda.1se) ## plot에서 점선과 만나는 점

## lambda값에 따른 선택된 변수들 ##
coef(lasso_model_cv, s = "lambda.1se")
#coef(lasso_model_cv, s = lasso_model_cv$lambda.1se)

coef(lasso_model_cv, s = "lambda.min")
#coef(lasso_model_cv, s = lasso_model_cv$lambda.min)

  ## 선택된 변수의 수 ##
length(which(coef(lasso_model_cv, s = "lambda.min") > 0)) ## 42
length(which(coef(lasso_model_cv, s = "lambda.1se") > 0)) ## 25


#### 3.glmnet에서 alpha값의 선택(elasticnet) ####

set.seed(18906)
foldid <- sample(1:10, size = length(train$wage), replace = T) ## cv를 위해서 fold index를 지정

cv_alpha1 <- cv.glmnet(x, train$wage, foldid = foldid, alpha = 1, family = "binomial") ## lasso
cv_alpha.5 <- cv.glmnet(x, train$wage, foldid = foldid, alpha = 0.5, family = "binomial") ## lasso와 ridge를 같이 쓸 수 있다.
cv_alpha0 <- cv.glmnet(x, train$wage, foldid = foldid, alpha = 0, family = "binomial") ## ridge

par(mfrow=c(2,2))

plot(cv_alpha1, main = "alpha = 1")   ## alpha1(lasso)은 log(lambda)의 값이 -7.xx, -5.xx 에서 최적이고
plot(cv_alpha.5, main = "alpha = 0.5") ## alpha0.5은 log(lambda)의 값이 -7.xx, -5.xx 에서 최적이다
plot(cv_alpha0, main = "alpha = 0")   ## alpha0(ridge)은 log(lambda)의 값이 -3.xx 일 때 최적이다.

plot(log(cv_alpha1$lambda), cv_alpha1$cvm, pch = 19, col = "red", 
     xlab = "log(lambda)", ylab = cv_alpha1$name, main = "alpha = 1")
points(log(cv_alpha.5$lambda), cv_alpha.5$cvm, pch = 19, col = "green")
points(log(cv_alpha0$lambda), cv_alpha0$cvm, pch = 19, col = "blue")
legend("topleft",legend = c("alpha = 1(lasso)", "alpha = .5", "alpha = 0(ridge)"), 
       pch = 19, col = c("red","green","blue"))
  ## binomial deviance가 작을수록 좋은 모델
  ## alpha가 1일때(lasso일 때), 모델이 더 간단하므로 alpha = 1을 사용!! ##가


par(mfrow=c(1,1))

#### 4. lasso를 이용하여 차원을 축소 했을 때와 로지스틱 모델의 ROC커브 비교 ####

y <- test$wage
predict_test_cv <- predict(lasso_model_cv, s = "lambda.1se", newx = xx[test_idx,] , type = "response")
predict_test_cv <- as.vector(predict_test_cv)

pred2 <- prediction(predict_test_cv, y)
pred2_perf <- performance(pred2, "tpr", "fpr")

plot(pred2_perf, col = "red")
plot(pred_perf, col = "blue", add = T)

auc2 <- performance(pred2, measure = "auc")
auc2 <- auc2@y.values[[1]]

auc1; auc2