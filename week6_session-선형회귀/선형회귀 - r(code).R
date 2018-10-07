library(readxl)
library(readr)
library(car)
library(MASS)


# 1. simple linear regression

data1 <- read_excel("C:/Users/wjssm/Desktop/lr/data1.XLS")
data1
lr1 <- lm(y ~ x , data = data1)
lr1
summary(lr1)


###residual(잔차)
res<-resid(lr1) # data1$y-predict(lr1, data1)
plot(res)
plot(lr1, which=1:2)


###graph 설명
#####1-잔차 vs 순서 그림 : 독립성 가정 확인
#####2-잔차 vs 적합치 : 등분산성 가정 확인
#####3-잔차의 Q-Q plot : 정규성 가정 확인


plot(data1$x,data1$y)
abline(lr1$coefficients[1], lr1$coefficients[2],col="red")

summary(lr1)
summary(lr1)$r.squared


#2.multiple linear regression
data(Boston);str(Boston)

lr2<-lm(medv~., data = Boston) #lm(y~x1+x2) or lm(y~.-x1-x2)
summary(lr2)

###residual(잔차)
res2<-resid(lr2) 
plot(res2)
plot(lr2, which=1:2)


###multicolinearity(다중공선성)

#####correlation
cor(Boston[,-14])
symnum(abs(round(cor(Boston[,-14]),2))) 
# rad변수 상관관계 매우 높음.



#####vif : 보통 vif>5 주의, vif > 10 다중공선성이 있다고 판단.
vif(lr2) 

#vif 값 역시 tax, rad변수 값이 매우 큼.


lr22<-lm(medv ~. -tax, data=Boston)
summary(lr2)$r.squared; summary(lr22)$r.squared

#tax 변수가 빠져도 R^2 값은 별 차이 없음. 
#R^2값이 절대적인 모형선택의 기준이라고 할 수 없음. 
#이럴 경우엔 더 simple한 모형이 좋음.



###variable selection

#- step : AIC를 기준으로 좋은 모형을 찾아줌.

###1.backward elimination (default)
###한 번 빠진 변수는 다시 추가될 수 없음.
step(lr2)
###2.forward selection
###한 번 추가된 변수는 다시 빠질 수 없음.
lr2m<-lm(medv ~ 1, data=Boston)
step(lr2m, direction="forward", scope=formula(lr2))
###3.both
step(lr2, direction='both')



#3.dummy variable
apt <- read_delim("C:/Users/wjssm/Desktop/lr/apt.txt","\t", escape_double = FALSE,
                  trim_ws = TRUE)
apt

table(apt$subway) #아파트 근처의 지하철 개수 



lm(y~.,data = apt)

####package 사용 library(dummies) 사용해도 됨.


###as.numeric
apt$bigcompany <- as.numeric(apt$company=='HD' | apt$company=='SS')

apt


#4.variable transforamtion
attach(apt)

par(mfrow=c(2,2))
plot(year, y)
plot(size, y)
plot(volume, y)
plot(bigcompany, y)

#y transformation
plot(year, log(y))
plot(size, log(y))
plot(volume, log(y))
plot(bigcompany, log(y))

ld1<-lm(y~year+size+volume+bigcompany, data = apt)
s1<-step(ld1); summary(s1) #AIC=4214.8, R-squared:  0.8774

ld2<-lm(log(y)~year+size+volume+bigcompany, data = apt)
s2<-step(ld2); summary(s2) #AIC=-802.75, R-squared:  0.8906

ld3<-lm(y~(year+size+volume+bigcompany)^2, data = apt)
s3<-step(ld3); summary(s3) #AIC=4164.96, R-squared:  0.9048

ld4<-lm(log(y)~(year+size+volume+bigcompany)^2, data = apt)
s4<-step(ld4); summary(s4) #AIC=-829.92, R-squared:  0.905

data.frame('R-squared' = c(summary(s1)$r.squared,summary(s2)$r.squared,
                           summary(s3)$r.squared,summary(s4)$r.squared),
           'p' = c(extractAIC(s1)[1],extractAIC(s2)[1],extractAIC(s3)[1],extractAIC(s4)[1]),
           'AIC' = c(extractAIC(s1)[2],extractAIC(s2)[2],extractAIC(s3)[2],extractAIC(s4)[2]),
           row.names = c('y','logy','y_^2','logy_^2'))


