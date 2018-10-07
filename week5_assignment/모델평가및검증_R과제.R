set.seed(180808)
library(caret)

######1.
# caret의 confusionMatrix 이용하여 predict된 결과의 성능을 확인하세요
## 기회가 된다면 Kappa 통계량에 대해 좀더 알아보세요
sms_results <- read.csv("sms_results.csv")
head(sms_results)
table(sms_results$actual_type, sms_results$predict_type) #actual_type , predict_type 으로 
confusionMatrix(sms_result$predict_type, sms_results$actual_type) 


######2.
#glm 모델의 성능을 평가합니다
##ifelse 함수를 이용해 glm$fitted.values가 0.5 이상이면 yes, 아니면 no로 지정하고 결과를 out에 할당하세요(factor)
###역시 confusionMatrix로 성능을 확인하세요
credit <- read.csv("credit.csv")
glm <- glm(default ~ ., data=credit,family = binomial)
summary(glm)
head(glm$fitted.values)

out <-NULL
for(i in 1:length(glm$fitted.values)) {
    if(glm$fitted.valuespi]>=0.5) {
        out<-append(out,'yes')
        }
    else{
        out<-append(out,'no')
        

table(credit$default,out)

