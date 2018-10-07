# 17.09.28. A조
# 의사결정나무(Decision Tree) 

# 필요한 라이브러리 설치 및 불러오기
install.packages('MASS')
install.packages('tree')
library(MASS) 
library(tree)

# 만만한 iris data 이용...
data(iris)

# 입력변수에 따른 출력변수 plot 찍어보기 
# 입력변수(Sepal.Lenghth, Sepal.Width, Petal.Length, Petal.Width)
# 출력변수(Species): Setosa(검), versicolor(빨), virginica(초)
# 산점도를 보면 tree모델을 적용하여 분류해볼만함
plot(iris[,1:4], col=as.integer(iris$Species), pch=substring((iris$Species),1,1))

# tree 모델 적용하기
ir.tr = tree(Species ~., iris)  # iris에 species를 최종라벨으로 tree적용!
summary(ir.tr)  # 만들어진 tree모델의 개요를 볼 수 있음
ir.tr  # tree의 구체적인 내용(분류조건)을 줄글로 볼 수 있음... 눈이 어지러움

# 나무 모양으로 그림 그리기 ! 보기 편함
plot(ir.tr)  # 가지를 만들고
text(ir.tr, all=T)  # 글자를 채워넣기
# 그림을 다시보면 모든 입력변수 4가지를 전부 분류 기준으로 사용했다
# 조금 과하다는 생각이 듬 (overfitting)

# nod를 직접 정해주기 
ir.tr1=snip.tree(ir.tr, nodes=c(12,7)) # 7번과 12번마디를 쳐내고 저장
plot(ir.tr1)
text(ir.tr1, all=T)

# 산점도에서 tree모델로 어떤식으로 구분했는지도 볼 수 있음 
par(pty="s")
plot(iris[,3],iris[,4],type="n", xlab="petal length", ylab="petal width")
text(iris[,3],iris[,4],c("s","c","v")[iris[, 5]])
partition.tree(ir.tr1, add=TRUE, cex=1.5)

# 하나하나 줄글을 보고 수동으로 가지를 쳐내는 작업을 하기 귀찮음...
# tree 사이즈에따른 오분류 class의 갯수를 보자
ir.tr2=prune.misclass(ir.tr)
plot(ir.tr2) # 4개 이후론 misclass비슷함. 4로 선택! 

fin.tr=prune.misclass(ir.tr, best=4) # best=4
plot(fin.tr)
text(fin.tr, all=T)