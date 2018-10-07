# Exercises 

#ggplot2 패키지에 내장되어있는 diamond 데이터를 활용하여 다음의 문제들을 해결하시오


library(ggplot2)
View(diamonds)



#1. cut 데이터의 빈도를 나타내는 histogram을 그리시오
ggplot(diamonds,aes(x="",fill=cut)) + geom_bar(width=1)

#2. cut 데이터의 빈도를 나타내는 Pie Chart를 그리시오
ggplot(diamonds,aes(x="",fill=cut)) + geom_bar(width=1) +
  coord_polar(theta='y')

#3. carat별로 diamond의 색 분포를 나타내는 histogram을 그리시오
qplot(carat, data = diamonds, geom = "histogram", fill = color)


#4. carat과 price에 대한 scatterplot을 그리시오.
ggplot(data = diamonds, mapping = aes(x=carat,y=price)) + geom_point()


#5. 3캐럿 이상인 다이아몬드들의 carat과 price에 대한 scatterplot을 그리시오. color는 clarity로 하고, point의 size는 cut으로하시오
qplot(carat,price,data=diamonds[diamonds$carat >= 3,],geom='point', 
      color=clarity, 
      size=cut)
