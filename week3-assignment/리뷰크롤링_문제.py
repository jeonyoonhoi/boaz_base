"""
크롤링할 url은 아래에 있습니다.
목표는 페이지, 별점, 리뷰글을 크롤링하는 것입니다.
보내드린 result_example.csv 파일과 같이 만들어서 코드와 함께 제출해주세요.
result_example.csv는 엑셀로 열면 깨질 수도 있으니 메모장에서 보시거나 파이썬으로 보시면 됩니다.(encoding: utf-8)
15페이지까지 크롤링해서 보내주시면 됩니다.

데이터 입출력은 csv, pandas 패키지 중에 편한 것을 선택하셔서 쓰시면 됩니다.
사용 예시를 살짝 적어놓았습니다. 안 따르셔도 돼요.
"""

from bs4 import BeautifulSoup
from urllib.request import urlopen
import pandas as pd
import csv

# 크롤링할 url
# http://deal.11st.co.kr/product/SellerProductDetail.tmall?method=getProductReviewList&prdNo=87595509&page=1&pageTypCd=first&reviewDispYn=Y&isPreview=false&reviewOptDispYn=n&optSearchBtnAndGraphLayer=n&reviewBottomBtn=Y&openDetailContents=Y&pageSize=10&isIgnoreAuth=false&lctgrNo=1001369&leafCtgrNo=0&groupProductNo=0&groupFirstViewPrdNo=0&selNo=16674487#this

# 크롤링한 파일 저장 경로
file_path = 'C:/Users/'

star = []
review = []


# # 크롤링한 파일 저장 이름
# file_name = 'result_example.csv'
#
# # csv 패키지 사용할 때, 저장 결과 파일 생성
# f = open(file_path + file_name, 'a', encoding='utf-8', newline='')
# result = csv.writer(f)
# # 첫 행은 열 이름을 써야할 것입니다.
# result.writerow(['페이지', '별점', '리뷰'])
#
# # pandas 사용할 때, 리스트에 모았다가 한번에 df로 만들 수 있습니다.
# page_num = []
# 둘 중 아무거나 상관없어요^^
while
for


# csv 패키지 사용할 때, 저장 파일 닫기
f.close()


# pandas 사용할 때, 결과 정리
result = pd.DataFrame({'페이지': page_num, '별점': star, '리뷰': review})
# 결과 저장
result.to_csv(file_path+file_name, encoding='utf-8')