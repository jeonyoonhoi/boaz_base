# -*- coding: utf-8 -*-
"""
Created on Thu Jul 19 13:33:19 2018

@author: user
"""

from bs4 import BeautifulSoup
from urllib import request, parse
from PIL import Image
import numpy as np

word = parse.quote('사과')  # parse.quote(str): URL 인코딩 됨
url='https://www.google.co.kr/search?q='+word+'&newwindow=1&source=lnms&tbm=isch&sa=X&ved=0ahUKEwiW1K-8qarcAhXJaN4KHaEtBi0Q_AUICigB&biw=1920&bih=974'

hdr = {'User-Agent': 'Mozillar/5.0', 'referer': 'https://www.google.co.kr'}
req = request.Request(url, headers=hdr)

html = BeautifulSoup(request.urlopen(req), 'lxml')
img = html.find_all('img')
img[1]

file_dir = 'C:/Users/user/Desktop/apples/'
count = 1
for tag in img[1:]:
      src = tag.get('src')  # .get(‘속성’): 각 태그에서 해당하는 속성을 가져옴
      # request.urlretrieve(이미지 URL, filename=’저장할 이름’): 이미지 저장함
      request.urlretrieve(src, filename=file_dir+'apple{}.jpg'.format(count))
      count += 1

apple = Image.open(file_dir+'apple1.jpg')  # Image.open(‘이미지 이름’)
img_array = np.array(apple)
img_array.shape

apple = Image.open(request.urlopen(src))
img_array = np.array(apple)
img_array.shape
