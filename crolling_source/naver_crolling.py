from selenium import webdriver
from bs4 import BeautifulSoup
import openpyxl
import time
from pymongo import MongoClient
import datetime

client = MongoClient('mongodb://saecomaster:sksmssk12@na-shard-00-00-olywc.mongodb.net:27017,na-shard-00-01-olywc.mongodb.net:27017,na-shard-00-02-olywc.mongodb.net:27017/test?ssl=true&replicaSet=na-shard-0&authSource=admin&retryWrites=true');
db = client.apt_code;
collection = db.apt_code_naver;
docs = collection.distinct("sigungu_code",{"sigungu_code":{"$gte":4100000000,"$lte":4800000000}});
sigungu_arr =[]
for doc in docs :
    sigungu_arr.append(doc);
try :
    wb = openpyxl.load_workbook('C:/Users/tj/Documents/pythonExcel/region_code_naver.xlsx');
    ws = wb.active;

    driver = webdriver.Chrome('C:/chromedriver_win32/chromedriver')
    driver.implicitly_wait(3)
    driver.get('https://land.naver.com/')
    total_count = 1;
    part_seq = 1;

    for r in ws.rows:
        if int(r[0].value) not in sigungu_arr and int(r[0].value) >= 4150035000 and int(r[0].value) <= 4800000000:

            region_code = str(r[0].value)
            API_HOST = 'https://land.naver.com/article/articleList.nhn?rletTypeCd=A01&tradeTypeCd=&hscpTypeCd=A01%3AA03%3AA04&cortarNo='+region_code
            driver.get(API_HOST)


            # driver.find_element_by_xpath('//div[@class="loc_view"]/div[@id="loc_view1"]/div[@class="selectbox-box"]').click()
            html = driver.page_source
            soup = BeautifulSoup(html, 'html.parser')
            # notices = soup.select('.selectbox-layer:nth-child(2) > div.selectbox-list > ul > li');
            notices = soup.select('div#sideListLayer > div.map_tab > ul.lst_tab > li#complexListTitle > div#complexListLayer > div.housing_inner > ul._ul_complex_list > li > span > a');

            write_row = 2;
            for n in notices:
                print(r[0].value,r[1].value)
                collection.save({"sigungu_code": r[0].value,
                                   "sigungu_name":r[1].value,
                                   "apt_code":n.get("hscp_no"),
                                   "apt_name":n.text.strip(),
                                   "lat":n.get("mapx"),
                                   "lon":n.get("mapy"),
                                   "date":datetime.datetime.now()
                                   })
            time.sleep(10);
except Exception as ex:
    print('에러발생',ex);
# driver.close();