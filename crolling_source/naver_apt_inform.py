from selenium import webdriver
from bs4 import BeautifulSoup
import time
import re
from pymongo import MongoClient

client = MongoClient('mongodb://saecomaster:sksmssk12@na-shard-00-00-olywc.mongodb.net:27017,na-shard-00-01-olywc.mongodb.net:27017,na-shard-00-02-olywc.mongodb.net:27017/test?ssl=true&replicaSet=na-shard-0&authSource=admin&retryWrites=true');
db = client.apt_code;
collection_code = db.apt_code_naver;
collection_inform = db.apt_inform_naver;
docs = collection_code.find({"sigungu_code": { "$gte" : 0, "$lte" : 2000000000},"check": None});


doc_id = []
doc_apt_code = []
doc_sigungu_code = []
doc_sigungu_name = []
for doc in docs :
    doc_id.append(doc["_id"]);
    doc_apt_code.append(doc["apt_code"]);
    doc_sigungu_code.append(doc["sigungu_code"])
    doc_sigungu_name.append(doc["sigungu_name"])
try:

    driver = webdriver.Chrome('C:/chromedriver_win32/chromedriver')
    driver.implicitly_wait(3)
    driver.get('https://land.naver.com/')

    for r in range(0,len(doc_id)):
        print(str(doc_apt_code[r]),str(doc_sigungu_name[r]))
        API_HOST = 'https://land.naver.com/article/articleList.nhn?rletTypeCd=A01&tradeTypeCd=&hscpTypeCd=A01%3AA03%3AA04&rletNo=' + str(
            doc_apt_code[r])
        driver.get(API_HOST)
        time.sleep(1);
        driver.find_element_by_xpath('//a[@title="단지정보"]').click()
        time.sleep(1);
        html = driver.page_source
        soup = BeautifulSoup(html, 'html.parser')
        notices_apt = soup.select('div.tab_article table.housing_info tbody tr td');


        apt_info = []
        for n in notices_apt:
            apt_info.append(n.text.strip().replace('\n', '').replace('\t', '').replace(' ', ''))
            n.text.strip()

        if len(apt_info) <= 12:
            apt_info.append("없음");

        arr_tube = []
        notices_tube = soup.select('dl.surroundings > dd[tabindex="0"]');
        a_trim = ''
        for a in notices_tube:
            a_trim = a.text.strip().replace('\n', '').replace('\t', '').replace(' ', '')
            if len(a_trim) > 0:
                arr_tube.append(a_trim);


        arr_bus = []
        notices_bus = soup.select('dl.surroundings > dd.bus > ul > li');
        b_trim = ''
        for b in notices_bus:
            b_trim = b.text.strip().replace('\n', '').replace('\t', '').replace(' ', '')
            if len(b_trim) > 0:
                arr_bus.append(b_trim)


        arr_address = '';
        address = soup.select('div.traffic_dsc > ul.normal > li.info.info_v2 ');
        for d in address:
            arr_address = d.text.strip().replace('\n', '').replace('\t', '').split("도로명")[0];

        tube_str = '';
        tube_temp = '';
        tube_cnt = 0
        if len(arr_tube) > 0:
            for tube in arr_tube:
                tube_temp = tube + '|'
                tube_str = tube_str + tube_temp
            tube_cnt = int((len(tube_str.replace("|", ",").split(",")) - 1) // 2)

        bus_str = '';
        bus_temp = '';
        bus_cnt = 0

        if len(arr_bus) > 0:
            for bus in arr_bus:
                bus_temp = bus + '|'
                bus_str = bus_str + bus_temp
            bus_cnt = len(bus_str.replace("|", ",").split(",")) - 1

        if len(re.findall('[\d\.]+', apt_info[0])) > 0:
            apt_info0 = re.findall('[\d\.]+', apt_info[0])[0]
        else:
            apt_info0 = "없음"

        if len(re.findall('[\d\.]+', apt_info[1])) > 0:
            apt_info1 = re.findall('[\d\.]+', apt_info[1])[0]
        else:
            apt_info1 = "없음"

        if len(re.findall('[\d\.]+', apt_info[4])) > 0:
            apt_info4 = re.findall('[\d\.]+', apt_info[4])[0]
        else:
            apt_info4 = "없음"

        if len(re.findall('[\d\.]+', apt_info[5])) > 0:
            apt_info5 = re.findall('[\d\.]+', apt_info[5])[0]
        else:
            apt_info5 = "없음"

        if len(re.findall('[\d\.]+', apt_info[8])) > 0:
            apt_info8 = re.findall('[\d\.]+', apt_info[8])[0]
        else:
            apt_info8 = "없음"

        if len(re.findall('[\d\.]+', apt_info[9])) > 0:
            apt_info9 = re.findall('[\d\.]+', apt_info[9])[0]
        else:
            apt_info9 = "없음"

        if len(re.findall('[\d\.]+', apt_info[10])) > 0:
            apt_info10 = re.findall('[\d\.]+', apt_info[10])[0]
        else:
            apt_info10 = "없음"

        if len(re.findall('[\d\.]+', apt_info[11])) > 0:
            apt_info11 = re.findall('[\d\.]+', apt_info[11])[0]
        else:
            apt_info11 = "없음"


        collection_inform.save({"house_cnt"		: apt_info0,  							# 세대수
                         "tot_dong_cnt"		: apt_info1,  							# 총동수
                         "complete_date"	: apt_info[2],  						# 준공년월
                         "build_co"			: apt_info[3],  						# 건설사명
                         "tot_park_cnt"		: apt_info4,  							# 총주차대수
                         "per_park_cnt"		: apt_info5,  							# 세대당 주차대수
                         "heat_mth"			: apt_info[6],  						# 난방방식
                         "heat_fuel"		: apt_info[7],  						# 난방연료
                         "vlRat"			: apt_info8,  							# 용적률
                         "bcRat"			: apt_info9,  							# 건폐율
                         "max_flr"			: apt_info10,  							# 최고층
                         "min_flr"			: apt_info11,  							# 최저층
                         "area"				: str(apt_info[12]).replace(",", "."),  # 면적
                         "sub"				: tube_str.replace(",", "."),  			# 지하철
                         "sub_cnt"			: tube_cnt,  							# 지하철수
                         "bus"				: bus_str.replace(",", "."),  			# 버스
                         "bus_cnt"			: bus_cnt,  							# 버스수
                         "address"			: arr_address,  						# 주소
                         "apt_code"			: str(doc_apt_code[r]),  					# 아파트코드
                         "sigungu_code"		: str(doc_sigungu_code[r])  						# sigungucode
        });

        collection_code.update(
            {"_id" : doc_id[r]},
            {"$set": {"check": "check"}}
        );

        time.sleep(10);
except Exception as ex:
    print('에러발생', ex);
