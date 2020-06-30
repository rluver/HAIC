require('dplyr')
require('tidyr')
require('stringr')
require('data.table')
require('GGally')





jeju = fread('D:/jeju_data_ver1/201901-202003.csv', encoding = 'UTF-8')
index = fread('D:/코스피_산업별_주가지수_20200630231011.csv', stringsAsFactors = F) %>% t() %>% 
  as.data.frame()

names(index) = index %>% slice(1) %>% unlist()
index = index %>% slice(-1)

index = index %>% gather(key = 'days', value = 'amt', names(index)[-1]) %>% 
  select(2, 1, 3)

arr = array(dim = c(23, 23, 41))

index = index %>% mutate(제조업 = 제조업 %>% as.character() %>% as.numeric(),
                         음식료품 = 음식료품 %>% as.character() %>% as.numeric(),
                         섬유의복 = 섬유의복 %>% as.character() %>% as.numeric(),
                         종이목재 = 종이목재 %>% as.character() %>% as.numeric(),
                         화학 = 화학 %>% as.character() %>% as.numeric(),
                         의약품 = 의약품 %>% as.character() %>% as.numeric(),
                         비금속광물 = 비금속광물 %>% as.character() %>% as.numeric(),
                         철강금속 = 철강금속 %>% as.character() %>% as.numeric(),
                         기계 = 기계 %>% as.character() %>% as.numeric(),
                         전기전자 = 전기전자 %>% as.character() %>% as.numeric(),
                         의료정밀 = 의료정밀 %>% as.character() %>% as.numeric(),
                         운수장비 = 운수장비 %>% as.character() %>% as.numeric(),
                         유통업 = 유통업 %>% as.character() %>% as.numeric(),
                         전기가스업 = 전기가스업 %>% as.character() %>% as.numeric(),
                         건설업 = 건설업 %>% as.character() %>% as.numeric(),
                         운수창고업 = 운수창고업 %>% as.character() %>% as.numeric(),
                         통신업 = 통신업 %>% as.character() %>% as.numeric(),
                         금융업 = 금융업 %>% as.character() %>% as.numeric(),
                         은행 = 은행 %>% as.character() %>% as.numeric(),
                         증권 = 증권 %>% as.character() %>% as.numeric(),
                         보험 = 보험 %>% as.character() %>% as.numeric(),
                         서비스업 = 서비스업 %>% as.character() %>% as.numeric())

kinds = jeju %>% select(STD_CLSS_NM) %>% unique()

j = 0 
for(i in kinds %>% unlist())
{
  j = j + 1
  data = index %>% bind_cols(jeju %>% filter(STD_CLSS_NM == i) %>% 
                               group_by(REG_YYMM) %>% summarise(sum = sum(AMT) %>% as.numeric()) %>% select(2)) 
  
  data = data %>% mutate(sum = as.numeric(sum))
  
  arr[, , j] = data %>% cor()
}


rownames(arr) = names(data)
colnames(arr) = names(data)

