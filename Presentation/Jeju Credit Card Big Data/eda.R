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




seoul = jeju %>% filter(CARD_SIDO_NM %in% c('서울', '경기', '인천'))
kangwon = jeju %>% filter(CARD_SIDO_NM %in% c('강원'))
daejeon = jeju %>% filter(CARD_SIDO_NM %in% c('대전', '충남', '세종'))
jeonbuk = jeju %>% filter(CARD_SIDO_NM %in% c('전북'))
kwangju = jeju %>% filter(CARD_SIDO_NM %in% c('광주', '전남', '제주'))
daegu = jeju %>% filter(CARD_SIDO_NM %in% c('대구', '경북'))
chungbuk = jeju %>% filter(CARD_SIDO_NM %in% c('충북'))
pusan = jeju %>% filter(CARD_SIDO_NM %in% c('부산', '경남', '울산'))


# 수도권
cor(seoul %>% group_by(REG_YYMM) %>% summarise(sum = sum(AMT)) %>% .$sum,
    float %>% slice(1) %>% select(-1, -2) %>% unlist())

# 강원
cor(seoul %>% group_by(REG_YYMM) %>% summarise(sum = sum(AMT)) %>% .$sum,
    float %>% slice(2) %>% select(-1, -2) %>% unlist())

# 대전충남
cor(seoul %>% group_by(REG_YYMM) %>% summarise(sum = sum(AMT)) %>% .$sum,
    float %>% slice(3) %>% select(-1, -2) %>% unlist())

# 전북
cor(seoul %>% group_by(REG_YYMM) %>% summarise(sum = sum(AMT)) %>% .$sum,
    float %>% slice(4) %>% select(-1, -2) %>% unlist())

# 광주전남
cor(seoul %>% group_by(REG_YYMM) %>% summarise(sum = sum(AMT)) %>% .$sum,
    float %>% slice(5) %>% select(-1, -2) %>% unlist())

# 대구경북
cor(seoul %>% group_by(REG_YYMM) %>% summarise(sum = sum(AMT)) %>% .$sum,
    float %>% slice(6) %>% select(-1, -2) %>% unlist())

# 부산경남
cor(seoul %>% group_by(REG_YYMM) %>% summarise(sum = sum(AMT)) %>% .$sum,
    float %>% slice(7) %>% select(-1, -2) %>% unlist())

# 충북
cor(seoul %>% group_by(REG_YYMM) %>% summarise(sum = sum(AMT)) %>% .$sum,
    float %>% slice(8) %>% select(-1, -2) %>% unlist())



temp = fread('d:/temp.csv', encoding = 'UTF-8')
sum_df = fread('d:/sum_df.csv', encoding = 'UTF-8') %>% 
  mutate(sum_AMT = sum_AMT %>% as.numeric())



sum_df %>% right_join(temp, by = c('CARD_SIDO_NM', 'STD_CLSS_NM', 'year', 'month'))

temp %>% left_join(sum_df, by = c("CARD_SIDO_NM", "STD_CLSS_NM", "year", 'month')) %>% 
  write.csv('d:/merge.csv')



quan_sido = jeju %>% group_by(REG_YYMM, CARD_SIDO_NM) %>% summarise(quar1 = quantile(AMT, prob = c(0.25)) %>% as.numeric(),
                                                                    quar3 = quantile(AMT, prob = c(0.75))%>% as.numeric()) %>% 
  mutate(iqr = quar3 - quar1,
         lower = quar1 - 1.5 * iqr,
         upper = quar3 + 1.5 * iqr)

quan_clss = jeju %>% group_by(REG_YYMM, STD_CLSS_NM) %>% summarise(quar1 = quantile(AMT, prob = c(0.25)) %>% as.numeric(),
                                                                   quar3 = quantile(AMT, prob = c(0.75))%>% as.numeric()) %>% 
  mutate(iqr = quar3 - quar1,
         lower = quar1 - 1.5 * iqr,
         upper = quar3 + 1.5 * iqr)

quan_age = jeju %>% group_by(REG_YYMM, AGE) %>% summarise(quar1 = quantile(AMT, prob = c(0.25)) %>% as.numeric(),
                                                          quar3 = quantile(AMT, prob = c(0.75))%>% as.numeric()) %>% 
  mutate(iqr = quar3 - quar1,
         lower = quar1 - 1.5 * iqr,
         upper = quar3 + 1.5 * iqr)


card_outlier_merged = jeju %>% left_join(quan_sido, by = c('REG_YYMM', 'CARD_SIDO_NM')) %>% 
  mutate(outlier_sido = ifelse(AMT <= lower, 1, 
                          ifelse(AMT >= upper, 1, 0))) %>% 
  select(-quar1, -quar3, -iqr, -lower, -upper) %>% 
  left_join(quan_clss, by = c('REG_YYMM', 'STD_CLSS_NM')) %>% 
  mutate(outlier_clss = ifelse(AMT <= lower, 1, 
                               ifelse(AMT >= upper, 1, 0))) %>% 
  select(-quar1, -quar3, -iqr, -lower, -upper) %>% 
  left_join(quan_age, by = c('REG_YYMM', 'AGE')) %>% 
  mutate(outlier_age = ifelse(AMT <= lower, 1, 
                               ifelse(AMT >= upper, 1, 0))) %>% 
  select(-quar1, -quar3, -iqr, -lower, -upper)


card_outlier_merged %>% fwrite('d:/card_outlier_merged.csv', row.names = F)



