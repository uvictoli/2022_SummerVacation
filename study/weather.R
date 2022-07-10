# 기존 데이터 객체 삭제
rm(list=ls())

# 사용할 페키지 설치 및 불러오기
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Data R로 불러오기
data.raw <- read.csv(file.choose()) #csv 파일 R로 불러오기

View(data.raw)
head(data.raw)
dim(data.raw)
str(data.raw)

#변수명 변경
names(data.raw) # 데이터 프레임 열이름 확인
names(data.raw) <- c("일자", "평균기온", "최대기온", "최소기온", "평균지표온도",
                     "최대지표온도", "최소지표온도", "습도", "일사량", "풍속") #열이름 변경
names(data.raw) # 변경된 열이름 확인

#일교차 변수 만들기 (최대기온 - 최소기온)
(data.raw$최대기온 - data.raw$최소기온)
data.raw$일교차 <- (data.raw$최대기온 - data.raw$최소기온)
head(data.raw)

#일교차 변수 만들기(mutate 사용)
data.raw_tempdist <- mutate(data.raw, 일교차 = 최대기온 - 최소기온)
head(data.raw_tempdist)

#월(month) 변수 생성
#'일자'에서 '월'만 추출해 새로운 열을 만들고 싶은 경우
data.month <- mutate(data.raw_tempdist, 월 = substr(data.raw_tempdist$일자, 6, 7))
head(data.month)

#일교차의 평균, 표준편차, 최대값, 최소값 구하기
mean(data.raw_tempdist$일교차)
sd(data.raw_tempdist$일교차)
max(data.raw_tempdist$일교차)
min(data.raw_tempdist$일교차)

# 그룹화 시키기
data.group <- group_by(data.month, 월)
data.group

#그룹된 데이터로 월 평균 일교차 확인
data.group_avg <- summarise(data.group, avg=mean(일교차))
data.group_avg
#그룹된 데이터에 월 평균 일교차 변수 추가
data.avg.td <- mutate(data.group, 월평균 = mean(일교차))
data.avg.td

#불쾌지수 변수 만들기 (Temperature Humidity Index = <1.8 x 온도 - 0.55 (1- 습도)(1.8 x 온도 - 26) + 32>)
#습도가 퍼센트이므로 소수로 바꿔주기 => (습도/100)
data.thi <- mutate(data.month, 불쾌지수 = 1.8*평균기온-0.55*(1-습도/100)*(1.8*평균기온-26)+32)
head(data.thi)

#불쾌지수가 70 초과인 행 추출하기
data.thi$불쾌지수 > 70 # 불쾌지수가 70 초과인 행은 True로 나타남
data.thi_70 <- data.thi[data.thi$불쾌지수 > 70, ] #True로 나타나는 행 출력
head(data.thi_70)
dim(data.thi_70)[1] #행의 개수 출력

# 월 평균 일교차 꺾은선 + 점그래프
ggplot(data=data.group_avg, aes(x=as.numeric(월), y=avg)) + #as.numeric(): 문자 -> 숫자
  geom_line() + 
  geom_point()

ggplot(data=data.group_avg, aes(x=as.numeric(월), y=avg)) +
  geom_line() +
  geom_point(size=2, color="red") +
  ggtitle("월 평균 일교차") +
  ylab("기온") +
  xlab("월") +
  theme(plot.title = element_text(hjust = 0.5)) # 그래프 제목의 위치 (왼쪽:0, 가운데:0.5, 오른쪽:1)

#문자형을 날짜형으로 변경
head(data.thi$일자) #데이터 확인
str(data.thi$일자) #변수 일자의 형식 확인 (chr: 문자열)
head(as.Date(data.thi$일자)) #as.Date함수를 확용하여 날짜형으로 변경
str(as.Date(data.thi$일자)) #변경 후 형식 확인

#월 평균 불쾌지수 꺾은선 그래프
ggplot(data=data.thi, aes(x=as.Date(일자), y=불쾌지수)) + 
  geom_line() +
  ggtitle("일별 불쾌지수") +
  xlab("날짜") +
  geom_hline(yintercept = 70, linetype="dashed", size=1, color="red")

