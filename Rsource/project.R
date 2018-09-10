install.packages('propagate')
install.packages('FactoMineR')
install.packages('randomForest')
install.packages('caret')
install.packages('ROCR')
install.packages('neuralnet')
install.packages('NeuralNetTools')

library(propagate)
library(FactoMineR)
library(car)
library(randomForest)
library(nnet)
library(devtools)
library(caret)
library(ROCR)
library(neuralnet)
library(NeuralNetTools)

setwd('C:\\Users\\tj\\Desktop\\아파트데이터최종본')


######################전국#########################
all.df <- read.csv("전국.csv",  stringsAsFactors= F)
all.scale.df <- as.data.frame(scale(all.df,scale=F))

#all.scale.df.fit <- fitDistr(log(all.scale.df[,1]))
all.scale.lm <- lm(all.scale.df[,1]~.,data = all.scale.df)
all.scale.lm
all.scale.pca <- PCA(all.scale.df, quanti.sup = 1)

all.scale.pca.df <- data.frame(all.scale.pca$var)
View(all.scale.pca.df)
write.csv(all.scale.pca.df,"all.scale.pca.df.csv")



all.scale.lm2 <- lm(all.scale.df[,1]~ 실질가구처분소득+
                      시도내이동.시군구내+
                      비금융자산_기타+
                      전출전입+
                      혼인건수+
                      실업률+
                      시도간전입+
                      시도간전출+
                      시도내이동.시군구간.전입+
                      시도내이동.시군구간.전출+
                      이혼건수+
                      조혼인율+
                      실업자+
                      비경제활동인구+
                      실질GDP성장+
                      X40.49세_여+
                      고용률+
                      CPI+
                      조이혼율+
                      경상수지+
                      X20.29세_남+
                      X40.49세+
                      조사망률+
                      X20.29세+
                      금융자산_자본+
                      범죄발생건수+
                      국제수지_건설수지+
                      X40.49세_남+
                      출생아수+
                      조출생률+
                      사망자수+
                      경제활동참가율+
                      X20.29세_여+
                      금융자산_채무_증권+
                      실거래건설년도+
                      실거래거래건수+
                      국제수지_경상수지+
                      자연증가건수+
                      세대원별세대수_10인세대이상+
                      자연증가율
                    , data=all.scale.df)

#R  0.9663 
#p-value 2.2e-16
summary(all.scale.lm2) 
step(all.scale.lm2)

all.scale.step.lm <-lm(formula = all.scale.df[, 1] ~ 
                         실질가구처분소득 + 
                         시도내이.시군구내 +                          
                         비금융자산_기타 + 
                         혼인건수 + 
                         실업률 + 
                         시도간전입 +
                         X40.49세_여 + 
                         조사망률 + 
                         X20.29세 + 
                         금융자산_자본 + 
                         범죄발생건수  + 
                         조출생률 + 
                         실거래건설년도 + 
                         실거래거래건수 + 
                         세대원별세대수_10인세대이상
                       , data = all.scale.df)


#R-squared:  0.9686 
#p-value: < 2.2e-16
summary(all.scale.step.lm)
vif(all.scale.step.lm) < 10
confint(all.scale.step.lm)

part.scale.df = all.scale.df[,c(1,80,
                                33 ,
                                90 ,
                                8  ,
                                29 ,
                                36 ,
                                70 ,
                                5  ,
                                42 ,
                                83 ,
                                102,
                                3  ,
                                111,
                                109,
                                22)
                             ]

head(part.scale.df)
part.scale.pca <- PCA(part.scale.df,quanti.sup = 1)
part.scale.pca.df <- data.frame(part.scale.pca$var)
View(cor(part.scale.df))
write.csv(part.scale.pca.df,"part.scale.pca.df.csv")


part.scale.df.lm2 <- lm(part.scale.df[,1] ~ part.scale.df[,2] +
                          part.scale.df[,3]                   +
                          part.scale.df[,4]                   +
                          part.scale.df[,5]                   +
                          part.scale.df[,6]                   +
                          part.scale.df[,7]                   +
                          part.scale.df[,8]                   +
                          part.scale.df[,9]                   +
                          part.scale.df[,10]                  +
                          part.scale.df[,11]                  +
                          part.scale.df[,12]                  +
                          part.scale.df[,13]                  +
                          part.scale.df[,14]                  +
                          part.scale.df[,15]                  +
                          part.scale.df[,16]                  +
                          part.scale.df[,17]
                        ,data=part.scale.df)
summary(part.scale.df.lm2)

#랜덤포레스트
all.scale.rnd <- randomForest(아파트평단가~
                                      실질가구처분소득 + 
                                      시도내이.시군구내 +                          
                                      비금융자산_기타 + 
                                      혼인건수 + 
                                      실업률 + 
                                      시도간전입 +
                                      X40.49세_여 + 
                                      조사망률 + 
                                      X20.29세 + 
                                      금융자산_자본 + 
                                      범죄발생건수  + 
                                      조출생률 + 
                                      실거래건설년도 + 
                                      실거래거래건수 + 
                                      세대원별세대수_10인세대이상
                                    , data = all.scale.df,importance=T,na.action = na.omit)

as.data.frame(all.scale.rnd$importance)[c(order(-as.data.frame(all.scale.rnd$importance)$IncNodePurity)),]

#[order(IncNodePurity)]
summary(all.scale.rnd)
varImpPlot(all.scale.rnd)
plot(all.scale.rnd)
mean(all.scale.rnd$err.rate)
xtabs( 실질가구처분소득 + 시도내이동.시군구내 + 
        비금융자산_기타 + 전출전입 + 혼인건수 + 실업률 + 시도간전입 + 
        조혼인율 + 실업자 + 비경제활동인구 + 실질GDP성장 + X40.49세_여 + 
        고용률 + CPI + 경상수지 + 조사망률 + X20.29세 + 금융자산_자본 + 
        범죄발생건수 + 출생아수 + 조출생률 + 사망자수 + 금융자산_채무_증권 + 
        실거래건설년도 + 실거래거래건수 + 자연증가건수 + 세대원별세대수_10인세대이상 + 
        자연증가율 ~ all.scale.df$아파트평단가 + all.scale.rnd$predicted)
sum(all.scale.df$아파트평단가 == all.scale.rnd$predicted)/NROW(all.scale.df$아파트평단가)

###########################1.서울특별시 ###############################
seoul.df <- read.csv("서울특별시.csv",  stringsAsFactors=F)
seoul.scale.df <- as.data.frame(scale(seoul.df,scale=F))
seoul.scale.pca <- PCA(seoul.scale.df, quanti.sup = 1)

seoul.scale.pca.df <- data.frame(seoul.scale.pca$var)
write.csv(seoul.scale.pca.df,"seoul.scale.pca.df.csv")


seoul.scale.lm <- lm(seoul.scale.df[,1]~ 
                       시도내이동_시군구내+
                       총전출+
                       총전입+
                       시도내이동_시군구간전입전출+
                       비경제활동인구+
                       시도간전출+
                       경제활동참가율+
                       순이동+
                       고용률+
                       비금융자산_기타+
                       시도간전입+
                       경제활동인구+
                       실업률+
                       실업자+
                       취업자+
                       실질가구처분소득+
                       전체세대+
                       X15세이상인구+
                       금융자산_채무_증권+
                       CPI+
                       X50_59세_여+
                       사망자수+
                       범죄발생건수+
                       경상수지+
                       조출생률+
                       원유.생산+
                       조사망률+
                       금융자산_자본+
                       실질GDP성장+
                       국제수지_경상수지+
                       X15_64세고용률+
                       X50_59세
                     ,data = seoul.scale.df)
#R  0.938 
#p-value: < 2.2e-16
summary(seoul.scale.lm) 
step(seoul.scale.lm)

seoul.scale.step.lm <- lm(formula = seoul.scale.df[, 1] ~ 
                            시도내이동_시군구내 + 
                            총전출  + 
                            실업자 + 
                            취업자 + 
                            실질가구처분소득 + 
                            전체세대 + 
                            CPI  + 
                            사망자수 + 
                            범죄발생건수 + 
                            조출생률 + 
                            원유.생산 + 
                            금융자산_자본  
                          ,data = seoul.scale.df)
summary(seoul.scale.step.lm)
vif(seoul.scale.step.lm)

seoul.part.scale.df = seoul.scale.df[,c(1,
                                        35 ,
                                        33 ,
                                        26 ,
                                        25 ,
                                        81 ,
                                        12 ,
                                        93 ,
                                        4  ,
                                        109,
                                        3  ,
                                        99 ,
                                        84
                                        
)
]
seoul.part.scale.pca <- PCA(seoul.part.scale.df,quanti.sup = 1)
seoul.part.scale.pca.df <- data.frame(seoul.part.scale.pca$var)
write.csv(seoul.part.scale.pca.df,"seoul.part.scale.pca.df.csv")


#랜덤포레스트
seoul.scale.rnd <- randomForest(seoul.part.scale.df[,1]~ 
                                  시도내이동_시군구내 + 
                                  총전출  + 
                                  실업자 + 
                                  취업자 + 
                                  실질가구처분소득 +
                                  전체세대 + 
                                  CPI  + 
                                  사망자수 + 
                                  범죄발생건수 + 
                                  조출생률 + 
                                  원유.생산 + 
                                  금융자산_자본
                                , data = seoul.part.scale.df,importance=T)

as.data.frame(seoul.scale.rnd$importance)[c(order(-as.data.frame(seoul.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ seoul.scale.df[,1] + seoul.scale.rnd$predicted)
varImpPlot(seoul.scale.rnd)
plot(seoul.scale.rnd)

#신경망
seoul.scale.nnet <- nnet(seoul.part.scale.df[,1]~ 
                           시도내이동_시군구내 + 
                           총전출  + 
                           실업자 + 
                           취업자 + 
                           실질가구처분소득 + 
                           전체세대 + 
                           CPI  + 
                           사망자수 + 
                           범죄발생건수 + 
                           조출생률 + 
                           원유.생산 +
                           금융자산_자본
                         ,data=seoul.part.scale.df,size=3,decay=5e-04,maxit=1000)#옵션체크

summary(seoul.scale.nnet)
garson(seoul.scale.nnet)$data
plotnet(seoul.scale.nnet)


seoul.scale.neuralnet <- neuralnet(seoul.scale.df[,1]~
                                     시도내이동_시군구내 + 
                                     총전출  + 
                                     실업자 + 
                                     취업자 + 
                                     실질가구처분소득 + 
                                     전체세대 + 
                                     CPI  + 
                                     사망자수 + 
                                     범죄발생건수 + 
                                     조출생률 + 
                                     원유.생산 +
                                     금융자산_자본
                                   ,data=seoul.scale.df, hidden=5, linear.output=FALSE, likelihood=TRUE)#옵션체크

seoul.scale.neuralnet 


###########################2.부산광역시 ###############################
busan.df <- read.csv("부산광역시.csv",  stringsAsFactors= F)
busan.scale.df <- as.data.frame(scale(busan.df,scale=F))
busan.scale.pca <- PCA(busan.scale.df, quanti.sup = 1)

busan.scale.pca.df <- data.frame(busan.scale.pca$var)
write.csv(busan.scale.pca.df,"busan.scale.pca.df.csv")

busan.scale.lm <- lm(busan.scale.df[,1] ~
                       시도간전입+
                       총전출+
                       비금융자산_기타+
                       시도내이동_시군구내+
                       총전입+
                       시도내이동_시군구간전입전출+
                       조출생률+
                       실질가구처분소득+
                       시도간전출+
                       실업률+
                       조혼인율+
                       혼인건수+
                       실업자+
                       출생아수+
                       사망자수+
                       조사망률+
                       범죄발생건수+
                       자연증가율+
                       자연증가건수+
                       CPI+
                       X50_59세_남+
                       X50_59세+
                       실거래거래건수+
                       X0_9세_여+
                       국제수지_금융서비스수지+
                       금융자산_채무_증권+
                       순이동+
                       경상수지+
                       X50_59세_여+
                       X10인세대이상+
                       국제수지_건설수지+
                       실질GDP성장+
                       X0_9세+
                       금융자산_자본+
                       X0_9세_남+
                       고용률
                     ,data = busan.scale.df)

#R  0.9663 
#p-value 2.2e-16
summary(busan.scale.lm) 
step(busan.scale.lm)

busan.scale.step.lm <- lm(formula = busan.scale.df[, 1] ~ 비금융자산_기타 + 혼인건수 + 사망자수 + X50_59세 + 실거래거래건수 +  실질GDP성장 + X0_9세 + 금융자산_자본 + 고용률, data = busan.scale.df)

summary(busan.scale.step.lm)
vif(busan.scale.step.lm)

busan.part.scale.df = busan.scale.df[,c(1,
                                        91 ,
                                        8  ,
                                        4  ,
                                        46 ,
                                        110,
                                        79 ,
                                        41 ,
                                        84 ,
                                        30
                                        
)
]
busan.part.scale.pca <- PCA(busan.part.scale.df,quanti.sup = 1)
busan.part.scale.pca.df <- data.frame(busan.part.scale.pca$var)
write.csv(busan.part.scale.pca.df,"busan.part.scale.pca.df.csv")

#랜덤포레스트
busan.scale.rnd <- randomForest(busan.scale.df[,1]~
                                  비금융자산_기타 + 혼인건수 + 사망자수 + X50_59세 + 실거래거래건수 +  실질GDP성장 + X0_9세 + 금융자산_자본 + 고용률
                                , data = busan.scale.df,importance=T)
as.data.frame(busan.scale.rnd$importance)[c(order(-as.data.frame(busan.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ busan.scale.df[,1] + busan.scale.rnd$predicted)
varImpPlot(busan.scale.rnd)
plot(busan.scale.rnd)

#신경망
busan.scale.nnet <- nnet(busan.scale.df[,1]~ 
                           비금융자산_기타 + 혼인건수 + 사망자수 + X50_59세 + 실거래거래건수 +  실질GDP성장 + X0_9세 + 금융자산_자본 + 고용률
                         ,data=busan.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크

summary(seoul.scale.nnet)
garson(seoul.scale.nnet)$data
plotnet(seoul.scale.nnet)

busan.scale.neuralnet <- neuralnet(busan.scale.df[,1]~
                                     비금융자산_기타 + 혼인건수 + 사망자수 + X50_59세 + 실거래거래건수 +  실질GDP성장 + X0_9세 + 금융자산_자본 + 고용률                                     
                                   ,data=busan.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

plot(busan.scale.neuralnet )

###########################3.인천광역시 ###############################
incheon.df <- read.csv("인천광역시.csv",  stringsAsFactors= F)
incheon.scale.df <- as.data.frame(scale(incheon.df,scale=F))
incheon.scale.pca <- PCA(incheon.scale.df, quanti.sup = 1)

incheon.scale.pca.df <- data.frame(incheon.scale.pca$var)
write.csv(incheon.scale.pca.df,"incheon.scale.pca.df.csv")

incheon.scale.lm <- lm(incheon.scale.df[,1] ~
                         총전출						+
                         시도내이동_시군구내+
                         총전입+
                         시도내이동_시군구간전입전출+
                         실업률+
                         비금융자산_기타+
                         시도간전출+
                         실업자+
                         시도간전입+
                         X0_9세_남+
                         X0_9세+
                         실질가구처분소득+
                         고용률+
                         X0_9세_여+
                         실질GDP성장+
                         이혼건수+
                         조사망률+
                         실거래층수+
                         순이동+
                         금융자산_자본+
                         출생아수+
                         금융자산_채무_증권+
                         X40_49세_여+
                         CPI+
                         조이혼율+
                         혼인건수+
                         X40_49세+
                         X40_49세_남+
                         조출생률+
                         경상수지+
                         X15_64세고용률
                       ,data = incheon.scale.df)

#R  0.9663 
#p-value 2.2e-16
summary(incheon.scale.lm) 
step(incheon.scale.lm)

incheon.scale.step.lm <- lm(formula = incheon.scale.df[, 1] ~ 시도내이동_시군구내  + 
                              비금융자산_기타  + X0_9세_남 + 실질가구처분소득 + 
                              고용률 + 실질GDP성장 + 이혼건수 + 조사망률 + 출생아수 + X40_49세_여 
                            + 혼인건수 , data = incheon.scale.df)

summary(incheon.scale.step.lm)
vif(incheon.scale.step.lm)

incheon.part.scale.df = incheon.scale.df[,c(1,
                                            35	,
                                            91	,
                                            54	,
                                            81	,
                                            30	,
                                            79	,
                                            10	,
                                            5	,
                                            2	,
                                            71	,
                                            8	
)
]
incheon.part.scale.pca <- PCA(incheon.part.scale.df,quanti.sup = 1)
incheon.part.scale.pca.df <- data.frame(incheon.part.scale.pca$var)
write.csv(incheon.part.scale.pca.df,"incheon.part.scale.pca.df.csv")

#랜덤포레스트
incheon.scale.rnd <- randomForest(incheon.scale.df[,1]~
                                    시도내이동_시군구내  + 
                                    비금융자산_기타  + X0_9세_남 + 실질가구처분소득 + 
                                    고용률 + 실질GDP성장 + 이혼건수 + 조사망률 + 출생아수 + X40_49세_여 
                                  + 혼인건수
                                  
                                  , data = incheon.scale.df,importance=T)

as.data.frame(incheon.scale.rnd$importance)[c(order(-as.data.frame(incheon.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ incheon.scale.df[,1] + incheon.scale.rnd$predicted)
varImpPlot(incheon.scale.rnd)
plot(incheon.scale.rnd)

#신경망
incheon.scale.nnet <- nnet(incheon.scale.df[,1]~ 
                             시도내이동_시군구내  + 
                             비금융자산_기타  + X0_9세_남 + 실질가구처분소득 + 
                             고용률 + 실질GDP성장 + 이혼건수 + 조사망률 + 출생아수 + X40_49세_여 
                           + 혼인건수
                           ,data=incheon.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크

plotnet(incheon.scale.nnet)

incheon.scale.neuralnet <- neuralnet(incheon.scale.df[,1]~
                                       시도내이동_시군구내  + 
                                       비금융자산_기타  + X0_9세_남 + 실질가구처분소득 + 
                                       고용률 + 실질GDP성장 + 이혼건수 + 조사망률 + 출생아수 + X40_49세_여 
                                     + 혼인건수
                                     ,data=incheon.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

incheon.scale.neuralnet 
###########################4.대구광역시 ###############################
daegu.df <- read.csv("대구광역시.csv",  stringsAsFactors= F)
daegu.scale.df <- as.data.frame(scale(daegu.df,scale=F))
daegu.scale.pca <- PCA(daegu.scale.df, quanti.sup = 1)

daegu.scale.pca.df <- data.frame(daegu.scale.pca$var)
write.csv(daegu.scale.pca.df,"daegu.scale.pca.csv")

daegu.scale.lm <- lm(daegu.scale.df[,1] ~
                       실업률+
                       총전출+
                       실업자+
                       총전입+
                       시도간전입+
                       시도내이동_시군구간전입전출+
                       비금융자산_기타+
                       실거래건설년도+
                       X10인세대이상+
                       시도간전출+
                       시도내이동_시군구내+
                       실질가구처분소득+
                       조출생률+
                       출생아수+
                       총인구수_여+
                       연령구간인구수_여+
                       혼인건수+
                       조혼인율+
                       이혼건수+
                       조사망률+
                       X20_29세_남+
                       CPI+
                       조이혼율+
                       금융자산_채무_증권+
                       사망자수+
                       자연증가율+
                       자연증가건수+
                       금융자산_자본+
                       경상수지+
                       실질GDP성장+
                       X20_29세+
                       실거래거래건수+
                       X20_29세_여+
                       총인구수+
                       연령구간인구수
                     ,data = daegu.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(daegu.scale.lm) 
step(daegu.scale.lm)

daegu.scale.step.lm <- lm(formula = daegu.scale.df[, 1] ~ 
                            실업자 + 
                            시도내이동_시군구간전입전출 + 
                            비금융자산_기타 + 
                            실거래건설년도 + 
                            X10인세대이상 + 
                            실질가구처분소득 +     
                            총인구수_여 + 
                            조혼인율 + 
                            X20_29세_남  + 
                            금융자산_채무_증권  +
                            금융자산_자본 + 
                            실질GDP성장 + 
                            실거래거래건수
                          , data = daegu.scale.df)

summary(daegu.scale.step.lm)
vif(daegu.scale.step.lm) >10

daegu.part.scale.df = daegu.scale.df[,c(1,
                                        26 ,
                                        36 ,
                                        91 ,
                                        112,
                                        22 ,
                                        81 ,
                                        65 ,
                                        56 ,
                                        83 ,
                                        84 ,
                                        79 ,
                                        110,
                                        9
                                        
)
]
daegu.part.scale.pca <- PCA(daegu.part.scale.df,quanti.sup = 1)
daegu.part.scale.pca.df <- data.frame(daegu.part.scale.pca$var)
write.csv(daegu.part.scale.pca.df,"daegu.part.scale.pca.df.csv")

#랜덤포레스트
daegu.scale.rnd <- randomForest(daegu.scale.df[,1]~
                                  실업자 + 
                                  시도내이동_시군구간전입전출 + 
                                  비금융자산_기타 + 
                                  실거래건설년도 + 
                                  X10인세대이상 + 
                                  실질가구처분소득 +     
                                  총인구수_여 + 
                                  조혼인율 + 
                                  X20_29세_남  + 
                                  금융자산_채무_증권  +
                                  금융자산_자본 + 
                                  실질GDP성장 + 
                                  실거래거래건수
                                , data = daegu.scale.df,importance=T)

as.data.frame(daegu.scale.rnd$importance)[c(order(-as.data.frame(daegu.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ daegu.scale.df[,1] + daegu.scale.rnd$predicted)
varImpPlot(daegu.scale.rnd)
plot(daegu.scale.rnd)

#신경망
daegu.scale.nnet <- nnet(daegu.scale.df[,1]~ 
                           실업자 + 
                           시도내이동_시군구간전입전출 + 
                           비금융자산_기타 + 
                           실거래건설년도 + 
                           X10인세대이상 + 
                           실질가구처분소득 +     
                           총인구수_여 + 
                           조혼인율 + 
                           X20_29세_남  + 
                           금융자산_채무_증권  +
                           금융자산_자본 + 
                           실질GDP성장 + 
                           실거래거래건수
                         ,data=daegu.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
daegu.scale.neuralnet <- neuralnet(daegu.scale.df[,1]~
                                     실업자 + 
                                     시도내이동_시군구간전입전출 + 
                                     비금융자산_기타 + 
                                     실거래건설년도 + 
                                     X10인세대이상 + 
                                     실질가구처분소득 +     
                                     총인구수_여 + 
                                     조혼인율 + 
                                     X20_29세_남  + 
                                     금융자산_채무_증권  +
                                     금융자산_자본 + 
                                     실질GDP성장 + 
                                     실거래거래건수
                                   ,data=daegu.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

daegu.scale.neuralnet 
###########################5.대전광역시 ###############################
daejun.df <- read.csv("대전광역시.csv",  stringsAsFactors= F)
daejun.scale.df <- as.data.frame(scale(daejun.df,scale=F))
daejun.scale.pca <- PCA(daejun.scale.df, quanti.sup = 1)

daejun.scale.pca.df <- data.frame(daejun.scale.pca$var)
write.csv(daejun.scale.pca.df,"daejun.scale.pca.csv")

daejun.scale.lm <- lm(daejun.scale.df[,1]~
                        실업자+
                        실업률+
                        시도간전출+
                        시도간전입+
                        총전출+
                        혼인건수+
                        조혼인율+
                        비금융자산_기타+
                        실질가구처분소득+
                        총전입+
                        범죄발생건수+
                        시도내이동_시군구간전입전출+
                        시도내이동_시군구내+
                        조사망률+
                        사망자수+
                        실거래거래건수+
                        X40_49세_여+
                        실질GDP성장+
                        이혼건수+
                        X40_49세+
                        조출생률+
                        실거래건설년도+
                        출생아수+
                        자연증가율+
                        자연증가건수+
                        X40_49세_남+
                        조이혼율+
                        비경제활동인구+
                        X20_29세_남+
                        X20_29세+
                        총인구수_남+
                        연령구간인구수_남+
                        국제수지_건설수지
                      ,data = daejun.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(daejun.scale.lm) 
step(daejun.scale.lm)

daejun.scale.step.lm <- lm(formula = daejun.scale.df[, 1] ~ 
                             시도간전입 + 
                             총전출 + 
                             비금융자산_기타 + 
                             사망자수 + 
                             실거래거래건수 + 
                             X40_49세_여 + 
                             실질GDP성장 + 
                             실거래건설년도  + 
                             X20_29세 + 
                             국제수지_건설수지
                           , data = daejun.scale.df)

summary(daejun.scale.step.lm)
vif(daejun.scale.step.lm)

daejun.part.scale.df = daejun.scale.df[,c(1,
                                          37	,
                                          33	,
                                          91	,
                                          4	,
                                          110	,
                                          71	,
                                          79	,
                                          112	,
                                          43	,
                                          104
                                          
)
]
daejun.part.scale.pca <- PCA(daejun.part.scale.df,quanti.sup = 1)
daejun.part.scale.pca.df <- data.frame(daejun.part.scale.pca$var)
write.csv(daejun.part.scale.pca.df,"daejun.part.scale.pca.df.csv")

#랜덤포레스트
daejun.scale.rnd <- randomForest(daejun.scale.df[,1]~
                                   시도간전입 + 
                                   총전출 + 
                                   비금융자산_기타 + 
                                   사망자수 + 
                                   실거래거래건수 + 
                                   X40_49세_여 + 
                                   실질GDP성장 + 
                                   실거래건설년도  + 
                                   X20_29세 + 
                                   국제수지_건설수지
                                 , data = daejun.scale.df,importance=T)

as.data.frame(daejun.scale.rnd$importance)[c(order(-as.data.frame(daejun.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ daejun.scale.df[,1] + daejun.scale.rnd$predicted)
varImpPlot(daejun.scale.rnd)
plot(daejun.scale.rnd)

#신경망
daejun.scale.nnet <- nnet(daejun.scale.df[,1]~ 
                            시도간전입 + 
                            총전출 + 
                            비금융자산_기타 + 
                            사망자수 + 
                            실거래거래건수 + 
                            X40_49세_여 + 
                            실질GDP성장 + 
                            실거래건설년도  + 
                            X20_29세 + 
                            국제수지_건설수지
                          ,data=daejun.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
daejun.scale.neuralnet <- neuralnet(daejun.scale.df[,1]~
                                      시도간전입 + 
                                      총전출 + 
                                      비금융자산_기타 + 
                                      사망자수 + 
                                      실거래거래건수 + 
                                      X40_49세_여 + 
                                      실질GDP성장 + 
                                      실거래건설년도  + 
                                      X20_29세 + 
                                      국제수지_건설수지
                                    ,data=daejun.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

daejun.scale.neuralnet 
###########################6.울산광역시 ###############################
ulsan.df <- read.csv("울산.csv",  stringsAsFactors= F)
ulsan.scale.df <- as.data.frame(scale(ulsan.df,scale=F))
ulsan.scale.pca <- PCA(ulsan.scale.df, quanti.sup = 1)

ulsan.scale.pca.df <- data.frame(ulsan.scale.pca$var)
write.csv(ulsan.scale.pca.df,"ulsan.scale.pca.df.csv")

ulsan.scale.lm <- lm(ulsan.scale.df[,1] ~
                       총전출+
                       시도내이동_시군구간전입전출+
                       총전입+
                       이혼건수+
                       X0_9세_남+
                       출생아수+
                       X0_9세+
                       비금융자산_기타+
                       범죄발생건수+
                       조출생률+
                       자연증가건수+
                       실업률+
                       시도간전출+
                       조이혼율+
                       실업자+
                       실질GDP성장+
                       시도간전입+
                       자연증가율+
                       시도내이동_시군구내+
                       실질가구처분소득+
                       X20_29세_여+
                       X15_64세고용률+
                       금융자산_자본+
                       실거래건설년도+
                       혼인건수+
                       X0_9세_여+
                       경제활동참가율+
                       금융자산_채무_증권+
                       조혼인율+
                       고용률+
                       X30_39세_남+
                       국제수지_건설수지+
                       CPI+
                       경제심리지수_순환변동치+
                       경상수지+
                       X9인세대+
                       경제심리지수_원계열
                     ,data = ulsan.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(ulsan.scale.lm) 
step(ulsan.scale.lm)

ulsan.scale.step.lm <- lm(formula = ulsan.scale.df[, 1] ~ 
                            총전출 + 
                            X0_9세_남 + 
                            비금융자산_기타 + 
                            조출생률 + 
                            실업률 + 
                            시도간전출 + 
                            조이혼율  + 
                            실질가구처분소득 + 
                            X20_29세_여 + 
                            실거래건설년도 + 
                            X30_39세_남 + 
                            X9인세대, data = ulsan.scale.df)

summary(ulsan.scale.step.lm)
vif(ulsan.scale.step.lm)

ulsan.part.scale.df = ulsan.scale.df[,c(1,
                                        33	,
                                        54	,
                                        91	,
                                        3	,
                                        29	,
                                        38	,
                                        11	,
                                        81	,
                                        69	,
                                        112	,
                                        57	,
                                        21	
                                        
                                        
)
]
ulsan.part.scale.pca <- PCA(ulsan.part.scale.df,quanti.sup = 1)
ulsan.part.scale.pca.df <- data.frame(ulsan.part.scale.pca$var)
write.csv(ulsan.part.scale.pca.df,"ulsan.part.scale.pca.df.csv")

#랜덤포레스트
ulsan.scale.rnd <- randomForest(ulsan.scale.df[,1]~
                                  총전출 + 
                                  X0_9세_남 + 
                                  비금융자산_기타 + 
                                  조출생률 + 
                                  실업률 + 
                                  시도간전출 + 
                                  조이혼율  + 
                                  실질가구처분소득 + 
                                  X20_29세_여 + 
                                  실거래건설년도 + 
                                  X30_39세_남 + 
                                  X9인세대
                                , data = ulsan.scale.df,importance=T)

as.data.frame(ulsan.scale.rnd$importance)[c(order(-as.data.frame(ulsan.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ ulsan.scale.df[,1] + ulsan.scale.rnd$predicted)
varImpPlot(ulsan.scale.rnd)
plot(ulsan.scale.rnd)

#신경망
ulsan.scale.nnet <- nnet(ulsan.scale.df[,1]~ 
                           총전출 + 
                           X0_9세_남 + 
                           비금융자산_기타 + 
                           조출생률 + 
                           실업률 + 
                           시도간전출 + 
                           조이혼율  + 
                           실질가구처분소득 + 
                           X20_29세_여 + 
                           실거래건설년도 + 
                           X30_39세_남 + 
                           X9인세대
                         ,data=ulsan.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
ulsan.scale.neuralnet <- neuralnet(ulsan.scale.df[,1]~
                                     총전출 + 
                                     X0_9세_남 + 
                                     비금융자산_기타 + 
                                     조출생률 + 
                                     실업률 + 
                                     시도간전출 + 
                                     조이혼율  + 
                                     실질가구처분소득 + 
                                     X20_29세_여 + 
                                     실거래건설년도 + 
                                     X30_39세_남 + 
                                     X9인세대
                                   ,data=ulsan.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

ulsan.scale.neuralnet 
###########################7.광주광역시 ###############################
ghwangju.df <- read.csv("광주광역시.csv",  stringsAsFactors= F)
ghwangju.scale.df <- as.data.frame(scale(ghwangju.df,scale=F))
ghwangju.scale.pca <- PCA(ghwangju.scale.df, quanti.sup = 1)

ghwangju.scale.pca.df <- data.frame(ghwangju.scale.pca$var)
write.csv(ghwangju.scale.pca.df,"ghwangju.scale.pca.df.csv")

ghwangju.scale.lm <- lm(ghwangju.scale.df[,1] ~
                          실업률+
                          실업자+
                          총전출+
                          시도간전출+
                          이혼건수+
                          실질가구처분소득+
                          비금융자산_기타+
                          조이혼율+
                          총전입+
                          시도간전입+
                          시도내이동_시군구내+
                          비경제활동인구+
                          실거래건설년도+
                          시도내이동_시군구간전입전출+
                          X100세이상_남+
                          혼인건수+
                          조사망률+
                          고용률+
                          경제활동참가율+
                          X20_29세_남+
                          조혼인율+
                          실거래거래건수+
                          CPI+
                          실질GDP성장+
                          X20_29세+
                          경상수지+
                          조출생률+
                          금융자산_채무_증권+
                          출생아수+
                          국제수지_금융서비스수지+
                          사망자수+
                          국제수지_건설수지+
                          X15_64세고용률+
                          X9인세대
                        ,data = ghwangju.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(ghwangju.scale.lm) 
step(ghwangju.scale.lm)

ghwangju.scale.step.lm <- lm(formula = ghwangju.scale.df[, 1] ~ 
                               실업자 + 
                               시도간전출 + 
                               실질가구처분소득 + 
                               비금융자산_기타 + 
                               시도내이동_시군구내 + 
                               비경제활동인구 + 
                               실거래건설년도 + 
                               X20_29세_남 + 
                               금융자산_채무_증권 + 
                               출생아수 + 
                               국제수지_금융서비스수지 + 
                               국제수지_건설수지 + 
                               X9인세대, data = ghwangju.scale.df)

summary(ghwangju.scale.step.lm)
vif(ghwangju.scale.step.lm)
View(cor(ghwangju.part.scale.df))

ghwangju.part.scale.df = ghwangju.scale.df[,c(1,
                                              26	,
                                              38	,
                                              81	,
                                              91	,
                                              35	,
                                              27	,
                                              112	,
                                              56	,
                                              83	,
                                              2	,
                                              105	,
                                              104	,
                                              21	
)
]
ghwangju.part.scale.pca <- PCA(ghwangju.part.scale.df,quanti.sup = 1)
ghwangju.part.scale.pca.df <- data.frame(ghwangju.part.scale.pca$var)
write.csv(ghwangju.part.scale.pca.df,"ghwangju.part.scale.pca.df.csv")

#랜덤포레스트
ghwangju.scale.rnd <- randomForest(ghwangju.scale.df[,1]~
                                     실업자 + 
                                     시도간전출 + 
                                     실질가구처분소득 + 
                                     비금융자산_기타 + 
                                     시도내이동_시군구내 + 
                                     비경제활동인구 + 
                                     실거래건설년도 + 
                                     X20_29세_남 + 
                                     금융자산_채무_증권 + 
                                     출생아수 + 
                                     국제수지_금융서비스수지 + 
                                     국제수지_건설수지 + 
                                     X9인세대
                                   , data = ghwangju.scale.df,importance=T)

as.data.frame(ghwangju.scale.rnd$importance)[c(order(-as.data.frame(ghwangju.scale.rnd$importance)$IncNodePurity)),]

xtabs(~ ghwangju.scale.df[,1] + ghwangju.scale.rnd$predicted)
varImpPlot(ghwangju.scale.rnd)
plot(ghwangju.scale.rnd)

#신경망
ghwangju.scale.nnet <- nnet(ghwangju.scale.df[,1]~ 
                              실업자 + 
                              시도간전출 + 
                              실질가구처분소득 + 
                              비금융자산_기타 + 
                              시도내이동_시군구내 + 
                              비경제활동인구 + 
                              실거래건설년도 + 
                              X20_29세_남 + 
                              금융자산_채무_증권 + 
                              출생아수 + 
                              국제수지_금융서비스수지 + 
                              국제수지_건설수지 + 
                              X9인세대
                            ,data=ghwangju.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크

plotnet(ghwangju.scale.nnet)

ghwangju.scale.neuralnet <- neuralnet(ghwangju.scale.df[,1]~
                                        실업자 + 
                                        시도간전출 + 
                                        실질가구처분소득 + 
                                        비금융자산_기타 + 
                                        시도내이동_시군구내 + 
                                        비경제활동인구 + 
                                        실거래건설년도 + 
                                        X20_29세_남 + 
                                        금융자산_채무_증권 + 
                                        출생아수 + 
                                        국제수지_금융서비스수지 + 
                                        국제수지_건설수지 + 
                                        X9인세대
                                      ,data=ghwangju.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

ghwangju.scale.neuralnet 
###########################8.경기도      ###############################
gyeonggi.df <- read.csv("경기도.csv",  stringsAsFactors= F)
gyeonggi.scale.df <- as.data.frame(scale(gyeonggi.df,scale=F))
gyeonggi.scale.pca <- PCA(gyeonggi.scale.df, quanti.sup = 1)

gyeonggi.scale.pca.df <- data.frame(gyeonggi.scale.pca$var)
write.csv(gyeonggi.scale.pca.df,"gyeonggi.scale.pca.df.csv")
gyeonggi.scale.lm <- lm(gyeonggi.scale.df[,1]~
                          비금융자산_기타+
                          실질가구처분소득+
                          총전입+
                          실업률+
                          총전출+
                          시도내이동_시군구내+
                          시도간전입+
                          시도내이동_시군구간전입전출+
                          이혼건수+
                          시도간전출+
                          실업자+
                          실거래건설년도+
                          혼인건수+
                          CPI+
                          비경제활동인구+
                          조사망률+
                          X4인세대+
                          X5인세대+
                          경상수지+
                          출생아수+
                          실질GDP성장+
                          금융자산_채무_증권+
                          조혼인율+
                          X6인세대+
                          조출생률+
                          X7인세대+
                          조이혼율+
                          순이동+
                          금융자산_자본+
                          자연증가건수+
                          국제수지_건설수지+
                          고용률+
                          국제수지_경상수지+
                          국제수지_금융서비스수지+
                          자연증가율+
                          사망자수
                        ,data = gyeonggi.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(gyeonggi.scale.lm) 
step(gyeonggi.scale.lm)

gyeonggi.scale.step.lm <- lm(formula = gyeonggi.scale.df[, 1] ~ 
                               실질가구처분소득 + 
                               이혼건수 + 
                               실업자 + 
                               실거래건설년도 + 
                               비경제활동인구 + 
                               X4인세대 + 
                               실질GDP성장 + 
                               조혼인율  + 
                               X7인세대  + 
                               금융자산_자본 + 
                               국제수지_건설수지 + 
                               고용률 + 
                               국제수지_경상수지 + 
                               국제수지_금융서비스수지
                             , data = gyeonggi.scale.df)

summary(gyeonggi.scale.step.lm)
vif(gyeonggi.scale.step.lm) 

gyeonggi.part.scale.df = gyeonggi.scale.df[,c(1,
                                              81	,
                                              10	,
                                              26	,
                                              112	,
                                              27	,
                                              16	,
                                              79	,
                                              9	,
                                              19	,
                                              84	,
                                              104	,
                                              30	,
                                              102	,
                                              105	
                                              
)
]

gyeonggi.part.scale.pca <- PCA(gyeonggi.part.scale.df,quanti.sup = 1)
gyeonggi.part.scale.pca.df <- data.frame(gyeonggi.part.scale.pca$var)
write.csv(gyeonggi.part.scale.pca.df,"gyeonggi.part.scale.pca.df.csv")

#랜덤포레스트
gyeonggi.scale.rnd <- randomForest(gyeonggi.scale.df[,1]~
                                     실질가구처분소득 + 
                                     이혼건수 + 
                                     실업자 + 
                                     실거래건설년도 + 
                                     비경제활동인구 + 
                                     X4인세대 + 
                                     실질GDP성장 + 
                                     조혼인율  + 
                                     X7인세대  + 
                                     금융자산_자본 + 
                                     국제수지_건설수지 + 
                                     고용률 + 
                                     국제수지_경상수지 + 
                                     국제수지_금융서비스수지
                                   , data = gyeonggi.scale.df,importance=T)

as.data.frame(gyeonggi.scale.rnd$importance)[c(order(-as.data.frame(gyeonggi.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ gyeonggi.scale.df[,1] + gyeonggi.scale.rnd$predicted)
varImpPlot(gyeonggi.scale.rnd)
plot(gyeonggi.scale.rnd)

#신경망
gyeonggi.scale.nnet <- nnet(gyeonggi.scale.df[,1]~ 
                              실질가구처분소득 + 
                              이혼건수 + 
                              실업자 + 
                              실거래건설년도 + 
                              비경제활동인구 + 
                              X4인세대 + 
                              실질GDP성장 + 
                              조혼인율  + 
                              X7인세대  + 
                              금융자산_자본 + 
                              국제수지_건설수지 + 
                              고용률 + 
                              국제수지_경상수지 + 
                              국제수지_금융서비스수지
                            ,data=gyeonggi.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
gyeonggi.scale.neuralnet <- neuralnet(gyeonggi.scale.df[,1]~
                                        실질가구처분소득 + 
                                        이혼건수 + 
                                        실업자 + 
                                        실거래건설년도 + 
                                        비경제활동인구 + 
                                        X4인세대 + 
                                        실질GDP성장 + 
                                        조혼인율  + 
                                        X7인세대  + 
                                        금융자산_자본 + 
                                        국제수지_건설수지 + 
                                        고용률 + 
                                        국제수지_경상수지 + 
                                        국제수지_금융서비스수지
                                      ,data=gyeonggi.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

gyeonggi.scale.neuralnet 
###########################9.강원도      ###############################
gangwon.df <- read.csv("강원도.csv",  stringsAsFactors= F)
gangwon.scale.df <- as.data.frame(scale(gangwon.df,scale=F))
gangwon.scale.pca <- PCA(gangwon.scale.df, quanti.sup = 1)

gangwon.scale.pca.df <- data.frame(gangwon.scale.pca$var)
write.csv(gangwon.scale.pca.df,"gangwon.scale.pca.df.csv")

gangwon.scale.lm <- lm(gangwon.scale.df[,1]~
                         혼인건수+
                         비금융자산_기타+
                         실질가구처분소득+
                         조혼인율+
                         시도내이동_시군구내+
                         순이동+
                         X9인세대+
                         실질GDP성장+
                         총전입+
                         실업률+
                         시도간전출+
                         CPI+
                         시도내이동_시군구간전입전출+
                         X20_29세+
                         총전출+
                         실업자+
                         이혼건수+
                         금융자산_채무_증권+
                         금융자산_자본+
                         비경제활동인구+
                         시도간전입+
                         경상수지+
                         고용률+
                         조사망률+
                         조이혼율+
                         사망자수+
                         경제활동참가율+
                         국제수지_건설수지+
                         X15_64세고용률+
                         취업자+
                         GDP단위당1차에너지공급량+
                         X20_29세_남
                       ,data = gangwon.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(gangwon.scale.lm) 
step(gangwon.scale.lm)

gangwon.scale.step.lm <- lm(formula = gangwon.scale.df[, 1] ~ 
                              비금융자산_기타 + 
                              실질가구처분소득 + 
                              조혼인율 + 
                              시도내이동_시군구내 + 
                              총전입 + 
                              실업률 + 
                              CPI + 
                              이혼건수 + 
                              금융자산_자본 + 
                              비경제활동인구 + 
                              X20_29세_남
                            , data = gangwon.scale.df)

summary(gangwon.scale.step.lm)
vif(gangwon.scale.step.lm)

gangwon.part.scale.df = gangwon.scale.df[,c(1,
                                            91 ,
                                            81 ,
                                            9  ,
                                            35 ,
                                            79 ,
                                            32 ,
                                            29 ,
                                            93 ,
                                            10 ,
                                            84 ,
                                            27 ,
                                            5  ,
                                            4  ,
                                            25 ,
                                            56
                                            
)
]
gangwon.part.scale.pca <- PCA(gangwon.part.scale.df,quanti.sup = 1)
gangwon.part.scale.pca.df <- data.frame(gangwon.part.scale.pca$var)
write.csv(gangwon.part.scale.pca.df,"gangwon.part.scale.pca.df.csv")

#랜덤포레스트
gangwon.scale.rnd <- randomForest(gangwon.scale.df[,1]~
                                    비금융자산_기타 + 
                                    실질가구처분소득 + 
                                    조혼인율 + 
                                    시도내이동_시군구내 + 
                                    총전입 + 
                                    실업률 + 
                                    CPI + 
                                    이혼건수 + 
                                    금융자산_자본 + 
                                    비경제활동인구 + 
                                    X20_29세_남
                                  , data = gangwon.scale.df,importance=T)

as.data.frame(gangwon.scale.rnd$importance)[c(order(-as.data.frame(gangwon.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ gangwon.scale.df[,1] + gangwon.scale.rnd$predicted)
varImpPlot(gangwon.scale.rnd)
plot(gangwon.scale.rnd)

#신경망
gangwon.scale.nnet <- nnet(gangwon.scale.df[,1]~ 
                             비금융자산_기타 + 
                             실질가구처분소득 + 
                             조혼인율 + 
                             시도내이동_시군구내 + 
                             총전입 + 
                             실업률 + 
                             CPI + 
                             이혼건수 + 
                             금융자산_자본 + 
                             비경제활동인구 + 
                             X20_29세_남
                           ,data=gangwon.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
gangwon.scale.neuralnet <- neuralnet(gangwon.scale.df[,1]~
                                       비금융자산_기타 + 
                                       실질가구처분소득 + 
                                       조혼인율 + 
                                       시도내이동_시군구내 + 
                                       총전입 + 
                                       실업률 + 
                                       CPI + 
                                       이혼건수 + 
                                       금융자산_자본 + 
                                       비경제활동인구 + 
                                       X20_29세_남
                                     ,data=gangwon.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

gangwon.scale.neuralnet 
###########################10.충청북도 ###############################
chungbuk.df <- read.csv("충청북도.csv",  stringsAsFactors= F)
chungbuk.scale.df <- as.data.frame(scale(chungbuk.df,scale=F))
chungbuk.scale.pca <- PCA(chungbuk.scale.df, quanti.sup = 1)

chungbuk.scale.pca.df <- data.frame(chungbuk.scale.pca$var)
write.csv(chungbuk.scale.pca.df,"chungbuk.scale.pca.df.csv")

chungbuk.scale.lm <- lm(chungbuk.scale.df[,1] ~
                          혼인건수+
                          시도내이동_시군구내+
                          조혼인율+
                          비경제활동인구+
                          시도간전출+
                          시도간전입+
                          총전입+
                          총전출+
                          X40_49세_여+
                          이혼건수+
                          조사망률+
                          조이혼율+
                          사망자수+
                          고용률+
                          X40_49세+
                          실업률+
                          경제활동참가율+
                          실질가구처분소득+
                          실업자+
                          X20_29세_남+
                          X20_29세+
                          실질GDP성장+
                          비금융자산_기타+
                          X40_49세_남+
                          순이동+
                          금융자산_채무_증권+
                          X15_64세고용률+
                          자연증가건수+
                          자연증가율+
                          출생아수+
                          국제수지_건설수지+
                          금융자산_자본+
                          조출생률+
                          X20_29세_여+
                          범죄발생건수+
                          취업자+
                          경제심리지수_순환변동치
                        ,data = chungbuk.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(chungbuk.scale.lm) 
step(chungbuk.scale.lm)

chungbuk.scale.step.lm <- lm(formula = chungbuk.scale.df[, 1] ~  
                               비경제활동인구 + 
                               시도간전출 + 
                               시도간전입 + 
                               X40_49세  + 
                               실질가구처분소득 + 
                               X20_29세_남 + 
                               비금융자산_기타 + 
                               금융자산_채무_증권 + 
                               조출생률 + 
                               범죄발생건수 + 
                               경제심리지수_순환변동치, data = chungbuk.scale.df)

summary(chungbuk.scale.step.lm)
vif(chungbuk.scale.step.lm)

chungbuk.part.scale.df = chungbuk.scale.df[,c(1,
                                              27	,
                                              38	,
                                              37	,
                                              45	,
                                              81	,
                                              56	,
                                              91	,
                                              83	,
                                              3	,
                                              109	,
                                              101	
                                              
                                              
)
]
chungbuk.part.scale.pca <- PCA(chungbuk.part.scale.df,quanti.sup = 1)
chungbuk.part.scale.pca.df <- data.frame(chungbuk.part.scale.pca$var)
write.csv(chungbuk.part.scale.pca.df,"chungbuk.part.scale.pca.df.csv")

#랜덤포레스트
chungbuk.scale.rnd <- randomForest(chungbuk.scale.df[,1]~
                                     비경제활동인구 + 
                                     시도간전출 + 
                                     시도간전입 + 
                                     X40_49세  + 
                                     실질가구처분소득 + 
                                     X20_29세_남 + 
                                     비금융자산_기타 + 
                                     금융자산_채무_증권 + 
                                     조출생률 + 
                                     범죄발생건수
                                     , data = chungbuk.scale.df,importance=T)

as.data.frame(chungbuk.scale.rnd$importance)[c(order(-as.data.frame(chungbuk.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ chungbuk.scale.df[,1] + chungbuk.scale.rnd$predicted)
varImpPlot(chungbuk.scale.rnd)
plot(chungbuk.scale.rnd)


#신경망
chungbuk.scale.nnet <- nnet(chungbuk.scale.df[,1]~ 
                              비경제활동인구 + 
                              시도간전출 + 
                              시도간전입 + 
                              X40_49세  + 
                              실질가구처분소득 + 
                              X20_29세_남 + 
                              비금융자산_기타 + 
                              금융자산_채무_증권 + 
                              조출생률 + 
                              범죄발생건수
                              ,data=chungbuk.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
garson(chungbuk.scale.nnet)$data
chungbuk.scale.neuralnet <- neuralnet(chungbuk.scale.df[,1]~
                                        비경제활동인구 + 
                                        시도간전출 + 
                                        시도간전입 + 
                                        X40_49세  + 
                                        실질가구처분소득 + 
                                        X20_29세_남 + 
                                        비금융자산_기타 + 
                                        금융자산_채무_증권 + 
                                        조출생률 + 
                                        범죄발생건수 + 
                                        ,data=chungbuk.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

chungbuk.scale.neuralnet 
###########################11.충청남도 ###############################
chungnam.df <- read.csv("충청남도.csv",  stringsAsFactors= F)
chungnam.scale.df <- as.data.frame(scale(chungnam.df,scale=F))
chungnam.scale.pca <- PCA(chungnam.scale.df, quanti.sup = 1)

chungnam.scale.pca.df <- data.frame(chungnam.scale.pca$var)
write.csv(chungnam.scale.pca.df,"chungnam.scale.pca.df.csv")

chungnam.scale.lm <- lm(chungnam.scale.df[,1] ~
                          이혼건수+
                          조이혼율+
                          혼인건수+
                          조혼인율+
                          X70_79세_여+
                          X70_79세+
                          조사망률+
                          시도간전출+
                          총전출+
                          시도내이동_시군구내+
                          총전입+
                          사망자수+
                          고용률+
                          금융자산_채무_증권+
                          실업률+
                          비경제활동인구+
                          시도내이동_시군구간전입전출+
                          금융자산_자본+
                          시도간전입+
                          실질GDP성장+
                          자연증가건수+
                          자연증가율+
                          경제활동참가율+
                          X15_64세고용률+
                          범죄발생건수+
                          총인구수_여+
                          연령구간인구수_여+
                          조출생률+
                          출생아수+
                          실업자+
                          총인구수+
                          연령구간인구수+
                          총인구수_남+
                          연령구간인구수_남+
                          국제수지_건설수지+
                          순이동+
                          X20_29세_남+
                          비금융자산_기타
                        ,data = chungnam.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(chungnam.scale.lm) 
step(chungnam.scale.lm)

chungnam.scale.step.lm <- lm(formula = chungnam.scale.df[, 1] ~ 이혼건수 + 
                               조혼인율 + X70_79세 + 총전입 + 사망자수 + 
                               금융자산_자본 + 자연증가건수  + X15_64세고용률 + 
                               범죄발생건수 + 실업자 + 총인구수, data = chungnam.scale.df)


summary(chungnam.scale.step.lm)
vif(chungnam.scale.step.lm)

chungnam.part.scale.df = chungnam.scale.df[,c(1,
                                              10	,
                                              9	,
                                              48	,
                                              32	,
                                              4	,
                                              84	,
                                              6	,
                                              31	,
                                              109	,
                                              26	,
                                              39	
                                              
)
]
chungnam.part.scale.pca <- PCA(chungnam.part.scale.df,quanti.sup = 1)
chungnam.part.scale.pca.df <- data.frame(chungnam.part.scale.pca$var)
write.csv(chungnam.part.scale.pca.df,"chungnam.part.scale.pca.df.csv")

#랜덤포레스트
chungnam.scale.rnd <- randomForest(chungnam.scale.df[,1]~
                                     이혼건수 + 
                                     조혼인율 + X70_79세 + 총전입 + 사망자수 + 
                                     금융자산_자본 + 자연증가건수  + X15_64세고용률 + 
                                     범죄발생건수 + 실업자 + 총인구수
                                   , data = chungnam.scale.df,importance=T)

as.data.frame(chungnam.scale.rnd$importance)[c(order(-as.data.frame(chungnam.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ chungnam.scale.df[,1] + chungnam.scale.rnd$predicted)
varImpPlot(chungnam.scale.rnd)
plot(chungnam.scale.rnd)

#신경망
chungnam.scale.nnet <- nnet(chungnam.scale.df[,1]~ 
                              이혼건수 + 
                              조혼인율 + X70_79세 + 총전입 + 사망자수 + 
                              금융자산_자본 + 자연증가건수  + X15_64세고용률 + 
                              범죄발생건수 + 실업자 + 총인구수
                            ,data=chungnam.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
chungnam.scale.neuralnet <- neuralnet(chungnam.scale.df[,1]~
                                        이혼건수 + 
                                        조혼인율 + X70_79세 + 총전입 + 사망자수 + 
                                        금융자산_자본 + 자연증가건수  + X15_64세고용률 + 
                                        범죄발생건수 + 실업자 + 총인구수
                                      ,data=chungnam.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

chungnam.scale.neuralnet 
###########################12.전라북도 ###############################
junbuk.df <- read.csv("전라북도.csv",  stringsAsFactors= F)
junbuk.scale.df <- as.data.frame(scale(junbuk.df,scale=F))
junbuk.scale.pca <- PCA(junbuk.scale.df, quanti.sup = 1)

junbuk.scale.pca.df <- data.frame(junbuk.scale.pca$var)
write.csv(junbuk.scale.pca.df,"junbuk.scale.pca.df.csv")

junbuk.scale.lm <- lm(junbuk.scale.df[,1]~
                        비금융자산_기타+
                        실질가구처분소득+
                        이혼건수+
                        조이혼율+
                        실거래거래건수+
                        실거래건설년도+
                        X40_49세_여+
                        비경제활동인구+
                        고용률+
                        시도내이동_시군구간전입전출+
                        X40_49세+
                        순이동+
                        시도간전출+
                        실질GDP성장+
                        경제활동참가율+
                        총전입+
                        X40_49세_남+
                        총전출+
                        CPI+
                        총인구수+
                        연령구간인구수+
                        총인구수_여+
                        연령구간인구수_여+
                        X10인세대이상+
                        총인구수_남+
                        연령구간인구수_남+
                        시도간전입+
                        혼인건수+
                        X15_64세고용률+
                        조사망률+
                        경상수지+
                        사망자수+
                        조혼인율+
                        시도내이동_시군구내+
                        금융자산_자본+
                        취업자+
                        금융자산_채무_증권+
                        금융자산_통화_예금+
                        실업률+
                        조출생률
                      ,data = junbuk.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(junbuk.scale.lm) 
step(junbuk.scale.lm)

junbuk.scale.step.lm <- lm(formula = junbuk.scale.df[, 1] ~ 실질가구처분소득 + 실거래거래건수  
                           + 시도내이동_시군구간전입전출 + X40_49세 + 실질GDP성장
                           + 총전입 + 총인구수_여 + X10인세대이상 + 
                             조사망률 + 경상수지 + 금융자산_자본 + 실업률, data = junbuk.scale.df)

summary(junbuk.scale.step.lm)
vif(junbuk.scale.step.lm)

junbuk.part.scale.df = junbuk.scale.df[,c(1,
                                          81	,
                                          110	,
                                          36	,
                                          45	,
                                          79	,
                                          32	,
                                          65	,
                                          22	,
                                          5	,
                                          92	,
                                          84	,
                                          29	
                                          
)
]
junbuk.part.scale.pca <- PCA(junbuk.part.scale.df,quanti.sup = 1)
junbuk.part.scale.pca.df <- data.frame(junbuk.part.scale.pca$var)
write.csv(junbuk.part.scale.pca.df,"junbuk.part.scale.pca.df.csv")

#랜덤포레스트
junbuk.scale.rnd <- randomForest(junbuk.scale.df[,1]~
                                   실질가구처분소득 + 실거래거래건수  
                                 + 시도내이동_시군구간전입전출 + X40_49세 + 실질GDP성장
                                 + 총전입 + 총인구수_여 + X10인세대이상 + 
                                   조사망률 + 경상수지 + 금융자산_자본 + 실업률
                                 , data = junbuk.scale.df,importance=T)

as.data.frame(junbuk.scale.rnd$importance)[c(order(-as.data.frame(junbuk.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ junbuk.scale.df[,1] + junbuk.scale.rnd$predicted)
varImpPlot(junbuk.scale.rnd)
plot(junbuk.scale.rnd)

#신경망
junbuk.scale.nnet <- nnet(junbuk.scale.df[,1]~ 
                            실질가구처분소득 + 실거래거래건수  
                          + 시도내이동_시군구간전입전출 + X40_49세 + 실질GDP성장
                          + 총전입 + 총인구수_여 + X10인세대이상 + 
                            조사망률 + 경상수지 + 금융자산_자본 + 실업률
                          ,data=junbuk.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
junbuk.scale.neuralnet <- neuralnet(junbuk.scale.df[,1]~
                                      실질가구처분소득 + 실거래거래건수  
                                    + 시도내이동_시군구간전입전출 + X40_49세 + 실질GDP성장
                                    + 총전입 + 총인구수_여 + X10인세대이상 + 
                                      조사망률 + 경상수지 + 금융자산_자본 + 실업률
                                    ,data=junbuk.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

junbuk.scale.neuralnet 
###########################13.전라남도 ###############################
junnam.df <- read.csv("전라남도.csv",  stringsAsFactors= F)
junnam.scale.df <- as.data.frame(scale(junnam.df,scale=F))
junnam.scale.pca <- PCA(junnam.scale.df, quanti.sup = 1)

junnam.scale.pca.df <- data.frame(junnam.scale.pca$var)
write.csv(junnam.scale.pca.df,"junnam.scale.pca.df.csv")

junnam.scale.lm <- lm(junnam.scale.df[,1] ~
                        실질가구처분소득+
                        순이동+
                        비금융자산_기타+
                        조이혼율+
                        이혼건수+
                        범죄발생건수+
                        X10인세대이상+
                        시도간전입+
                        총전입+
                        혼인건수+
                        고용률+
                        시도내이동_시군구간전입전출+
                        X60_69세_여+
                        조혼인율+
                        X15_64세고용률+
                        실질GDP성장+
                        총전출+
                        금융자산_자본+
                        비경제활동인구+
                        시도간전출+
                        경제활동참가율+
                        시도내이동_시군구내+
                        금융자산_채무_증권+
                        CPI+
                        X70_79세_여+
                        조사망률+
                        취업자+
                        사망자수+
                        조출생률+
                        국제수지_건설수지+
                        경상수지+
                        실업률+
                        X60_69세+
                        X100세이상_남+
                        출생아수+
                        자연증가율+
                        자연증가건수+
                        실업자
                      ,data = junnam.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(junnam.scale.lm) 
step(junnam.scale.lm)

junnam.scale.step.lm <- lm(formula = junnam.scale.df[, 1] ~ 
                             실질가구처분소득 + 
                             조이혼율 +
                             총전입  + 
                             조혼인율 + 
                             X15_64세고용률 + 
                             실질GDP성장 + 
                             조사망률+ 
                             국제수지_건설수지 + 
                             경상수지 + 
                             실업률 + 
                             X60_69세 + 
                             X100세이상_남
                           ,  data = junnam.scale.df)

summary(junnam.scale.step.lm)
vif(junnam.scale.step.lm)

junnam.part.scale.df = junnam.scale.df[,c(1,
                                          81	,
                                          11	,
                                          32	,
                                          9	,
                                          31	,
                                          79	,
                                          5	,
                                          104	,
                                          92	,
                                          29	,
                                          47	,
                                          64	
                                          
)
]
junnam.part.scale.pca <- PCA(junnam.part.scale.df,quanti.sup = 1)
junnam.part.scale.pca.df <- data.frame(junnam.part.scale.pca$var)
write.csv(junnam.part.scale.pca.df,"junnam.part.scale.pca.df.csv")

#랜덤포레스트
junnam.scale.rnd <- randomForest(junnam.scale.df[,1]~
                                   실질가구처분소득 + 
                                   조이혼율 +
                                   총전입  + 
                                   조혼인율 + 
                                   X15_64세고용률 + 
                                   실질GDP성장 + 
                                   조사망률+ 
                                   국제수지_건설수지 + 
                                   경상수지 + 
                                   실업률 + 
                                   X60_69세 + 
                                   X100세이상_남
                                 , data = junnam.scale.df,importance=T)

as.data.frame(junnam.scale.rnd$importance)[c(order(-as.data.frame(junnam.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ junnam.scale.df[,1] + junnam.scale.rnd$predicted)
varImpPlot(junnam.scale.rnd)
plot(junnam.scale.rnd)

#신경망
junnam.scale.nnet <- nnet(junnam.scale.df[,1]~ 
                            실질가구처분소득 + 
                            조이혼율 +
                            총전입  + 
                            조혼인율 + 
                            X15_64세고용률 + 
                            실질GDP성장 + 
                            조사망률+ 
                            국제수지_건설수지 + 
                            경상수지 + 
                            실업률 + 
                            X60_69세 + 
                            X100세이상_남
                          ,data=junnam.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
junnam.scale.neuralnet <- neuralnet(junnam.scale.df[,1]~
                                      실질가구처분소득 + 
                                      조이혼율 +
                                      총전입  + 
                                      조혼인율 + 
                                      X15_64세고용률 + 
                                      실질GDP성장 + 
                                      조사망률+ 
                                      국제수지_건설수지 + 
                                      경상수지 + 
                                      실업률 + 
                                      X60_69세 + 
                                      X100세이상_남
                                    ,data=junnam.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

junnam.scale.neuralnet 
###########################14.경상북도 ###############################
gyeongbuk.df <- read.csv("경상북도.csv",  stringsAsFactors= F)
gyeongbuk.scale.df <- as.data.frame(scale(gyeongbuk.df,scale=F))
gyeongbuk.scale.pca <- PCA(gyeongbuk.scale.df, quanti.sup = 1)

gyeongbuk.scale.pca.df <- data.frame(gyeongbuk.scale.pca$var)
write.csv(gyeongbuk.scale.pca.df,"gyeongbuk.scale.pca.df.csv")

gyeongbuk.scale.lm <- lm(gyeongbuk.scale.df[,1]~
                           실질가구처분소득+
                           비금융자산_기타+
                           혼인건수+
                           시도내이동_시군구내+
                           조혼인율+
                           총전입+
                           실거래건설년도+
                           실업률+
                           시도내이동_시군구간전입전출+
                           총전출+
                           고용률+
                           실업자+
                           시도간전입+
                           사망자수+
                           비경제활동인구+
                           경제활동참가율+
                           조사망률+
                           순이동+
                           CPI+
                           자연증가율+
                           자연증가건수+
                           시도간전출+
                           X15_64세고용률+
                           이혼건수+
                           실질GDP성장+
                           금융자산_자본+
                           조이혼율+
                           금융자산_채무_증권+
                           경상수지+
                           X70_79세_여+
                           취업자+
                           X10인세대이상+
                           조출생률+
                           실거래거래건수+
                           총인구수_여+
                           연령구간인구수_여+
                           총인구수+
                           연령구간인구수+
                           범죄발생건수+
                           총인구수_남+
                           연령구간인구수_남+
                           출생아수+
                           경제활동인구
                         ,data = gyeongbuk.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(gyeongbuk.scale.lm) 
step(gyeongbuk.scale.lm)

gyeongbuk.scale.step.lm <- lm(formula = gyeongbuk.scale.df[, 1] ~ 실질가구처분소득 
                              + 실거래건설년도 + 시도내이동_시군구간전입전출 + 
                                실업자 + 사망자수 + 이혼건수 + 실질GDP성장 + 금융자산_자본 + 조이혼율 + 
                                금융자산_채무_증권 + 경상수지 + 취업자 + 총인구수_여 + 
                                범죄발생건수, data = gyeongbuk.scale.df)

summary(gyeongbuk.scale.step.lm)
vif(gyeongbuk.scale.step.lm)

gyeongbuk.part.scale.df = gyeongbuk.scale.df[,c(1,
                                                81	,
                                                112	,
                                                36	,
                                                26	,
                                                4	,
                                                10	,
                                                79	,
                                                84	,
                                                11	,
                                                83	,
                                                92	,
                                                25	,
                                                65	,
                                                109	
)
]
gyeongbuk.part.scale.pca <- PCA(gyeongbuk.part.scale.df,quanti.sup = 1)
gyeongbuk.part.scale.pca.df <- data.frame(gyeongbuk.part.scale.pca$var)
write.csv(gyeongbuk.part.scale.pca.df,"gyeongbuk.part.scale.pca.df.csv")

#랜덤포레스트
gyeongbuk.scale.rnd <- randomForest(gyeongbuk.scale.df[,1]~
                                      실질가구처분소득 
                                    + 실거래건설년도 + 시도내이동_시군구간전입전출 + 
                                      실업자 + 사망자수 + 이혼건수 + 실질GDP성장 + 금융자산_자본 + 조이혼율 + 
                                      금융자산_채무_증권 + 경상수지 + 취업자 + 총인구수_여 + 
                                      범죄발생건수
                                    , data = gyeongbuk.scale.df,importance=T)

as.data.frame(gyeongbuk.scale.rnd$importance)[c(order(-as.data.frame(gyeongbuk.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ gyeongbuk.scale.df[,1] + gyeongbuk.scale.rnd$predicted)
varImpPlot(gyeongbuk.scale.rnd)
plot(gyeongbuk.scale.rnd)

#신경망
gyeongbuk.scale.nnet <- nnet(gyeongbuk.scale.df[,1]~ 
                               실질가구처분소득 
                             + 실거래건설년도 + 시도내이동_시군구간전입전출 + 
                               실업자 + 사망자수 + 이혼건수 + 실질GDP성장 + 금융자산_자본 + 조이혼율 + 
                               금융자산_채무_증권 + 경상수지 + 취업자 + 총인구수_여 + 
                               범죄발생건수
                             ,data=gyeongbuk.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
gyeongbuk.scale.neuralnet <- neuralnet(gyeongbuk.scale.df[,1]~
                                         실질가구처분소득 
                                       + 실거래건설년도 + 시도내이동_시군구간전입전출 + 
                                         실업자 + 사망자수 + 이혼건수 + 실질GDP성장 + 금융자산_자본 + 조이혼율 + 
                                         금융자산_채무_증권 + 경상수지 + 취업자 + 총인구수_여 + 
                                         범죄발생건수
                                       ,data=gyeongbuk.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

gyeongbuk.scale.neuralnet 
###########################15.경상남도 ###############################
gyeongnam.df <- read.csv("경상남도.csv",  stringsAsFactors= F)
gyeongnam.scale.df <- as.data.frame(scale(gyeongnam.df,scale=F))
gyeongnam.scale.pca <- PCA(gyeongnam.scale.df, quanti.sup = 1)

gyeongnam.scale.pca.df <- data.frame(gyeongnam.scale.pca$var)
write.csv(gyeongnam.scale.pca.df,"gyeongnam.scale.pca.df.csv")

gyeongnam.scale.lm <- lm(gyeongnam.scale.df[,1]~
                           비금융자산_기타+
                           시도내이동_시군구내+
                           총전입+
                           고용률+
                           실질가구처분소득+
                           순이동+
                           혼인건수+
                           시도간전입+
                           총전출+
                           시도간전출+
                           X40_49세_여+
                           조혼인율+
                           조사망률+
                           X40_49세+
                           경제활동참가율+
                           비경제활동인구+
                           X40_49세_남+
                           이혼건수+
                           실거래거래건수+
                           X20_29세_남+
                           시도내이동_시군구간전입전출+
                           실질GDP성장+
                           사망자수+
                           실업률+
                           조이혼율+
                           CPI+
                           금융자산_채무_증권+
                           X15_64세고용률+
                           경상수지+
                           실업자+
                           X0_9세_여+
                           금융자산_자본+
                           범죄발생건수+
                           X20_29세+
                           국제수지_건설수지
                         ,data = gyeongnam.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(gyeongnam.scale.lm) 
step(gyeongnam.scale.lm)

gyeongnam.scale.step.lm <- lm(formula = gyeongnam.scale.df[, 1] ~ 
                                시도내이동_시군구내  + 
                                실질가구처분소득 + 
                                순이동 + 
                                시도간전입 +
                                X40_49세_여 + 
                                조혼인율 + 
                                비경제활동인구 + 
                                실거래거래건수 + 
                                실질GDP성장 + 
                                실업률 + 
                                CPI + 
                                X15_64세고용률 + 
                                X0_9세_여 + 
                                국제수지_건설수지
                              , data = gyeongnam.scale.df)

summary(gyeongnam.scale.step.lm)
vif(gyeongnam.scale.step.lm)

gyeongnam.part.scale.df = gyeongnam.scale.df[,c(1,
                                                35	,
                                                81	,
                                                34	,
                                                37	,
                                                71	,
                                                9	,
                                                27	,
                                                110	,
                                                79	,
                                                29	,
                                                93	,
                                                31	,
                                                67	,
                                                104	
                                                
)
]
gyeongnam.part.scale.pca <- PCA(gyeongnam.part.scale.df,quanti.sup = 1)
gyeongnam.part.scale.pca.df <- data.frame(gyeongnam.part.scale.pca$var)
write.csv(gyeongnam.part.scale.pca.df,"gyeongnam.part.scale.pca.df.csv")

#랜덤포레스트
gyeongnam.scale.rnd <- randomForest(gyeongnam.scale.df[,1]~
                                      시도내이동_시군구내  + 
                                      실질가구처분소득 + 
                                      순이동 + 
                                      시도간전입 +
                                      X40_49세_여 + 
                                      조혼인율 + 
                                      비경제활동인구 + 
                                      실거래거래건수 + 
                                      실질GDP성장 + 
                                      실업률 + 
                                      CPI + 
                                      X15_64세고용률 + 
                                      X0_9세_여 + 
                                      국제수지_건설수지
                                    , data = gyeongnam.scale.df,importance=T)

as.data.frame(gyeongnam.scale.rnd$importance)[c(order(-as.data.frame(gyeongnam.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ gyeongnam.scale.df[,1] + gyeongnam.scale.rnd$predicted)
varImpPlot(gyeongnam.scale.rnd)
plot(gyeongnam.scale.rnd)

#신경망
gyeongnam.scale.nnet <- nnet(gyeongnam.scale.df[,1]~ 
                               시도내이동_시군구내  + 
                               실질가구처분소득 + 
                               순이동 + 
                               시도간전입 +
                               X40_49세_여 + 
                               조혼인율 + 
                               비경제활동인구 + 
                               실거래거래건수 + 
                               실질GDP성장 + 
                               실업률 + 
                               CPI + 
                               X15_64세고용률 + 
                               X0_9세_여 + 
                               국제수지_건설수지
                             ,data=gyeongnam.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
gyeongnam.scale.neuralnet <- neuralnet(gyeongnam.scale.df[,1]~
                                         시도내이동_시군구내  + 
                                         실질가구처분소득 + 
                                         순이동 + 
                                         시도간전입 +
                                         X40_49세_여 + 
                                         조혼인율 + 
                                         비경제활동인구 + 
                                         실거래거래건수 + 
                                         실질GDP성장 + 
                                         실업률 + 
                                         CPI + 
                                         X15_64세고용률 + 
                                         X0_9세_여 + 
                                         국제수지_건설수지
                                       ,data=gyeongnam.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

gyeongnam.scale.neuralnet 
###########################16.세종1 ###############################
sejong1.df <- read.csv("세종1.csv",  stringsAsFactors= F)
sejong1.scale.df <- as.data.frame(scale(sejong1.df,scale=F))
sejong1.scale.df
sejong1.scale.pca <- PCA(sejong1.scale.df, quanti.sup = 1)
sejong1.scale.pca.df <- data.frame(sejong1.scale.pca$var)
write.csv(sejong1.scale.pca.df,"sejong1.scale.pca.df.csv")

sejong1.scale.lm <- lm(sejong1.scale.df[,1] ~
                         비경제활동인구+
                         순이동+
                         금융자산_생명보험_연금+
                         실질GDP성장+
                         실업률+
                         시도간전입+
                         경제심리지수_순환변동치+
                         실업자+
                         총전입+
                         X15_64세고용률+
                         CPI+
                         GDP단위당1차에너지공급량+
                         경제심리지수_원계열+
                         경상수지+
                         금융자산_통화_예금+
                         X100세이상+
                         실질가구처분소득+
                         조혼인율+
                         X15세이상인구+
                         고용률+
                         비금융자산_기타+
                         금융자산_자본+
                         국제수지_투자소득지급+
                         조이혼율+
                         경제활동참가율+
                         조출생률+
                         경제활동인구+
                         취업자
                       ,data = sejong1.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(sejong1.scale.lm) 
step(sejong1.scale.lm)

sejong1.scale.step.lm <- lm(formula = sejong1.scale.df[, 1] ~ 
                              금융자산_생명보험_연금 + 
                              실업률 + 
                              X15_64세고용률 + 
                              CPI + 
                              GDP단위당1차에너지공급량 + 
                              경제심리지수_원계열 +
                              X100세이상 + 
                              경제활동인구, 
                            data = sejong1.scale.df)

summary(sejong1.scale.step.lm)
vif(sejong1.scale.step.lm)

sejong1.part.scale.df = sejong1.scale.df[,c(1,
                                            86	,
                                            29	,
                                            31	,
                                            93	,
                                            96	,
                                            100	,
                                            51	,
                                            24	
                                            
)
]
sejong1.part.scale.pca <- PCA(sejong1.part.scale.df,quanti.sup = 1)
sejong1.part.scale.pca.df <- data.frame(sejong1.part.scale.pca$var)
write.csv(sejong1.part.scale.pca.df,"sejong1.part.scale.pca.df.csv")

#랜덤포레스트
sejong1.scale.rnd <- randomForest(sejong1.scale.df[,1]~
                                    금융자산_생명보험_연금 + 
                                    실업률 + 
                                    X15_64세고용률 + 
                                    CPI + 
                                    GDP단위당1차에너지공급량 + 
                                    경제심리지수_원계열 +
                                    X100세이상 + 
                                    경제활동인구
                                  , data = sejong1.scale.df,importance=T)

as.data.frame(sejong1.scale.rnd$importance)[c(order(-as.data.frame(sejong1.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ sejong1.scale.df[,1] + sejong1.scale.rnd$predicted)
varImpPlot(sejong1.scale.rnd)
plot(sejong1.scale.rnd)

#신경망
sejong1.scale.nnet <- nnet(sejong1.scale.df[,1]~ 
                             금융자산_생명보험_연금 + 
                             실업률 + 
                             X15_64세고용률 + 
                             CPI + 
                             GDP단위당1차에너지공급량 + 
                             경제심리지수_원계열 +
                             X100세이상 + 
                             경제활동인구
                           ,data=sejong1.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
sejong1.scale.neuralnet <- neuralnet(sejong1.scale.df[,1]~
                                       금융자산_생명보험_연금 + 
                                       실업률 + 
                                       X15_64세고용률 + 
                                       CPI + 
                                       GDP단위당1차에너지공급량 + 
                                       경제심리지수_원계열 +
                                       X100세이상 + 
                                       경제활동인구
                                     ,data=sejong1.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

sejong1.scale.neuralnet 
###########################17.세종2 ###############################
sejong2.df <- read.csv("세종2.csv",  stringsAsFactors= F)
sejong2.scale.df <- as.data.frame(scale(sejong2.df,scale=F))
sejong2.scale.pca <- PCA(sejong2.scale.df, quanti.sup = 1)

sejong2.scale.pca.df <- data.frame(sejong2.scale.pca$var)
write.csv(sejong2.scale.pca.df,"sejong2.scale.pca.df.csv")

sejong2.scale.lm <- lm(sejong2.scale.df[,1] ~
                         원유.생산+
                         실질GDP성장+
                         실질가구처분소득+
                         금융자산_생명보험_연금+
                         순이동+
                         금융자산_자본+
                         시도간전입+
                         경제심리지수_순환변동치+
                         X100세이상+
                         경제심리지수_원계열+
                         총전입+
                         경상수지+
                         CPI+
                         금융자산_통화_예금+
                         비금융자산_기타+
                         GDP단위당1차에너지공급량+
                         조혼인율+
                         조이혼율+
                         국제수지_비화폐용금수지+
                         조출생률+
                         시도내이동_시군구내+
                         에너지공급_재생에너지+
                         X100세이상_여+
                         금융자산_투자펀드_주식
                       ,data = sejong2.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(sejong2.scale.lm) 
step(sejong2.scale.lm)

sejong2.scale.step.lm <- lm(formula = sejong2.scale.df[, 1] ~ 
                              실질가구처분소득 + 
                              금융자산_생명보험_연금 + 
                              순이동 + X100세이상 + 
                              경제심리지수_원계열 + 
                              X100세이상_여
                            , data = sejong2.scale.df)

summary(sejong2.scale.step.lm)
vif(sejong2.scale.step.lm)

sejong2.part.scale.df = sejong2.scale.df[,c(1,
                                            72	,
                                            77	,
                                            25	,
                                            42	,
                                            91	,
                                            68	
)
]
sejong2.part.scale.pca <- PCA(sejong2.part.scale.df,quanti.sup = 1)
sejong2.part.scale.pca.df <- data.frame(sejong2.part.scale.pca$var)
write.csv(sejong2.part.scale.pca.df,"sejong2.part.scale.pca.df.csv")

#랜덤포레스트
sejong2.scale.rnd <- randomForest(sejong2.scale.df[,1]~
                                    실질가구처분소득 + 
                                    금융자산_생명보험_연금 + 
                                    순이동 + X100세이상 + 
                                    경제심리지수_원계열 + 
                                    X100세이상_여
                                  , data = sejong2.scale.df,importance=T)

as.data.frame(sejong2.scale.rnd$importance)[c(order(-as.data.frame(sejong2.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ sejong2.scale.df[,1] + sejong2.scale.rnd$predicted)
varImpPlot(sejong2.scale.rnd)
plot(sejong2.scale.rnd)

#신경망
sejong2.scale.nnet <- nnet(sejong2.scale.df[,1]~ 
                             실질가구처분소득 + 
                             금융자산_생명보험_연금 + 
                             순이동 + X100세이상 + 
                             경제심리지수_원계열 + 
                             X100세이상_여
                           ,data=sejong2.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
sejong2.scale.neuralnet <- neuralnet(sejong2.scale.df[,1]~
                                       실질가구처분소득 + 
                                       금융자산_생명보험_연금 + 
                                       순이동 + X100세이상 + 
                                       경제심리지수_원계열 + 
                                       X100세이상_여
                                     ,data=sejong2.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

sejong2.scale.neuralnet 
###########################18.제주 ###############################
jeju.df <- read.csv("제주특별자치도.csv",  stringsAsFactors= F)
jeju.scale.df <- as.data.frame(scale(jeju.df,scale=F))
jeju.scale.pca <- PCA(jeju.scale.df, quanti.sup = 1)

jeju.scale.pca.df <- data.frame(jeju.scale.pca$var)
write.csv(jeju.scale.pca.df,"jeju.scale.pca.df.csv")

jeju.scale.lm <- lm(jeju.scale.df[,1]~
                      비금융자산_기타+
                      실질가구처분소득+
                      조이혼율+
                      이혼건수+
                      시도내이동_시군구내+
                      총전출+
                      시도내이동_시군구간전입전출+
                      총전입+
                      조사망률+
                      혼인건수+
                      실거래거래건수+
                      실업률+
                      조혼인율+
                      출생아수+
                      실질GDP성장+
                      비경제활동인구+
                      시도간전출+
                      X30_39세_여+
                      CPI+
                      X0_9세_남+
                      X30_39세+
                      실업자+
                      경상수지+
                      자연증가건수+
                      사망자수+
                      국제수지_건설수지+
                      금융자산_자본+
                      금융자산_채무_증권+
                      조출생률+
                      국제수지_금융서비스수지+
                      X30_39세_남+
                      X100세이상_남+
                      X0_9세+
                      자연증가율
                    ,data = jeju.scale.df)
#R  0.9663 
#p-value 2.2e-16
summary(jeju.scale.lm) 
step(jeju.scale.lm)

jeju.scale.step.lm <- lm(formula = jeju.scale.df[, 1] ~ 
                           비금융자산_기타 + 
                           조이혼율 + 
                           시도내이동_시군구내 + 
                           혼인건수 + 
                           X30_39세_여 + 
                           X0_9세_남 + 
                           자연증가건수 + 
                           사망자수 + 
                           국제수지_건설수지 + 
                           금융자산_자본 + 
                           금융자산_채무_증권 + 
                           X100세이상_남, data = jeju.scale.df)

summary(jeju.scale.step.lm)
vif(jeju.scale.step.lm)

jeju.part.scale.df = jeju.scale.df[,c(1,
                                      91	,
                                      11	,
                                      35	,
                                      8	,
                                      70	,
                                      54	,
                                      6	,
                                      4	,
                                      104	,
                                      84	,
                                      83	,
                                      64	
                                      
)
]
jeju.part.scale.pca <- PCA(jeju.part.scale.df,quanti.sup = 1)
jeju.part.scale.pca.df <- data.frame(jeju.part.scale.pca$var)
write.csv(jeju.part.scale.pca.df,"jeju.part.scale.pca.df.csv")

#랜덤포레스트
jeju.scale.rnd <- randomForest(jeju.scale.df[,1]~
                                 비금융자산_기타+
                                 실질가구처분소득+
                                 조이혼율+
                                 이혼건수+
                                 시도내이동_시군구내+
                                 총전출+
                                 시도내이동_시군구간전입전출+
                                 총전입+
                                 조사망률+
                                 혼인건수+
                                 실거래거래건수+
                                 실업률+
                                 조혼인율+
                                 출생아수+
                                 실질GDP성장+
                                 비경제활동인구+
                                 시도간전출+
                                 X30_39세_여+
                                 CPI+
                                 X0_9세_남+
                                 X30_39세+
                                 실업자+
                                 경상수지+
                                 자연증가건수+
                                 사망자수+
                                 국제수지_건설수지+
                                 금융자산_자본+
                                 금융자산_채무_증권+
                                 조출생률+
                                 국제수지_금융서비스수지+
                                 X30_39세_남+
                                 X100세이상_남+
                                 X0_9세+
                                 자연증가율
                               , data = jeju.scale.df,importance=T)

as.data.frame(jeju.scale.rnd$importance)[c(order(-as.data.frame(jeju.scale.rnd$importance)$IncNodePurity)),]
xtabs(~ jeju.scale.df[,1] + jeju.scale.rnd$predicted)
varImpPlot(jeju.scale.rnd)
plot(jeju.scale.rnd)

#신경망
jeju.scale.nnet <- nnet(jeju.scale.df[,1]~ 
                          비금융자산_기타+
                          실질가구처분소득+
                          조이혼율+
                          이혼건수+
                          시도내이동_시군구내+
                          총전출+
                          시도내이동_시군구간전입전출+
                          총전입+
                          조사망률+
                          혼인건수+
                          실거래거래건수+
                          실업률+
                          조혼인율+
                          출생아수+
                          실질GDP성장+
                          비경제활동인구+
                          시도간전출+
                          X30_39세_여+
                          CPI+
                          X0_9세_남+
                          X30_39세+
                          실업자+
                          경상수지+
                          자연증가건수+
                          사망자수+
                          국제수지_건설수지+
                          금융자산_자본+
                          금융자산_채무_증권+
                          조출생률+
                          국제수지_금융서비스수지+
                          X30_39세_남+
                          X100세이상_남+
                          X0_9세+
                          자연증가율
                        ,data=jeju.scale.df,size=5,decay=5e-04,maxit=1000)#옵션체크
jeju.scale.neuralnet <- neuralnet(jeju.scale.df[,1]~
                                    비금융자산_기타+
                                    실질가구처분소득+
                                    조이혼율+
                                    이혼건수+
                                    시도내이동_시군구내+
                                    총전출+
                                    시도내이동_시군구간전입전출+
                                    총전입+
                                    조사망률+
                                    혼인건수+
                                    실거래거래건수+
                                    실업률+
                                    조혼인율+
                                    출생아수+
                                    실질GDP성장+
                                    비경제활동인구+
                                    시도간전출+
                                    X30_39세_여+
                                    CPI+
                                    X0_9세_남+
                                    X30_39세+
                                    실업자+
                                    경상수지+
                                    자연증가건수+
                                    사망자수+
                                    국제수지_건설수지+
                                    금융자산_자본+
                                    금융자산_채무_증권+
                                    조출생률+
                                    국제수지_금융서비스수지+
                                    X30_39세_남+
                                    X100세이상_남+
                                    X0_9세+
                                    자연증가율
                                  ,data=jeju.scale.df, hidden=5,linear.output=FALSE, likelihood=TRUE)#옵션체크

jeju.scale.neuralnet 


