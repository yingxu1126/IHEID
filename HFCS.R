memory.limit(102400)
rm(list=ls())
library(dplyr)
setwd("E:/iheid/paper proposal/consumer credit/DATA/ECB data/HFCS")


#D2= read.csv("wave3/D2.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
#save(D2,file='D2.RData')
#D3= read.csv("wave3/D3.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
#save(D3,file='D3.RData')
#D4= read.csv("wave3/D4.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
#save(D4,file='D4.RData')
#D5= read.csv("wave3/D5.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
#save(D5,file='D5.RData')
#W= read.csv("wave3/W.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
#save(W,file='W.RData')
#load('D2.RData')
#load('D3.RData')
#load('D4.RData')
#load('D5.RData')
#load('W.RData')

#library('survey')
#repweg= select(W, "wr0001":"wr1000")
#hfcs.design = svrepdesign(repweights=repweg, weights= ~HW0010, data=imputationList(list(imp1,imp2,imp3,imp4,imp5)),
# scale=1,rscale=rep(1/999,1000),mse=FALSE,type='other', combined.weights=TRUE)
#install.packages('mitools')
#MIcombine(with(hfcs.design,svymean(~ HB0100)))

####wave3###
H1= read.csv("wave3/H1.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
save(H1,file='H1.RData')
HN1= read.csv("wave3/HN1.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
save(HN1,file='HN1.RData')
D1= read.csv("wave3/D1.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
save(D1,file='D1.RData')
load('H1.RData')
load('HN1.RData')
load('D1.RData')
wave3<-H1[,c("ID","SA0010","SA0110","SA0100","IM0100","HB0100","HB0500","HB0600","HB0700","HB0900",
             "HB1000","HB1010","HB2300","HB1301","HB1302","HB1303","HB1601","HB1602","HB1603",
             ##"HB33011","HB33012","HB33013","HB33021","HB33022","HB33023","HB33031","HB33032","HB33033", mgg for other properties
             ##"HB36011","HB36012","HB36013","HB36021","HB36022","HB36023","HB36031","HB36032","HB36033",
             "HC0110","HC0351a","HC0352a","HC0353a","HC0361","HC0362","HC0363","HC0501a","HC0502a",
             "HC0503a","HC0801","HC0802","HC0803","HB1701","HB1702","HB1703","HB1901","HB1902","HB1903",
             "HB1401","HB1402","HB1403","HB0800","HB1101","HB1102","HB1103")]
sbHN<-HN1[,c("ID","SA0010","SA0100","IM0100","HNB0920","HNG0110","HNG0210","HNG0310","HNG0410",
             "HNG0510","HNG0610","HNJ1100","HNJ2300a","HNJ2300b")]
sbHN$'netincome'<-rowSums(sbHN[,c("HNG0110","HNG0210","HNG0310","HNG0410","HNG0510","HNG0610")],na.rm=TRUE)
sbHN$'num_card'<-rowSums(sbHN[,c("HNJ2300a","HNJ2300b")],na.rm=TRUE)
sbHN<-sbHN[,c(1:5,12,15,16)]
sbD1<-D1[,c("ID","SA0010","SA0100","IM0100","DA3001","DH0001","DHAGEH1","DHAQ01","DHEDUH1",
            "DHEMPH1","DHHST","DHHTYPE","DHIDH1","DHIQ01","DHLQ01","DHNQ01","DI2000","DL1000",
            "DL1000i","DL1100","DL1100i","DL1110","DL1110i","DL1120","DL1120i","DL1210","DL1210i",
            "DL1220","DL1220i","DL1200","DL2000","DL2100","DL2110","DL2200","DLCC","DLCL","DN3001",
            "DNFPOS","DNNLA","DNNLAratio","DOCOGOOD","DOCOGOODP","DOCREDITAPPL","DOCREDITC",
            "DOABLETOSAVE","DODARATIO","DODIRATIO","DODNI","DODSMORTG","DODSTOTAL","DOLTVRATIO")]
wave3<-merge(wave3,sbHN,by=c('ID',"SA0010","SA0100","IM0100"))
wave3<-merge(wave3,sbD1,by=c('ID',"SA0010","SA0100","IM0100"))

colnames(wave3)<-c('ID','HHID','country','implicate','pastID','size','pc_ownership','way_acq',
                   'year_acq','price','MM','num_mgg','rent','year_m1','year_m2','year_m3','length_m1',
                   'length_m2','length_m3',
                   ##'year_o1_m1','year_o1_m2','year_o1_m3','year_o2_m1',
                   ##'year_o2_m2','year_o2_m3','year_o3_m1','year_o3_m2','year_o3_m3','length_o1_m1',
                   ##'length_o1_m2','length_o1_m3','length_o2_m1','length_o2_m2','length_o2_m3',
                   ##'length_o3_m1','length_o3_m2','length_o3_m3',
                   'leasing','purpose_priloan1',
                   'purpose_priloan2','purpose_priloan3','outstand_priloan1','outstand_priloan2',
                   'outstand_priloan3','purpose_noncol1','purpose_noncol2','purpose_noncol3',
                   'outstand_noncol1','outstand_noncol2','outstand_noncol3',
                   'outstand_m1','outstand_m2','outstand_m3','interest_m1','interest_m2','interest_m3',
                   'amount_m1','amount_m2','amount_m3',"price_initial",'refi_m1','refi_m2','refi_m3',
                   'rent_imputed','debitcard','netincome','num_creditcard','asset','num_member','age','wealth_q',
                   'edu','emp','house_status','type','person_ID','income_q','liability_q','netwealth_q',
                   'income','outstand_liability','dd','outstand_mgg','mm',
                   'outstand_HMRmgg','HMRmm','outstand_othmgg','othmm','outstand_overdraft','overdraft',
                   'outstand_ccard','ccard','outstand_nonmgg','payment_debt','payment_mgg','payment_HMRmgg',
                   'payment_noncol','has_ccard','has_overdraft','net_wealth','net_finwealth',
                   'net_liquidasset','ratio_nla_income','consump','consump_income',
                   'apply_credit','credit_constrain','save','debt_asset','debt_income',
                   'netwealth_income','mggservice_income','debtservice_income','LTV')
wave3$'APP'<-1
wave3$'buy'<-0
wave3$'buy'[wave3$'year_acq'>2014]<-1
wave3$'year_mm' <- apply(wave3[,c('year_m1','year_m2','year_m3')], 1, max,na.rm=TRUE)
wave3$'year_mm'[wave3$'year_mm'==-Inf]<-NA
wave3$'HMRmm_new'<-0
wave3$'HMRmm_new'[wave3$'year_mm'>2014]<-1
wave3$'owned'<-0
wave3$'owned'[wave3$'year_acq'<2015]<-1

save(wave3,file='wave3.RData')

###wave2###
H1b= read.csv("wave2/H1.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
save(H1b,file='H1b.RData')
HN1b= read.csv("wave2/HN1.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
save(HN1b,file='HN1b.RData')
D1b= read.csv("wave2/D1.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
save(D1b,file='D1b.RData')
load('H1b.RData')
load('HN1b.RData')
load('D1b.RData')
wave2<-H1b[,c("id","SA0010","SA0100","IM0100","HB0100","HB0500","HB0600","HB0700","HB0900",
             "HB1000","HB1010","HB2300","HB1301","HB1302","HB1303","HB1601","HB1602","HB1603",
             ##"HB3301","HB3302","HB3303","HB3601","HB3602","HB3603",  mgg for other properties, no # for properties
             "HC0110","HC0351a","HC0352a","HC0353a","HC0361","HC0362","HC0363","HC0501a","HC0502a",
             "HC0503a","HC0801","HC0802","HC0803","HB1701","HB1702","HB1703","HB1901","HB1902","HB1903",
             "HB1401","HB1402","HB1403","HB0800","HB1101","HB1102","HB1103")]
sbHN<-HN1b[,c("id","SA0010","SA0100","IM0100","HNB0920","HNG0110","HNG0210","HNG0310","HNG0410",
             "HNG0510","HNG0610","HNJ1100","HNJ2300a","HNJ2300b")]
sbHN$'netincome'<-rowSums(sbHN[,c("HNG0110","HNG0210","HNG0310","HNG0410","HNG0510","HNG0610")],na.rm=TRUE)
sbHN$'num_card'<-rowSums(sbHN[,c("HNJ2300a","HNJ2300b")],na.rm=TRUE)
sbHN<-sbHN[,c(1:5,12,15,16)]
sbD1<-D1b[,c('ID',"SA0010","SA0100","IM0100","DA3001","DH0001","DHAGEH1","DHAQ01","DHEDUH1",  
            "DHEMPH1","DHHST","DHHTYPE","DHIDH1","DHIQ01","DHLQ01","DHNQ01","DI2000","DL1000",
            "DL1000i","DL1100","DL1100i","DL1110","DL1110i","DL1120","DL1120i","DL1210","DL1210i",
            "DL1220","DL1220i","DL1200","DL2000","DL2100","DL2110","DL2200","DLCC","DLCL","DN3001",
            "DNFPOS","DNNLA","DNNLAratio","DOCOGOOD","DOCOGOODP","DOCREDITAPPL","DOCREDITC",
            "DOABLETOSAVE","DODARATIO","DODIRATIO","DODNI","DODSMORTG","DODSTOTAL","DOLTVRATIO")]
wave2<-merge(wave2,sbHN,by=c("id","SA0010","SA0100","IM0100"))
names(wave2)[1]<-"ID"
wave2<-merge(wave2,sbD1,by=c('ID',"SA0010","SA0100","IM0100"))

colnames(wave2)<-c('ID','HHID','country','implicate','size','pc_ownership','way_acq',
                   'year_acq','price','MM','num_mgg','rent','year_m1','year_m2','year_m3','length_m1',
                   'length_m2','length_m3',
                   ##'year_o_m1','year_o_m2','year_o_m3','length_o_m1',
                   ##'length_o_m2','length_o_m3',
                   'leasing','purpose_priloan1',
                   'purpose_priloan2','purpose_priloan3','outstand_priloan1','outstand_priloan2',
                   'outstand_priloan3','purpose_noncol1','purpose_noncol2','purpose_noncol3',
                   'outstand_noncol1','outstand_noncol2','outstand_noncol3',
                   'outstand_m1','outstand_m2','outstand_m3','interest_m1','interest_m2','interest_m3',
                   'amount_m1','amount_m2','amount_m3',"price_initial",'refi_m1','refi_m2','refi_m3',
                   'rent_imputed','debitcard','netincome','num_creditcard','asset','num_member','age','wealth_q',
                   'edu','emp','house_status','type','person_ID','income_q','liability_q','netwealth_q',
                   'income','outstand_liability','dd','outstand_mgg','mm',
                   'outstand_HMRmgg','HMRmm','outstand_othmgg','othmm','outstand_overdraft','overdraft',
                   'outstand_ccard','ccard','outstand_nonmgg','payment_debt','payment_mgg','payment_HMRmgg',
                   'payment_noncol','has_ccard','has_overdraft','net_wealth','net_finwealth',
                   'net_liquidasset','ratio_nla_income','consump','consump_income',
                   'apply_credit','credit_constrain','save','debt_asset','debt_income',
                   'netwealth_income','mggservice_income','debtservice_income','LTV')
wave2$'APP'<-0
wave2$'buy'<-0
wave2$'buy'[wave2$'year_acq'==2013]<-1
wave2$'buy'[wave2$'year_acq'==2014]<-1
wave2$'year_mm' <- apply(wave2[,c('year_m1','year_m2','year_m3')], 1, max,na.rm=TRUE)
wave2$'year_mm'[wave2$'year_mm'==-Inf]<-NA
wave2$'HMRmm_new'<-0
wave2$'HMRmm_new'[wave2$'year_mm'==2013]<-1
wave2$'HMRmm_new'[wave2$'year_mm'==2014]<-1
wave2$'owned'<-0
wave2$'owned'[wave2$'year_acq'<2013]<-1

save(wave2,file='wave2.RData')

load('wave2.RData')
load('wave3.RData')

##change the ID for wave2 data to match wave3 ID
test2<-wave2[,c('ID','HHID','country')]
colnames(test2)<-c('id','pastID','country')
test3<-wave3[,c('ID','HHID','country','pastID')]
test<-left_join(test2,test3,by=c("pastID","country"))
test<-test%>%distinct(id, .keep_all = TRUE)
test$'ID'<-ifelse(is.na(test$ID), test$id, test$ID)
wave2$ID<-test$'ID'
wave3<-wave3[,-c(5)]

datafull<-rbind(wave2,wave3)
#datafull$ID[duplicated(datafull$ID)] ##check how which households appear in both waves
datafull[,c(47:89)] <- lapply(datafull[,c(47:89)], function(x) as.numeric(as.character(x)))
datafull<-datafull[,-10]###delete the column "MM"="mm"
datafull$'own'<-1
datafull$'own'[datafull$'house_status'==3]<-0


##data cleaning##
datafull$'payment_debt'[is.na(datafull$'payment_debt')]<-0
datafull$'payment_mgg'[is.na(datafull$'payment_mgg')]<-0
datafull$'payment_HMRmgg'[is.na(datafull$'payment_HMRmgg')]<-0
datafull$'payment_noncol'[is.na(datafull$'payment_noncol')]<-0
datafull$'outstand_liability'[is.na(datafull$'outstand_liability')]<-0
datafull$'outstand_HMRmgg'[is.na(datafull$'outstand_HMRmgg')]<-0
datafull$'outstand_othmgg'[is.na(datafull$'outstand_othmgg')]<-0
datafull$'outstand_overdraft'[is.na(datafull$'outstand_overdraft')]<-0
datafull$'outstand_ccard'[is.na(datafull$'outstand_ccard')]<-0
datafull$'outstand_mgg'[is.na(datafull$'outstand_mgg')]<-0
datafull$'outstand_nonmgg'[is.na(datafull$'outstand_nonmgg')]<-0
datafull$'num_mgg'[is.na(datafull$'num_mgg')]<-0
datafull$'leasing'[is.na(datafull$'leasing')]<-0
datafull$'outstand_noncol1'[is.na(datafull$'outstand_noncol1')]<-0
datafull$'outstand_noncol2'[is.na(datafull$'outstand_noncol2')]<-0
datafull$'outstand_noncol3'[is.na(datafull$'outstand_noncol3')]<-0
datafull$'outstand_priloan1'[is.na(datafull$'outstand_priloan1')]<-0
datafull$'outstand_priloan2'[is.na(datafull$'outstand_priloan2')]<-0
datafull$'outstand_priloan3'[is.na(datafull$'outstand_priloan3')]<-0
datafull$'edu'[datafull$'edu'<0]<-NA
datafull$'age'[datafull$'age'<0]<-NA
datafull$'has_ccard'[is.na(datafull$'has_ccard')]<-0
datafull$'rent'[is.na(datafull$'rent')]<-0
datafull$'debt_asset'[is.na(datafull$'debt_asset')]<-0
datafull$'debt_income'[is.na(datafull$'debt_income')]<-0
datafull$'mggservice_income'[is.na(datafull$'mggservice_income')]<-0
datafull$'debtservice_income'[is.na(datafull$'debtservice_income')]<-0
datafull$'HMRmm_new'[datafull$'country'=='FI']<-NA
datafull$'buy'[datafull$'country'=='FI']<-NA

##calculate i_M and LTV_initial
datafull$'outstand_m1'[is.na(datafull$'outstand_m1')]<-0
datafull$'outstand_m2'[is.na(datafull$'outstand_m2')]<-0
datafull$'outstand_m3'[is.na(datafull$'outstand_m3')]<-0
datafull$'interest_m1'[is.na(datafull$'interest_m1')]<-0
datafull$'interest_m2'[is.na(datafull$'interest_m2')]<-0
datafull$'interest_m3'[is.na(datafull$'interest_m3')]<-0
datafull$'outstand_m'<-apply(datafull[,c('outstand_m1','outstand_m2','outstand_m3')], 1,sum)
data_i_M<-datafull[,c('outstand_m1','interest_m1','outstand_m2','interest_m2','outstand_m3','interest_m3','outstand_m')]
datafull$'i_M'<-apply(data_i_M, 1, function(x){(x[1]*x[2]+x[3]*x[4]+x[5]*x[6])/x[7]})
datafull$'i_M'[is.nan(datafull$'i_M')]<-NA
datafull$'amount_initial'<-0
data_LTV<-datafull[,c('year_acq','year_m1','year_m2','year_m3','amount_m1','amount_m2','amount_m3','price_initial','amount_initial')]
data_LTV$'year_acq'[is.na(data_LTV$'year_acq')]<-0
data_LTV$'year_m1'[is.na(data_LTV$'year_m1')]<-0
data_LTV$'year_m2'[is.na(data_LTV$'year_m2')]<-0
data_LTV$'year_m3'[is.na(data_LTV$'year_m3')]<-0
data_LTV$'amount_m1'[is.na(data_LTV$'year_m1')]<-0
data_LTV$'amount_m2'[is.na(data_LTV$'year_m2')]<-0
data_LTV$'amount_m3'[is.na(data_LTV$'year_m3')]<-0
data_LTV$'amount_initial'<-apply(data_LTV, 1, function(x){
  if (x[1]==x[2]){x[5]+x[9]}
  else {x[9]}
  })
data_LTV$'amount_initial'<-apply(data_LTV, 1, function(x){
  if (x[1]==x[3]){x[6]+x[9]}
  else {x[9]}
})
data_LTV$'amount_initial'<-apply(data_LTV, 1, function(x){
  if (x[1]==x[4]){x[7]+x[9]}
  else {x[9]}
})
data_LTV$'amount_initial'[data_LTV$'amount_initial'==0]<-NA
data_LTV$'LTV_initial'<-data_LTV$'amount_initial'/data_LTV$'price_initial'
data_LTV$'price'<-datafull$'price'
datafull$'amount_initial'<-data_LTV$'amount_initial'
datafull$'LTV_initial'<-datafull$'amount_initial'/datafull$'price_initial'
#the quality of data is low, need to clean a bit to get real LTV, especially for those with LTV>1.
datafull$'LTV_initial'[datafull$'LTV_initial'>1700]<-NA
cleanLTV<-as.data.frame(datafull[,c('LTV_initial')])
colnames(cleanLTV)<-'LTV_initial'
cleanLTV$'LTV_initial_0'<-cleanLTV$'LTV_initial'
cleanLTV$'LTV_initial_0'[is.na(cleanLTV$'LTV_initial')]<-0
datafull$'LTV_initial'<-apply(cleanLTV, 1, function(x){
  if (x[2]>200){x[1]/1000}
  else {x[1]}
  })
datafull$'LTV_initial'<-apply(cleanLTV, 1, function(x){
  if (x[2]>100){x[1]/100}
  else {x[1]}
})
cleanLTV<-datafull[,c('price','price_initial','LTV_initial')]
cleanLTV$'price_initial'[is.na(cleanLTV$'price_initial')]<-0
cleanLTV$'price'[is.na(cleanLTV$'price')]<-0
datafull$'LTV_initial'<-apply(cleanLTV, 1, function(x){
  if (x[1]>8*x[2]){x[3]/10}
  else {x[3]}
})

##construct variable for consumer credit
consumercredit<-datafull[,c('ID','HHID','country','implicate','outstand_ccard','purpose_priloan1',
                            'purpose_priloan2','purpose_priloan3','outstand_priloan1','outstand_priloan2',
                            'outstand_priloan3','purpose_noncol1','purpose_noncol2','purpose_noncol3',
                            'outstand_noncol1','outstand_noncol2','outstand_noncol3')]
consumercredit$'purpose1'<-0
consumercredit$'purpose1'[consumercredit$purpose_priloan1==4]<-1
consumercredit$'purpose1'[consumercredit$purpose_priloan1==8]<-1
consumercredit$'purpose2'<-0
consumercredit$'purpose2'[consumercredit$purpose_priloan2==4]<-1
consumercredit$'purpose2'[consumercredit$purpose_priloan2==8]<-1
consumercredit$'purpose3'<-0
consumercredit$'purpose3'[consumercredit$purpose_priloan3==4]<-1
consumercredit$'purpose3'[consumercredit$purpose_priloan3==8]<-1
consumercredit$'loan1' <- apply(consumercredit[,c('purpose1','outstand_priloan1')], 1,prod)
consumercredit$'loan2' <- apply(consumercredit[,c('purpose2','outstand_priloan2')], 1,prod)
consumercredit$'loan3' <- apply(consumercredit[,c('purpose3','outstand_priloan3')], 1,prod)
consumercredit$'priloan' <- apply(consumercredit[,c('loan1','loan2','loan3')], 1,sum)
consumercredit$'purpose1'<-0
consumercredit$'purpose1'[consumercredit$purpose_noncol1==4]<-1
consumercredit$'purpose1'[consumercredit$purpose_noncol1==8]<-1
consumercredit$'purpose2'<-0
consumercredit$'purpose2'[consumercredit$purpose_noncol2==4]<-1
consumercredit$'purpose2'[consumercredit$purpose_noncol2==8]<-1
consumercredit$'purpose3'<-0
consumercredit$'purpose3'[consumercredit$purpose_noncol3==4]<-1
consumercredit$'purpose3'[consumercredit$purpose_noncol3==8]<-1
consumercredit$'loan1' <- apply(consumercredit[,c('purpose1','outstand_noncol1')], 1,prod)
consumercredit$'loan2' <- apply(consumercredit[,c('purpose2','outstand_noncol2')], 1,prod)
consumercredit$'loan3' <- apply(consumercredit[,c('purpose3','outstand_noncol3')], 1,prod)
consumercredit$'noncol' <- apply(consumercredit[,c('loan1','loan2','loan3')], 1,sum)
consumercredit$'outstand_cc' <- apply(consumercredit[,c('priloan','noncol','outstand_ccard')], 1,sum)

datafull<-cbind(datafull,consumercredit[,c('priloan','noncol','outstand_cc')])

refinance<-datafull[,c('year_m1','refi_m1','year_m2','refi_m2','year_m3','refi_m3',"year_mm")]
refinance$'year_m1'[is.na(refinance$'year_m1')]<-0
refinance$'year_m2'[is.na(refinance$'year_m2')]<-0
refinance$'year_m3'[is.na(refinance$'year_m3')]<-0
refinance$'year_mm'[is.na(refinance$'year_mm')]<-0
refinance$'refi_m1'[is.na(refinance$'refi_m1')]<-0
refinance$'refi_m2'[is.na(refinance$'refi_m2')]<-0
refinance$'refi_m3'[is.na(refinance$'refi_m3')]<-0
refinance$'refi'<-apply(refinance, 1, function(x){
  if (x[2]==1&&x[1]==x[7]){x[2]}
  else {
    if (x[4]==1&&x[3]==x[7]){x[4]}
    else{
      if (x[6]==1&&x[5]==x[7]){x[6]}
      else{0}
    }
  }
   })

datafull$'refi'<-refinance$'refi'
datafull$'refi'[datafull$country=="FI"]<-NA


##add GDP & PPP & i & i_cc & MRO & credit_condition
GDPPPP= read.csv("GDP_PPP.csv", sep=',',header=TRUE, stringsAsFactors=FALSE, na.string = c('','.'))
datafull<-left_join(datafull,GDPPPP,by=c("country","APP"))
datafull<-left_join(datafull,GDPPPP[,c(1:2,9:10)],by=c("country","APP"))

##save data set##
save(datafull,file='datafull.RData')
load('datafull.RData')
library(foreign)
write.dta(datafull, "HFCS.dta")

######################################
###          several tests         ###
######################################
library(ggplot2)
test<-datafull[,c('HHID','country', 'year_acq','year_m1','year_m2','year_m3','APP')]

###see the distribution of the year when hh acquire the house###
test1<-test[which(test$'wave'==1),]
test1<-test1[which(test1$'year_acq'>2006),]
ggplot(test1[which(test1$'country'=='SK'),], aes(year_acq))+
  geom_histogram(binwidth = 1)

###see the distribution of the year when hh get the latest mgg###
test2<-test[which(test$'wave'==1),]
test2<-test2[which(test2$'year_mm'>2006),]
ggplot(test2[which(test2$'country'=='SK'),], aes(year_mm))+
  geom_histogram(binwidth = 1)