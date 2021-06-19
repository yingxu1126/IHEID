use "E:\iheid\paper proposal\consumer credit\DATA\ECB data\HFCS\HFCS.dta", clear
encode country, gen (Country)
egen long id = group(ID)
gen cc=0
replace cc=1 if outstand_cc>0
gen employ=0
replace employ=1 if emp==1| emp==2
drop if country=="ES"
drop if country=="HR"
drop if country=="LT"
drop if  country=="HU"
drop if  country=="PL"

replace income=income/100000
gen wealth=asset/1000000
replace net_wealth=net_wealth/100000

tab country if HMRmm_new==1, sum(refi)
//figures
graph box edu, over(w_income_q) 
*saving(loan)
graph box edu, over(w_wealth_q) 

//weighted regression
*regress depvar list_of_indep_vars [pweight=varname]
*The probability weight, called a pweight in Stata, is calculated as N/n, where N = the number of elements in the population and n = the number of elements in the sample.
*For example, if a population has 10 elements and 3 are sampled at random with replacement, then the probability weight would be 10/3 = 3.33.  
*the inverse of the probability of an observation being selected into the sample

//calculate quintiles with weights
gen w_income_q=.
egen groups=group(country APP)
forval i=1/34{
xtile five_income=income if groups==`i'[pweight=weight], n(5)
replace w_income_q=five_income if groups==`i'
drop five_income
}
gen w_wealth_q=.
forval i=1/34{
xtile five_wealth=net_wealth if groups==`i'[pweight=weight], n(5)
replace w_wealth_q=five_wealth if groups==`i'
drop five_wealth
}
**income groups
***credit
reghdfe apply_credit own edu employ has_ccard has_overdraft net_wealth GDP PPP hp i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model6
esttab model* using inequal1_qw5.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0) ///
coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
coefplot model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")

***house purchase
reghdfe HMRmm_new buy edu employ othmm has_ccard has_overdraft net_wealth income owned GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
gen buyAPP=buy*APP
reghdfe HMRmm_new buy buyAPP edu employ othmm has_ccard has_overdraft net_wealth income owned GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth owned i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp if own==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp if owned==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
esttab model1 model2 model3 model4 model5 using inequal2_qw5.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
coefplot (model3, label(all households)) (model4, label(owners)) (model5,label(previous renters)), keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0) ///
coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")

***key indicators
reghdfe LTV edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe debtservice_income own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe refi edu employ othmm has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp if HMRmm==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
esttab model1 model2 model3 model4 using inequal3_qw5.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||, keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
coefplot (model4, label(refi)), keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0) coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")

**net wealth groups
reghdfe apply_credit own edu employ has_ccard has_overdraft  income GDP PPP hp i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model6
esttab model* using inequal4_qw5.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
coefplot  model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")

***house purchase
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if own==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if owned==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
esttab model1 model2 model3 using inequal6_qw5.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
coefplot (model1, label(all households)) (model2, label(owners)) (model3,label(previous renters)), keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")

***key indicators
reghdfe LTV edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe debtservice_income own edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe refi edu employ othmm has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if HMRmm==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
esttab model1 model2 model3 model4 using inequal5_qw5.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||, keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
coefplot (model4, label(refi)) , keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")

**different education groups
gen high_edu=1
replace high_edu=0 if edu==1 |edu==2
reghdfe refi employ othmm has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if HMRmm==1&high_edu==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe refi employ othmm has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if HMRmm==1&high_edu==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
coefplot (model1, label(low edu)) (model2, label(high edu)) (model4, label(total)) , keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")


********************************
      //robustness check//
********************************
//A. add additional characteristics
***income
reghdfe apply_credit own edu employ has_ccard has_overdraft net_wealth GDP PPP hp i.w_income_q w_income_q#APP ib5.w_income_q save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model6
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0) ///
coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
coefplot model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth owned i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age if own==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age if owned==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
coefplot (model3, label(all households)) (model4, label(owners)) (model5,label(previous renters)), keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0) ///
coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
reghdfe LTV edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe debtservice_income own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe refi edu employ othmm has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q GDP PPP hp save age if HMRmm==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||, keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
coefplot (model4, label(refi)), keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0) coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
*net wealth
reghdfe apply_credit own edu employ has_ccard has_overdraft  income GDP PPP hp save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model6
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
coefplot  model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if own==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if owned==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
coefplot (model1, label(all households)) (model2, label(owners)) (model3,label(previous renters)), keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
reghdfe LTV edu employ has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||, keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
gen high_edu=1
replace high_edu=0 if edu==1 |edu==2
reghdfe refi employ othmm has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if HMRmm==1&high_edu==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe refi employ othmm has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if HMRmm==1&high_edu==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe refi edu employ othmm has_ccard has_overdraft income save age i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if HMRmm==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
coefplot (model1, label(low edu)) (model2, label(high edu))(model3, label(total))  , keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")


//B. quintiles among all countries rather per country
xtile five_income=income [pweight=weight], n(5)
xtile five_wealth=net_wealth [pweight=weight], n(5)
***income
reghdfe apply_credit own edu employ has_ccard has_overdraft net_wealth GDP PPP hp i.five_income five_income#APP ib5.five_income save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model6
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.five_income#1.APP 2.five_income#1.APP 3.five_income#1.APP 4.five_income#1.APP) xline(0) ///
coeflabel(1.five_income#1.APP="0-20%" 2.five_income#1.APP="20-40%" 3.five_income#1.APP="40-60%" 4.five_income#1.APP="60-80%")ytitle("income quintiles")
coefplot model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.five_income#1.APP 2.five_income#1.APP 3.five_income#1.APP 4.five_income#1.APP) xline(0)byopts(xrescale)coeflabel(1.five_income#1.APP="0-20%" 2.five_income#1.APP="20-40%" 3.five_income#1.APP="40-60%" 4.five_income#1.APP="60-80%")ytitle("income quintiles")
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth owned i.five_income five_income#APP ib5.five_income GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age if own==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age if owned==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
coefplot (model3, label(all households)) (model4, label(owners)) (model5,label(previous renters)), keep(1.five_income#1.APP 2.five_income#1.APP 3.five_income#1.APP 4.five_income#1.APP) xline(0) ///
coeflabel(1.five_income#1.APP="0-20%" 2.five_income#1.APP="20-40%" 3.five_income#1.APP="40-60%" 4.five_income#1.APP="60-80%")ytitle("income quintiles")
reghdfe LTV edu employ has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe refi edu employ othmm has_ccard has_overdraft net_wealth i.five_income five_income#APP ib5.five_income GDP PPP hp save age if HMRmm==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||, keep(1.five_income#1.APP 2.five_income#1.APP 3.five_income#1.APP 4.five_income#1.APP) xline(0)byopts(xrescale)coeflabel(1.five_income#1.APP="0-20%" 2.five_income#1.APP="20-40%" 3.five_income#1.APP="40-60%" 4.five_income#1.APP="60-80%")ytitle("income quintiles")
coefplot (model4, label(refi)), keep(1.five_income#1.APP 2.five_income#1.APP 3.five_income#1.APP 4.five_income#1.APP) xline(0) coeflabel(1.five_income#1.APP="0-20%" 2.five_income#1.APP="20-40%" 3.five_income#1.APP="40-60%" 4.five_income#1.APP="60-80%")ytitle("income quintiles")
*net wealth
reghdfe apply_credit own edu employ has_ccard has_overdraft  income GDP PPP hp i.five_wealth five_wealth#APP ib5.five_wealth [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model6
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.five_wealth#1.APP 2.five_wealth#1.APP 3.five_wealth#1.APP 4.five_wealth#1.APP) xline(0)coeflabel(1.five_wealth#1.APP="0-20%" 2.five_wealth#1.APP="20-40%" 3.five_wealth#1.APP="40-60%" 4.five_wealth#1.APP="60-80%")ytitle("net wealth quintiles")
coefplot  model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.five_wealth#1.APP 2.five_wealth#1.APP 3.five_wealth#1.APP 4.five_wealth#1.APP) xline(0)byopts(xrescale)coeflabel(1.five_wealth#1.APP="0-20%" 2.five_wealth#1.APP="20-40%" 3.five_wealth#1.APP="40-60%" 4.five_wealth#1.APP="60-80%")ytitle("net wealth quintiles")
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp if own==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp if owned==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
coefplot (model1, label(all households)) (model2, label(owners)) (model3,label(previous renters)), keep(1.five_wealth#1.APP 2.five_wealth#1.APP 3.five_wealth#1.APP 4.five_wealth#1.APP) xline(0)coeflabel(1.five_wealth#1.APP="0-20%" 2.five_wealth#1.APP="20-40%" 3.five_wealth#1.APP="40-60%" 4.five_wealth#1.APP="60-80%")ytitle("net wealth quintiles")
reghdfe LTV edu employ has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||, keep(1.five_wealth#1.APP 2.five_wealth#1.APP 3.five_wealth#1.APP 4.five_wealth#1.APP) xline(0)byopts(xrescale)coeflabel(1.five_wealth#1.APP="0-20%" 2.five_wealth#1.APP="20-40%" 3.five_wealth#1.APP="40-60%" 4.five_wealth#1.APP="60-80%")ytitle("net wealth quintiles")
gen high_edu=1
replace high_edu=0 if edu==1 |edu==2
reghdfe refi edu employ othmm has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp if HMRmm==1&high_edu==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe refi edu employ othmm has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp if HMRmm==1&high_edu==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe refi edu employ othmm has_ccard has_overdraft income i.five_wealth five_wealth#APP ib5.five_wealth GDP PPP hp if HMRmm==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
coefplot (model1, label(low edu)) (model2, label(high edu))(model3, label(total)), keep(1.five_wealth#1.APP 2.five_wealth#1.APP 3.five_wealth#1.APP 4.five_wealth#1.APP) xline(0)coeflabel(1.five_wealth#1.APP="0-20%" 2.five_wealth#1.APP="20-40%" 3.five_wealth#1.APP="40-60%" 4.five_wealth#1.APP="60-80%")ytitle("net wealth quintiles")

//C. weights by relative country population size, no weights for quintiles
egen sum_w=total(weight),by(APP country)
egen sum_obs=total(implicate),by(APP country)
gen pweight=sum_w/sum_obs
**income groups
***credit
reghdfe apply_credit own edu employ has_ccard has_overdraft net_wealth GDP PPP hp i.income_q income_q#APP ib5.income_q [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model6
esttab model* using inequal1_wp.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
***house purchase
reghdfe HMRmm_new buy edu employ othmm has_ccard has_overdraft net_wealth income owned GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model1
gen buyAPP=buy*APP
reghdfe HMRmm_new buy buyAPP edu employ othmm has_ccard has_overdraft net_wealth income owned GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth owned i.income_q income_q#APP ib5.income_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp if own==1 [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp if owned==0 [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model5
esttab model1 model2 model3 model4 model5 using inequal2_wp.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
***key indicators
reghdfe LTV edu employ has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe debtservice_income own edu employ has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe refi edu employ othmm has_ccard has_overdraft net_wealth i.income_q income_q#APP ib5.income_q GDP PPP hp if HMRmm==1 [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model4
esttab model1 model2 model3 model4 using inequal3_wp.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
**net wealth groups
reghdfe apply_credit own edu employ has_ccard has_overdraft  income GDP PPP hp i.netwealth_q netwealth_q#APP ib5.netwealth_q [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model6
esttab model* using inequal4_wp.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
reghdfe outstand_liability rent edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
***house purchase
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp if own==1 [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp if owned==0 [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model3
esttab model1 model2 model3 using inequal6_wp.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
***key indicators
reghdfe LTV edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe debtservice_income own edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe refi edu employ othmm has_ccard has_overdraft income i.netwealth_q netwealth_q#APP ib5.netwealth_q GDP PPP hp if HMRmm==1 [pweight=pweight], absorb (type Country year) vce(cluster id)
est store model4
esttab model1 model2 model3 model4 using inequal5_wp.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)

//D. NO WEIGHTS
**income groups
***credit
reghdfe apply_credit own edu employ has_ccard has_overdraft net_wealth GDP PPP hp i.income_q income_q#APP ib5.income_q, absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model6
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.income_q#1.APP 2.income_q#1.APP 3.income_q#1.APP 4.income_q#1.APP) xline(0) ///
coeflabel(1.income_q#1.APP="0-20%" 2.income_q#1.APP="20-40%" 3.income_q#1.APP="40-60%" 4.income_q#1.APP="60-80%")ytitle("income quintiles")
coefplot model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.income_q#1.APP 2.income_q#1.APP 3.income_q#1.APP 4.income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.income_q#1.APP="0-20%" 2.income_q#1.APP="20-40%" 3.income_q#1.APP="40-60%" 4.income_q#1.APP="60-80%")ytitle("income quintiles")
reghdfe HMRmm_new buy edu employ othmm has_ccard has_overdraft net_wealth income owned GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model1
gen buyAPP=buy*APP
reghdfe HMRmm_new buy buyAPP edu employ othmm has_ccard has_overdraft net_wealth income owned GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth owned i.income_q income_q#APP GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model3
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q if own==1, absorb (type Country year) vce(cluster id)
est store model4
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q if owned==0, absorb (type Country year) vce(cluster id)
est store model5
coefplot (model3, label(all households)) (model4, label(owners)) (model5,label(previous renters)),keep(1.income_q#1.APP 2.income_q#1.APP 3.income_q#1.APP 4.income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.income_q#1.APP="0-20%" 2.income_q#1.APP="20-40%" 3.income_q#1.APP="40-60%" 4.income_q#1.APP="60-80%")ytitle("income quintiles")
reghdfe LTV edu employ has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q, absorb (type Country year) vce(cluster id)
est store model2
reghdfe refi edu employ othmm has_ccard has_overdraft net_wealth i.income_q income_q#APP GDP PPP hp ib5.income_q if HMRmm==1, absorb (type Country year) vce(cluster id)
est store model4
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||,keep(1.income_q#1.APP 2.income_q#1.APP 3.income_q#1.APP 4.income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.income_q#1.APP="0-20%" 2.income_q#1.APP="20-40%" 3.income_q#1.APP="40-60%" 4.income_q#1.APP="60-80%")ytitle("income quintiles")
coefplot (model4, label(refi)),keep(1.income_q#1.APP 2.income_q#1.APP 3.income_q#1.APP 4.income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.income_q#1.APP="0-20%" 2.income_q#1.APP="20-40%" 3.income_q#1.APP="40-60%" 4.income_q#1.APP="60-80%")ytitle("income quintiles")
**net wealth groups
reghdfe apply_credit own edu employ has_ccard has_overdraft  income GDP PPP hp i.netwealth_q netwealth_q#APP ib5.netwealth_q, absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q, absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q, absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q, absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q, absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q, absorb (type Country year) vce(cluster id)
est store model6
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.netwealth_q#1.APP 2.netwealth_q#1.APP 3.netwealth_q#1.APP 4.netwealth_q#1.APP) xline(0)coeflabel(1.netwealth_q#1.APP="0-20%" 2.netwealth_q#1.APP="20-40%" 3.netwealth_q#1.APP="40-60%" 4.netwealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
coefplot  model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.netwealth_q#1.APP 2.netwealth_q#1.APP 3.netwealth_q#1.APP 4.netwealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.netwealth_q#1.APP="0-20%" 2.netwealth_q#1.APP="20-40%" 3.netwealth_q#1.APP="40-60%" 4.netwealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q, absorb (type Country year) vce(cluster id)
est store model1
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q if own==1, absorb (type Country year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q if owned==0, absorb (type Country year) vce(cluster id)
est store model3
coefplot (model1, label(all households)) (model2, label(owners)) (model3,label(previous renters)), keep(1.netwealth_q#1.APP 2.netwealth_q#1.APP 3.netwealth_q#1.APP 4.netwealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.netwealth_q#1.APP="0-20%" 2.netwealth_q#1.APP="20-40%" 3.netwealth_q#1.APP="40-60%" 4.netwealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
reghdfe LTV edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q, absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q, absorb (type Country year) vce(cluster id)
est store model2
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||, keep(1.netwealth_q#1.APP 2.netwealth_q#1.APP 3.netwealth_q#1.APP 4.netwealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.netwealth_q#1.APP="0-20%" 2.netwealth_q#1.APP="20-40%" 3.netwealth_q#1.APP="40-60%" 4.netwealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
gen high_edu=1
replace high_edu=0 if edu==1 |edu==2
reghdfe refi edu employ othmm has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q if HMRmm==1&high_edu==0, absorb (type Country year) vce(cluster id)
est store model1
reghdfe refi edu employ othmm has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q if HMRmm==1&high_edu==1, absorb (type Country year) vce(cluster id)
est store model2
reghdfe refi edu employ othmm has_ccard has_overdraft income i.netwealth_q netwealth_q#APP GDP PPP hp ib5.netwealth_q if HMRmm==1, absorb (type Country year) vce(cluster id)
est store model3
coefplot (model1, label(low edu)) (model2, label(high edu))(model3, label(total)), keep(1.netwealth_q#1.APP 2.netwealth_q#1.APP 3.netwealth_q#1.APP 4.netwealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.netwealth_q#1.APP="0-20%" 2.netwealth_q#1.APP="20-40%" 3.netwealth_q#1.APP="40-60%" 4.netwealth_q#1.APP="60-80%")ytitle("net wealth quintiles")

//E. Country*Year fixed effects
***income quintiles
reghdfe apply_credit own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model6
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0) ///
coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
coefplot model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
reghdfe HMRmm_new buy edu employ othmm has_ccard has_overdraft net_wealth income owned [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model1
gen buyAPP=buy*APP
reghdfe HMRmm_new buy buyAPP edu employ othmm has_ccard has_overdraft net_wealth income owned [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth owned i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model3
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q if own==1 [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model4
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q if owned==0 [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model5
coefplot (model3, label(all households)) (model4, label(owners)) (model5,label(previous renters)), keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0) ///
coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
reghdfe LTV edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model2
reghdfe refi edu employ othmm has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q if HMRmm==1 [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model4
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||, keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
coefplot (model4, label(refi)), keep(1.w_income_q#1.APP 2.w_income_q#1.APP 3.w_income_q#1.APP 4.w_income_q#1.APP) xline(0) coeflabel(1.w_income_q#1.APP="0-20%" 2.w_income_q#1.APP="20-40%" 3.w_income_q#1.APP="40-60%" 4.w_income_q#1.APP="60-80%")ytitle("income quintiles")
**net wealth quintiles
reghdfe apply_credit own edu employ has_ccard has_overdraft  income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model6
coefplot (model1, label(apply_credit)) (model2, label(dd)) (model3,label(HMRmm_new)) (model4,label(cc)), keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
coefplot  model5, bylabel(outstand_HMRmgg) || model6, bylabel(outstand_cc) ||, keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model1
reghdfe buy owned edu employ othmm  has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q if own==1 [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model2
reghdfe buy edu employ othmm  has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q if owned==0 [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model3
coefplot (model1, label(all households)) (model2, label(owners)) (model3,label(previous renters)), keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
reghdfe LTV edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model2
coefplot  model1, bylabel(LTV) || model2, bylabel(DTI) ||, keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)byopts(xrescale)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")
reghdfe refi employ othmm has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if HMRmm==1&high_edu==0 [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model1
reghdfe refi employ othmm has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if HMRmm==1&high_edu==1 [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model2
reghdfe refi edu employ othmm has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q GDP PPP hp if HMRmm==1 [pweight=weight], absorb (type Country#year) vce(cluster id)
est store model3
coefplot (model1, label(low edu)) (model2, label(high edu))(model3, label(total))  , keep(1.w_wealth_q#1.APP 2.w_wealth_q#1.APP 3.w_wealth_q#1.APP 4.w_wealth_q#1.APP) xline(0)coeflabel(1.w_wealth_q#1.APP="0-20%" 2.w_wealth_q#1.APP="20-40%" 3.w_wealth_q#1.APP="40-60%" 4.w_wealth_q#1.APP="60-80%")ytitle("net wealth quintiles")


*******************************************
          //2-step procedure//
*******************************************
//solve betas for the 1-step
reghdfe dd own edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight] if country=="AT", absorb (type year) vce(cluster id)
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight] if country=="AT", absorb (type year) vce(cluster id)
reghdfe cc mm own edu employ has_ccard has_overdraft income i.w_wealth_q w_wealth_q#APP ib5.w_wealth_q [pweight=weight] if country=="AT", absorb (type year) vce(cluster id)
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft net_wealth i.w_income_q w_income_q#APP ib5.w_income_q [pweight=weight] if country=="AT", absorb (type year) vce(cluster id)

*******************************************
          //RIF regressions//
*******************************************
**calculate RIF coefficients
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==0  [pweight=weight],rif(gini) abs(type Country year) vce(cluster id)
est store model1
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==0  [pweight=weight],rif(ucs(70)) abs(type Country year) vce(cluster id)
est store model2
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==0  [pweight=weight],rif(ucs(80)) abs(type Country year) vce(cluster id)
est store model3
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==0  [pweight=weight],rif(ucs(90)) abs(type Country year) vce(cluster id)
est store model4
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==0  [pweight=weight],rif(ucs(95)) abs(type Country year) vce(cluster id)
est store model5
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==0  [pweight=weight],rif(ucs(99)) abs(type Country year) vce(cluster id)
est store model6
esttab model* using rif.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==1  [pweight=weight],rif(gini) abs(type Country year) vce(cluster id)
est store model1
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==1  [pweight=weight],rif(ucs(70)) abs(type Country year) vce(cluster id)
est store model2
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==1  [pweight=weight],rif(ucs(80)) abs(type Country year) vce(cluster id)
est store model3
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==1  [pweight=weight],rif(ucs(90)) abs(type Country year) vce(cluster id)
est store model4
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==1  [pweight=weight],rif(ucs(95)) abs(type Country year) vce(cluster id)
est store model5
rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==1  [pweight=weight],rif(ucs(99)) abs(type Country year) vce(cluster id)
est store model6
esttab model* using rif1.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)
*coefplot (model1, label(Gini))  (model3, label(top 30%))  (model4, label(top 20%))  (model5, label(top 10%))  (model6, label(top 5%))  (model7, label(top 1%)) , vertical keep(net_wealth )  ytitle("RIF coefficients")
*rifhdreg outstand_liability own edu employ has_ccard has_overdraft net_wealth income if APP==0  [pweight=weight],rif(iqsr(60 90)) abs(type Country year) vce(cluster id)

**RIF Decomposition: Oaxaca Blinder
qui:tab type, gen(dtype_)
qui:tab Country, gen(dcountry_)
oaxaca_rif outstand_liability own edu employ has_ccard has_overdraft net_wealth income (type:normalize(dtype_*)) (Country:normalize(dcountry_*)) [pweight=weight],rif(gini) by(APP) swap w(1) cluster(id)
est store model1
oaxaca_rif outstand_liability own edu employ has_ccard has_overdraft net_wealth income (type:normalize(dtype_*)) (Country:normalize(dcountry_*)) [pweight=weight],rif(ucs(90)) by(APP) swap w(1) cluster(id)
est store model2
oaxaca_rif outstand_liability own edu employ has_ccard has_overdraft net_wealth income (type:normalize(dtype_*)) (Country:normalize(dcountry_*)) [pweight=weight],rif(ucs(95)) by(APP) swap w(1) cluster(id)
est store model3
esttab model1 model2 model3 using OB.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)

//different country groups
gen group_inequal=.
replace group_inequal=1 if country=="NL" |country=="SK" |country=="SI"
replace group_inequal=2 if country=="AT" |country=="BE" |country=="FI" |country=="FR" |country=="IT"
replace group_inequal=3 if country=="IE" |country=="LU" |country=="MT"
replace group_inequal=4 if country=="CY" |country=="EE" |country=="DE" |country=="GR" |country=="LV" |country=="PT"

//only focus on the top 10% households and compared them with the rest 90%
egen ten=xtile(income), n(10) by(country)
gen top_i=0
replace top_i=1 if ten==10
reghdfe apply_credit own edu employ has_ccard has_overdraft net_wealth GDP PPP hp top_i top_i#APP [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe dd own edu employ has_ccard has_overdraft net_wealth top_i top_i#APP GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe HMRmm_new own edu employ othmm has_ccard has_overdraft net_wealth top_i top_i#APP GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe cc mm own edu employ has_ccard has_overdraft net_wealth top_i top_i#APP GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe outstand_HMRmgg outstand_cc rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth top_i top_i#APP  GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
reghdfe outstand_cc outstand_HMRmgg rent edu employ outstand_othmgg has_ccard has_overdraft net_wealth top_i top_i#APP  GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model6
esttab model* using inequal1_w10.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)

reghdfe LTV edu employ has_ccard has_overdraft net_wealth top_i top_i#APP GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model1
reghdfe debt_income own edu employ has_ccard has_overdraft net_wealth top_i top_i#APP GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model2
reghdfe debtservice_income own edu employ has_ccard has_overdraft net_wealth top_i top_i#APP GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe refi edu employ othmm has_ccard has_overdraft net_wealth top_i top_i#APP GDP PPP hp if HMRmm==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
esttab model1 model2 model3 model4 using inequal3_w5.rtf, se nogap compress star (* 0.1 ** 0.05 *** 0.01)

reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth owned top_i top_i#APP GDP PPP hp [pweight=weight], absorb (type Country year) vce(cluster id)
est store model3
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth top_i top_i#APP GDP PPP hp if own==1 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model4
reghdfe buy edu employ othmm  has_ccard has_overdraft net_wealth top_i top_i#APP GDP PPP hp if owned==0 [pweight=weight], absorb (type Country year) vce(cluster id)
est store model5
