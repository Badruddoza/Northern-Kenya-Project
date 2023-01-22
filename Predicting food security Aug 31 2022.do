clear all
set more off, permanent
cd "C:\Users\syedb\OneDrive - Texas Tech University\0. Research\shaikh mahfuz bhai\Food security northern kenya\Prediction"
**cd "C:\Users\sbadrudd\OneDrive - Texas Tech University\0. Research\shaikh mahfuz bhai\Food security northern kenya\Prediction"
use "dissertation data.dta", clear






************************** ANALYSIS ********************************************
** Table A1. Prevalence of food insecurity in the nine counties of the study area
tabulate a06 OrdFSEC, row
** Table 2. Descriptive statistics of the dependent variable **********
tab OrdFSEC
tab OrdFSEC, gen(fsec)
sum fsec*
tabstat fsec*, by(OrdFSEC)



tab Educationcategory, gen(ed)
tab Countycate, gen(fsp)
tabstat fsp*, by(Countycate)
label var fsp1 "No FS program"
label var fsp2 "REGAL-IR program"
label var fsp3 "REGAL-IR and AG program"



* full predictor list without Location1
global fullxvars lndailypercapitafoodexp age Gender1 Gender1_age ed2 ed3 ///
impwater fsp2 fsp3 hhsize numlivestock1 Assets ///
impsanitation electric CashcropFarm ///
Ownland employed offarmacti Povertyline
sum $fullxvars

* predictor list without rural urban (Location1 variable)
global xvars lndailypercapitafoodexp age Gender1 Gender1_age ///
ed2 ed3 Assets   impwater fsp2 fsp3 hhsize 

* predictor list with rural urban (Location1 variable)
global xvars1 lndailypercapitafoodexp age Gender1 Gender1_age ///
ed2 ed3 Assets  Location1 Asset_location  impwater fsp2 fsp3 hhsize 

*** Table 2. Summary statistics for independent variables *****
asdoc sum dailypercapitafoodexp age hhsize Gender1 ed1 ed2 ed3 ///
Assets fsp1 fsp2 fsp3 Location1 numlivestock1 CashcropFarm, title(Overall summary statistics) ///
stat(N mean sd) save(sumstat.doc) replace


* pearson correlation
corr $xvars1
*keep $fullxvars Location1
*export delimited using "corr_matrix.csv", delimiter("@") nolabel datafmt replace

/* stepwise regression
stepwise, pr(.1): reg OrdFSEC $fullxvars Location1
* lndailypercapitafoodexp age impsanitation CashcropFarm ed2 Assets impwater 
* hhsize fsp2 electric numlivestock1
global xvarsbest lndailypercapitafoodexp age impsanitation CashcropFarm ed2 /// 
Assets impwater hhsize fsp2 electric numlivestock1
*/





******* Bayesian information criterion (BIC) for model selection
oprobit OrdFSEC $fullxvars Location1
dis -2*e(ll)+ln(e(N))*e(k)

oprobit OrdFSEC $xvars1
dis -2*e(ll)+ln(e(N))*e(k)

oprobit OrdFSEC $xvarsbest
dis -2*e(ll)+ln(e(N))*e(k)

** BIC is a measure that combines fit and complexity. Fit is measured
* negatively by -2*ln(likelihood), the larger the value, the worse the fit.
* Complexity is measured positivley by ln(N)*k.
* Overall, the smaller the value of the information criterion, the better.




** Table A2. Chi-Square test
do "t test and chi2 test aug 31 2022.do" /*results are in chisquare.xlsx*/



********************************************************************************
************************** Heckman sample selection ****************************
********************************************************************************
capture drop probitxb 
capture drop pdf 
capture drop cdf 
capture drop imr
probit Location1 $xvars CashcropFarm
outreg2 using heckman.doc, replace ctitle("Coefficient") title("Table A3")
mfx, predict()
outreg2 using heckman.doc, mfx ctitle("Heckman first step")

predict probitxb if e(sample), xb
ge pdf=normalden(probitxb)
ge cdf=normal(probitxb)
ge imr=pdf/cdf 
/*use this imr in the outcome equation*/



********************************************************************************
************************* Probit regression results ****************************
********************************************************************************

** Table  3    Regression coefficients and marginal effects (Robust estimation)
/*ssc install mfx2*/
/*Overall model*/
oprobit OrdFSEC $xvars1 imr, vce(robust)
outreg2 using results.doc, replace ctitle("Coefficient") title("Table 3")
mfx, predict(outcome(0))
outreg2 using results.doc, mfx ctitle("Very low FS 0")
mfx, predict(outcome(1)) 
outreg2 using results.doc, mfx ctitle("Low FS 1")
mfx, predict(outcome(2))
outreg2 using results.doc, mfx ctitle("Food Secure 2")

** Table 4 Regression coefficients and marginal effects for Rural households
/*Rural households model*/
oprobit OrdFSEC $xvars imr if Location1==1, vce(robust)
outreg2 using results_rural.doc, replace ctitle("Coefficient") title("Table 4")
mfx, predict(outcome(0))
outreg2 using results_rural.doc, mfx ctitle("Very low FS 0")
mfx, predict(outcome(1)) 
outreg2 using results_rural.doc, mfx ctitle("Low FS 1")
mfx, predict(outcome(2))
outreg2 using results_rural.doc, mfx ctitle("Food Secure 2")

** Table 5 Regression coefficients and marginal effects for Urban households
/*Urban  households model*/
oprobit OrdFSEC $xvars imr if Location1==0, vce(robust)
outreg2 using results_urban.doc, replace ctitle("Coefficient") title("Table 5")
mfx, predict(outcome(0))
outreg2 using results_urban.doc, mfx ctitle("Very low FS 0")
mfx, predict(outcome(1)) 
outreg2 using results_urban.doc, mfx ctitle("Low FS 1")
mfx, predict(outcome(2))
outreg2 using results_urban.doc, mfx ctitle("Food Secure 2")

** Table 6 Regression coefficients and marginal effects for rural households with number of livestock variable
/*Rural households model*/
oprobit OrdFSEC $xvars numlivestock1 imr if Location1==1, vce(robust)
outreg2 using results_rural_livestock.doc, replace ctitle("Coefficient") title("Table 6")
mfx, predict(outcome(0))
outreg2 using results_rural_livestock.doc, mfx ctitle("Very low FS 0")
mfx, predict(outcome(1)) 
outreg2 using results_rural_livestock.doc, mfx ctitle("Low FS 1")
mfx, predict(outcome(2))
outreg2 using results_rural_livestock.doc, mfx ctitle("Food Secure 2")

********************************************************************************
********************************************************************************




********************************************************************************
********************** Logit regression results ********************************
********************************************************************************
** Table  A3    Regression coefficients and marginal effects (Robust estimation)
/*ssc install mfx2*/
/*Overall model*/
ologit OrdFSEC $xvars1 imr, vce(robust)
outreg2 using results.doc, replace ctitle("Coefficient") title("Table A10")
mfx, predict(outcome(0))
outreg2 using results.doc, mfx ctitle("Very low FS 0")
mfx, predict(outcome(1)) 
outreg2 using results.doc, mfx ctitle("Low FS 1")
mfx, predict(outcome(2))
outreg2 using results.doc, mfx ctitle("Food Secure 2")

** Table A4 Regression coefficients and marginal effects for Rural households
/*Rural households model*/
ologit OrdFSEC $xvars imr if Location1==1, vce(robust)
outreg2 using results_rural.doc, replace ctitle("Coefficient") title("Table A11")
mfx, predict(outcome(0))
outreg2 using results_rural.doc, mfx ctitle("Very low FS 0")
mfx, predict(outcome(1)) 
outreg2 using results_rural.doc, mfx ctitle("Low FS 1")
mfx, predict(outcome(2))
outreg2 using results_rural.doc, mfx ctitle("Food Secure 2")


** Table A5 Regression coefficients and marginal effects for Urban households
/*Urban  households model*/
ologit OrdFSEC $xvars imr if Location1==0, vce(robust)
outreg2 using results_urban.doc, replace ctitle("Coefficient") title("Table A12")
mfx, predict(outcome(0))
outreg2 using results_urban.doc, mfx ctitle("Very low FS 0")
mfx, predict(outcome(1)) 
outreg2 using results_urban.doc, mfx ctitle("Low FS 1")
mfx, predict(outcome(2))
outreg2 using results_urban.doc, mfx ctitle("Food Secure 2")


** Table A6 Regression coefficients and marginal effects for rural households with number of livestock variable
/*Rural households model*/
ologit OrdFSEC $xvars numlivestock1 imr if Location1==1, vce(robust)
outreg2 using results_rural_livestock.doc, replace ctitle("Coefficient") title("Table A13")
mfx, predict(outcome(0))
outreg2 using results_rural_livestock.doc, mfx ctitle("Very low FS 0")
mfx, predict(outcome(1)) 
outreg2 using results_rural_livestock.doc, mfx ctitle("Low FS 1")
mfx, predict(outcome(2))
outreg2 using results_rural_livestock.doc, mfx ctitle("Food Secure 2")

********************************************************************************
********************************************************************************




********************************************************************************
************************ Ordered random forest *********************************
********************************************************************************
* save data for R
preserve
_strip_labels _all
keep OrdFSEC lndailypercapitafoodexp age Gender1 Gender1_age ed2 ed3 /// 
Assets Location1 Asset_location impwater fsp2 fsp3 hhsize
export delimited using "rdata.csv", replace
restore
** was run in R

********************************************************************************
********************************************************************************








