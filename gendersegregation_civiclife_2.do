/*
This do-file runs the later parts of the data analyses.
*/

clear all
capture log close
set more off, perm
set seed 199699
*set data path
global data   	"path/stata_datasets"

*using full data set (women and men) for final analyses:
use "${data}/smd.dta", clear

************************
***PCA - GENDER NORMS***
************************
global xlist gendn1_6 gendn2_6 gendn3_6 gendn4_6 gendn5_6
global ncomp 1

describe $xlist
summarize $xlist
corr $xlist

pca $xlist
pca $xlist if woman==0
pca $xlist if woman==1

pca $xlist
screeplot, yline(1) ci(het)
//only one factor above 1 -> only retain one factor. -> explains 36% in the data (see "proportion")
pca $xlist, mineigen(1) //loadings -> there is still quite some unexlpained varaince (up to 79% per item). loadings show the correlations with overall scale. All above .3 which is a usual threshold. 

estat loadings
predict pc1 //append component to the dataset. 
*KMO measure of sampling adequacy
estat kmo //all numbers above .5 is high enough - this is the case for all variables.


***********************
***STARTING ANALYSIS***
***********************

*How often are women and men involved in voluntary organizations (of any composition)?
tabstat anyvol_6 anyvol_10 anystart anyquit if bothwaves==2, by(woman)
ttest anyvol_6 if bothwaves==2, by(woman)
ttest anyvol_10 if bothwaves==2, by(woman)
ttest anystart if bothwaves==2, by(woman)
ttest anyquit if bothwaves==2, by(woman)
*The cross-sectional involvement rates of women and men are very similar (no sign. difference).
*But women are much more likely to start and to quit than men (i.e., their involvement is more volatile).

*Average probability of starting and quitting:
tabstat anystart if starteligible==1
tabstat anyquit if quiteligible==1

*How many people with multiple transitions of the same type?
tab start_nr woman, col // less than 7% of woman and men have multiple transitions of the same type.


*General transitions - joining and quitting:
*Joining
putexcel set ${data}/general_starting_quitting, replace

logit anystart woman if starteligible ==1, vce(robust)
estimates store start1
margins, dydx(*)
mat s1 = r(table)'
putexcel A1 = matrix(s1)

logit anystart woman migrant college cdu6 cd614 cd1418 age_6 religiosity_6 east_6 i.occstfpt_6 anyvol_6 if starteligible ==1, vce(robust)
estimates store start2
margins, dydx(*)
mat s2 = r(table)'
putexcel J1 = matrix(s2)

*Quitting:
logit quitevent woman if quiteligible==1 & qmodel_3mew!=., vce(robust)
estimates store quit1
margins, dydx(*)
mat q1 = r(table)'
putexcel S1 = matrix(q1)

logit quitevent woman migrant college cdu6 cd614 cd1418 age_6 religiosity_6 east_6 i.occstfpt_6 if quiteligible==1 & qmodel_3mew!=., vce(robust)
estimates store quit2
margins, dydx(*)
mat q2 = r(table)'
putexcel AB1 = matrix(q2)

*overview over regression coefficients:
estimates table start1 start2 quit1 quit2, star 

***Are network composition and gender norms linked to the proability to join (any org)?
**among men:
*only friednship networks:
logit anystart ws_friends_6 if woman==0, vce(robust)
est sto any_m_f1
logit anystart ws_friends_all ws_friends_almost_all ws_friends_more_half ws_friends_half ws_friends_less_half ws_friends_almost_none if woman==0, vce(robust)
est sto any_m_f2
*only gender norms:
logit anystart gendn_index_6 if woman==0, vce(robust)
est sto any_m_g
*both networks and norms together:
logit anystart ws_friends_6 gendn_index_6 if woman==0, vce(robust)
est sto any_m_f1_g
logit anystart ws_friends_all ws_friends_almost_all ws_friends_more_half ws_friends_half ws_friends_less_half ws_friends_almost_none gendn_index_6 if woman==0, vce(robust)
est sto any_m_f2_g
*among women:
*only friednship networks:
logit anystart ws_friends_6 if woman==1, vce(robust)
est sto any_w_f1
logit anystart ws_friends_all ws_friends_almost_all ws_friends_more_half ws_friends_half ws_friends_less_half ws_friends_almost_none if woman==1, vce(robust)
est sto any_w_f2
*only gender norms:
logit anystart gendn_index_6 if woman==1, vce(robust)
est sto any_w_g
*both networks and norms together:
logit anystart ws_friends_6 gendn_index_6 if woman==1, vce(robust)
est sto any_w_f1_g
logit anystart ws_friends_all ws_friends_almost_all ws_friends_more_half ws_friends_half ws_friends_less_half ws_friends_almost_none gendn_index_6 if woman==1, vce(robust)
est sto any_w_f2_g
*--> No significant effects of either of these on probability of starting vs not starting. 

esttab any_*

***H1a: COMPOSITION OF ORGANIZATIONS***
*Baseline model:
mlogit starttype_4ur i.woman, baseoutcome(1)
estimates store s_BM
margins woman, pwcompare(pveffects)
margins woman
dis 0.1132/0.0369 //Men are 3.07 times more likely to join VOM than women.
dis 0.1555/0.0578 //Women are 2.69 times more likely to join VOW than men. 
tab woman starttype_4ur, row // the same results as the cross-table of the whole sample
forvalues o = 2(1)4 {
	margins woman, predict(outcome(`o'))
	mat pp_outcome_`o' = r(table)'
	mat list pp_outcome_`o'
}

putexcel set ${data}/pp_baseline_start, replace
putexcel A1 = "Gender"
putexcel B1 = "Organization"
putexcel A2 = "Men"
putexcel A3 = "Women"
putexcel A4 = "Men"
putexcel A5 = "Women"
putexcel A6 = "Men"
putexcel A7 = "Women"
putexcel B2 = "VOM"
putexcel B3 = "VOM"
putexcel B4 = "VOE"
putexcel B5 = "VOE"
putexcel B6 = "VOW"
putexcel B7 = "VOW"
putexcel C1 = matrix(pp_outcome_2), colnames
putexcel C4 = matrix(pp_outcome_3)
putexcel C6 = matrix(pp_outcome_4)


***H2: SOCIAL NETWORKS***
mlogit starttype_4ur i.woman i.wom_sh_fr_6, baseoutcome(1) 
estimates store s_N
margins woman, predict(outcome(2)) 
margins woman, predict(outcome(3)) 
margins woman, predict(outcome(4))
margins woman, pwcompare 
*equivalent model with dummy variables, just to get the Wald statistic:
mlogit starttype_4ur i.woman ws_friends_almost_none ws_friends_less_half ws_friends_half ws_friends_more_half ws_friends_almost_all ws_friends_all, baseoutcome(1) 
test ws_friends_almost_none ws_friends_less_half ws_friends_half ws_friends_more_half ws_friends_almost_all ws_friends_all //Wald test for joint significance of all specified predictors

*Showing the predicted probabilities of joining a VOM, VOE and VOW for different network composition, by gender (hence the interaction effect here)
mlogit starttype_4ur i.woman##c.wom_sh_fr_6, baseoutcome(1)
estimates store s_N_i 
margins, at(wom_sh_fr_6=(1(1)7) woman=(0 1)) predict(outcome(2))
marginsplot
margins, at(wom_sh_fr_6=(1(1)7) woman=(0 1)) predict(outcome(3))
marginsplot
margins, at(wom_sh_fr_6=(1(1)7) woman=(0 1)) predict(outcome(4))
marginsplot

*Compare baseline and initial network model: (I have to use lincom and margins, because lincom only works within one model.)
suest s_BM s_N, coeflegend

*Baseline model - Probability differentials to be join menmajority vs. womenmajority organization. 
margins woman, expression((exp(xb(s_BM_menmajority))/(exp(xb(s_BM_uninvolved))+ exp(xb(s_BM_menmajority))+ exp(xb(s_BM_equal)) + exp(xb(s_BM_womenmajority))))- (exp(xb(s_BM_womenmajority))/(exp(xb(s_BM_uninvolved))+ exp(xb(s_BM_menmajority))+ exp(xb(s_BM_equal)) + exp(xb(s_BM_womenmajority)))))
mat m_baseline = r(table)'
mat list m_baseline

*Network model - Probability differentials to be join menmajority vs. womenmajority organization. 
margins woman, expression((exp(xb(s_N_menmajority))/(exp(xb(s_N_uninvolved))+ exp(xb(s_N_menmajority))+ exp(xb(s_N_equal)) + exp(xb(s_N_womenmajority))))- (exp(xb(s_N_womenmajority))/(exp(xb(s_N_uninvolved))+ exp(xb(s_N_menmajority))+ exp(xb(s_N_equal)) + exp(xb(s_N_womenmajority)))))
mat m_networks = r(table)'
mat list m_networks

*Difference between the probability differentials, incl. significance:
margins woman, expression(((exp(xb(s_BM_menmajority))/(exp(xb(s_BM_uninvolved))+ exp(xb(s_BM_menmajority))+ exp(xb(s_BM_equal)) + exp(xb(s_BM_womenmajority))))- (exp(xb(s_BM_womenmajority))/(exp(xb(s_BM_uninvolved))+ exp(xb(s_BM_menmajority))+ exp(xb(s_BM_equal)) + exp(xb(s_BM_womenmajority))))) - ((exp(xb(s_N_menmajority))/(exp(xb(s_N_uninvolved))+ exp(xb(s_N_menmajority))+ exp(xb(s_N_equal)) + exp(xb(s_N_womenmajority))))- (exp(xb(s_N_womenmajority))/(exp(xb(s_N_uninvolved))+ exp(xb(s_N_menmajority))+ exp(xb(s_N_equal)) + exp(xb(s_N_womenmajority))))))
mat m_difference = r(table)'
mat list m_difference

putexcel set ${data}/networks_all, replace
putexcel A2 = "Men - baseline"
putexcel A3 = "Women - baseline"
putexcel A4 = "Men - [+networks]"
putexcel A5 = "Women - [+networks]"
putexcel A6 = "Men - difference"
putexcel A7 = "Women - difference"
putexcel B1 = matrix(m_baseline), colnames
putexcel B4 = matrix(m_networks)
putexcel B6 = matrix(m_difference)


***H2: SOCIAL NETWORKS - ROBUSTNESS CHECKS***
*The same models as above, but taking the control variables into account.
*First, the baseline model.
mlogit starttype_4ur i.woman migrant college cdu6 cd614 cd1418 age_6 religiosity_6 east_6 i.occstfpt_6 anyvol_6, baseoutcome(1)
estimates store s_BM_r
margins woman, pwcompare 
esttab s_BM_r, star b(3) se(3) wide

forvalues o = 2(1)4 {
	margins woman, predict(outcome(`o'))
	mat pp_outcome_`o' = r(table)'
	mat list pp_outcome_`o'
}

putexcel set ${data}/pp_baseline_start_r, replace
putexcel A1 = "Gender"
putexcel B1 = "Organization"
putexcel A2 = "Men"
putexcel A3 = "Women"
putexcel A4 = "Men"
putexcel A5 = "Women"
putexcel A6 = "Men"
putexcel A7 = "Women"
putexcel B2 = "VOM"
putexcel B3 = "VOM"
putexcel B4 = "VOE"
putexcel B5 = "VOE"
putexcel B6 = "VOW"
putexcel B7 = "VOW"
putexcel C1 = matrix(pp_outcome_2), colnames
putexcel C4 = matrix(pp_outcome_3)
putexcel C6 = matrix(pp_outcome_4)


*Second, the networks model.
mlogit starttype_4ur i.woman i.wom_sh_fr_6 migrant college cdu6 cd614 cd1418 age_6 religiosity_6 east_6 i.occstfpt_6 anyvol_6, baseoutcome(1)
estimates store s_N_r
margins woman, pwcompare 
*equivalent model with dummy variables, just to get the Wald statistic:
mlogit starttype_4ur i.woman ws_friends_almost_none ws_friends_less_half ws_friends_half ws_friends_more_half ws_friends_almost_all ws_friends_all migrant college cdu6 cd614 cd1418 age_6 religiosity_6 i.occstfpt_6 anyvol_6, baseoutcome(1)
test ws_friends_almost_none ws_friends_less_half ws_friends_half ws_friends_more_half ws_friends_almost_all ws_friends_all //Wald test for joint significance of all specified predictors

*Compare baseline and initial network model: (I have to use lincom and margins, because lincom only works within one model.)
suest s_BM_r s_N_r, coeflegend

*Baseline model - Probability differentials to join menmajority vs. womenmajority organization. 
margins woman, expression((exp(xb(s_BM_r_menmajority))/(exp(xb(s_BM_r_uninvolved))+ exp(xb(s_BM_r_menmajority))+ exp(xb(s_BM_r_equal)) + exp(xb(s_BM_r_womenmajority))))- (exp(xb(s_BM_r_womenmajority))/(exp(xb(s_BM_r_uninvolved))+ exp(xb(s_BM_r_menmajority))+ exp(xb(s_BM_r_equal)) + exp(xb(s_BM_r_womenmajority)))))
mat m_baseline = r(table)'
mat list m_baseline

*Network model - Probability differentials to be join menmajority vs. womenmajority organization. 
margins woman, expression((exp(xb(s_N_r_menmajority))/(exp(xb(s_N_r_uninvolved))+ exp(xb(s_N_r_menmajority))+ exp(xb(s_N_r_equal)) + exp(xb(s_N_r_womenmajority))))- (exp(xb(s_N_r_womenmajority))/(exp(xb(s_N_r_uninvolved))+ exp(xb(s_N_r_menmajority))+ exp(xb(s_N_r_equal)) + exp(xb(s_N_r_womenmajority)))))
mat m_networks = r(table)'
mat list m_networks

*Difference between the probability differentials, incl. significance:
margins woman, expression(((exp(xb(s_BM_r_menmajority))/(exp(xb(s_BM_r_uninvolved))+ exp(xb(s_BM_r_menmajority))+ exp(xb(s_BM_r_equal)) + exp(xb(s_BM_r_womenmajority))))- (exp(xb(s_BM_r_womenmajority))/(exp(xb(s_BM_r_uninvolved))+ exp(xb(s_BM_r_menmajority))+ exp(xb(s_BM_r_equal)) + exp(xb(s_BM_r_womenmajority))))) - ((exp(xb(s_N_r_menmajority))/(exp(xb(s_N_r_uninvolved))+ exp(xb(s_N_r_menmajority))+ exp(xb(s_N_r_equal)) + exp(xb(s_N_r_womenmajority))))- (exp(xb(s_N_r_womenmajority))/(exp(xb(s_N_r_uninvolved))+ exp(xb(s_N_r_menmajority))+ exp(xb(s_N_r_equal)) + exp(xb(s_N_r_womenmajority))))))
mat m_difference = r(table)'
mat list m_difference

putexcel set ${data}/networks_all_r, replace
putexcel A2 = "Men - baseline"
putexcel A3 = "Women - baseline"
putexcel A4 = "Men - [+networks]"
putexcel A5 = "Women - [+networks]"
putexcel A6 = "Men - difference"
putexcel A7 = "Women - difference"
putexcel B1 = matrix(m_baseline), colnames
putexcel B4 = matrix(m_networks)
putexcel B6 = matrix(m_difference)

*Including all the control variables hardly changes the mediating role of networks. 


***H3: GENDER NORMS***
*Now, we investigate whether the probability differentials are larger among people with more traditional gender values. This is a moderation effect. I compare the probability of joining a VOM vs a VOW among people with the same score on the gender norm variable (separately for women and men). The interaction effect also gives credit to the fact that traditional gender norms imply the opposite for women and men in this respect (joining VOM vs. VOW). 

*Percentile variable:
xtile gendn_ind4 = gendn_index_6, n(4)
tab gendn_ind4

*Loop over gender (g) and values of gender norms (k)
forvalues g = 0(1)1 {
forvalues k = 1(1)4 {
	mlogit starttype_4ur woman##c.gendn_ind4 if finsamstart==1, baseoutcome(1)
	margins if woman==`g' & gendn_ind4==`k' & finsamstart==1, coeflegend post //predicted probability for each outcome level (uninvolved, VOM, VOE, VOW) for the subgroup of those with particular values on woman and gendn1_n variable.
	lincom   _b[2._predict] -  _b[4._predict] //probability differential VOM vs VOW.
	mat res_`k' = [r(estimate), r(se), r(lb), r(ub)]
}
mat fin_`g' = [res_1 \ res_2 \ res_3 \ res_4] //store results in matrix 
mat colnames fin_`g' = coef se lci hci
mat rownames fin_`g' = "Very egalitarian" "Egalitarian" "Traditional" "Very traditional"
}

mat list fin_0 //results for men
mat list fin_1 //results for women

*store the results in excel
putexcel set ${data}/gendn_index_men, replace
putexcel A1 = matrix(fin_0), colnames

putexcel set ${data}/gendn_index_women, replace
putexcel A1 = matrix(fin_1), colnames


*calculate z-value for difference between differentials for ppl with most traditional vs. most egalitarian values.
*men:
dis (fin_0[1,1]-fin_0[4,1])/sqrt(fin_0[1,2]^2 + fin_0[4,2]^2) //v.e. vs v.t. -> sign.
dis norm(3.371223)
dis (fin_0[1,1]-fin_0[3,1])/sqrt(fin_0[1,2]^2 + fin_0[3,2]^2) //v.e. vs t. -> sign.
dis (fin_0[1,1]-fin_0[2,1])/sqrt(fin_0[1,2]^2 + fin_0[2,2]^2) //v.e. vs e. -> n.sign.
dis (fin_0[2,1]-fin_0[4,1])/sqrt(fin_0[2,2]^2 + fin_0[4,2]^2) //e. vs v.t. -> sign.
dis (fin_0[2,1]-fin_0[3,1])/sqrt(fin_0[2,2]^2 + fin_0[3,2]^2) //e. vs t. -> n.sign.
dis (fin_0[3,1]-fin_0[4,1])/sqrt(fin_0[3,2]^2 + fin_0[4,2]^2) //t. vs. v.t. -> n.sign 
*--> all groups that are at least two steps apart differ significantly from each other. 

*women:
dis (fin_1[1,1]-fin_1[4,1])/sqrt(fin_1[1,2]^2 + fin_1[4,2]^2) //v.e. vs v.t. -> n. sign.
dis (fin_1[1,1]-fin_1[3,1])/sqrt(fin_1[1,2]^2 + fin_1[3,2]^2) //v.e. vs t. -> n. sign.
dis (fin_1[1,1]-fin_1[2,1])/sqrt(fin_1[1,2]^2 + fin_1[2,2]^2) //v.e. vs e. -> n. sign.
dis (fin_1[2,1]-fin_1[4,1])/sqrt(fin_1[2,2]^2 + fin_1[4,2]^2) //e. vs v.t. -> n. sign.
dis (fin_1[2,1]-fin_1[3,1])/sqrt(fin_1[2,2]^2 + fin_1[3,2]^2) //e. vs t. -> n.sign.
dis (fin_1[3,1]-fin_1[4,1])/sqrt(fin_1[3,2]^2 + fin_1[4,2]^2) //t. vs. v.t. -> n.sign 


***H3: GENDER NORMS - Robustness check: Including control variables***
*Loop over gender (g) and values of gender norms (k)
forvalues g = 0(1)1 {
forvalues k = 1(1)4 {
	mlogit starttype_4ur woman##c.gendn_ind4 woman#migrant woman#college woman#cdu6 woman#cd614 woman#cd1418 woman#c.age_6 woman#c.religiosity_6 woman#east_6 woman#i.occstfpt_6 woman#anyvol_6 if finsamstart==1, baseoutcome(1)
	margins if woman==`g' & gendn_ind4==`k' & finsamstart==1, coeflegend post //predicted probability for each outcome level (uninvolved, VOM, VOE, VOW) for the subgroup of those with particular values on woman and gendn1_n variable.
	lincom   _b[2._predict] -  _b[4._predict] //probability differential VOM vs VOW.
	mat res_`k' = [r(estimate), r(se), r(lb), r(ub)]
}
mat fin_`g' = [res_1 \ res_2 \ res_3 \ res_4] //store results in matrix 
mat colnames fin_`g' = coef se lci hci
mat rownames fin_`g' = "Very egalitarian" "Egalitarian" "Traditional" "Very traditional"
}

mat list fin_0 //results for men
mat list fin_1 //results for women

*store the results in excel
putexcel set ${data}/gendn_index_men_r, replace
putexcel A1 = matrix(fin_0), colnames

putexcel set ${data}/gendn_index_women_r, replace
putexcel A1 = matrix(fin_1), colnames


*calculate z-value for difference between differentials for ppl with most traditional vs. most egalitarian values.
*men:
dis (fin_0[1,1]-fin_0[4,1])/sqrt(fin_0[1,2]^2 + fin_0[4,2]^2) //v.e. vs v.t. -> sign.
dis (fin_0[1,1]-fin_0[3,1])/sqrt(fin_0[1,2]^2 + fin_0[3,2]^2) //v.e. vs t. -> sign.
dis (fin_0[1,1]-fin_0[2,1])/sqrt(fin_0[1,2]^2 + fin_0[2,2]^2) //v.e. vs e. -> n.sign.
dis (fin_0[2,1]-fin_0[4,1])/sqrt(fin_0[2,2]^2 + fin_0[4,2]^2) //e. vs v.t. -> sign.
dis (fin_0[2,1]-fin_0[3,1])/sqrt(fin_0[2,2]^2 + fin_0[3,2]^2) //e. vs t. -> n.sign.
dis (fin_0[3,1]-fin_0[4,1])/sqrt(fin_0[3,2]^2 + fin_0[4,2]^2) //t. vs. v.t. -> n.sign 
*--> all groups that are at least two steps apart differ significantly from each other. 

*women:
dis (fin_1[1,1]-fin_1[4,1])/sqrt(fin_1[1,2]^2 + fin_1[4,2]^2) //v.e. vs v.t. -> n. sign.
dis (fin_1[1,1]-fin_1[3,1])/sqrt(fin_1[1,2]^2 + fin_1[3,2]^2) //v.e. vs t. -> n. sign.
dis (fin_1[1,1]-fin_1[2,1])/sqrt(fin_1[1,2]^2 + fin_1[2,2]^2) //v.e. vs e. -> n. sign.
dis (fin_1[2,1]-fin_1[4,1])/sqrt(fin_1[2,2]^2 + fin_1[4,2]^2) //e. vs v.t. -> n. sign.
dis (fin_1[2,1]-fin_1[3,1])/sqrt(fin_1[2,2]^2 + fin_1[3,2]^2) //e. vs t. -> n.sign.
dis (fin_1[3,1]-fin_1[4,1])/sqrt(fin_1[3,2]^2 + fin_1[4,2]^2) //t. vs. v.t. -> n.sign 


***Re-Do joining analysis with involvement ***

*Baseline model:
gen ws_volorg6_4cat = ws_volorg6_3cat
replace ws_volorg6_4cat = 0 if ws_volorg6_3cat==.
label define ws_volorg6_4cat 0"not involved" 1"men-dominated" 2"gender-integrated" 3"women-dominated"
label values ws_volorg6_4cat ws_volorg6_4cat

mlogit ws_volorg6_4cat i.woman, baseoutcome(0)
estimates store s_BM_cs //cross sectional results
margins woman, pwcompare(pveffects)
margins woman

forvalues o = 1(1)3 {
	margins woman, predict(outcome(`o'))
	mat pp_outcome_`o' = r(table)'
	mat list pp_outcome_`o'
}

putexcel set ${data}/pp_baseline_cross_sect, replace
putexcel A1 = "Gender"
putexcel B1 = "Organization"
putexcel A2 = "Men"
putexcel A3 = "Women"
putexcel A4 = "Men"
putexcel A5 = "Women"
putexcel A6 = "Men"
putexcel A7 = "Women"
putexcel B2 = "VOM"
putexcel B3 = "VOM"
putexcel B4 = "VOE"
putexcel B5 = "VOE"
putexcel B6 = "VOW"
putexcel B7 = "VOW"
putexcel C1 = matrix(pp_outcome_1), colnames
putexcel C4 = matrix(pp_outcome_2)
putexcel C6 = matrix(pp_outcome_3)

*Including networks:
mlogit ws_volorg6_4cat i.woman i.wom_sh_fr_6, baseoutcome(0) 
estimates store s_N_cs // cross-sectional results
margins woman, predict(outcome(1)) 
margins woman, predict(outcome(2)) 
margins woman, predict(outcome(3))
margins woman, pwcompare 
*equivalent model with dummy variables, just to get the Wald statistic:
mlogit ws_volorg6_4cat i.woman ws_friends_almost_none ws_friends_less_half ws_friends_half ws_friends_more_half ws_friends_almost_all ws_friends_all, baseoutcome(0) 
test ws_friends_almost_none ws_friends_less_half ws_friends_half ws_friends_more_half ws_friends_almost_all ws_friends_all //Wald test for joint significance of all specified predictors

*Compare baseline and initial network model: (I have to use lincom and margins, because lincom only works within one model.)
suest s_BM_cs s_N_cs, coeflegend

*Baseline model - Probability differentials to be join menmajority vs. womenmajority organization. 
margins woman, expression((exp(xb(s_BM_cs_men_dominated))/(exp(xb(s_BM_cs_not_involved))+ exp(xb(s_BM_cs_men_dominated))+ exp(xb(s_BM_cs_gender_integrated)) + exp(xb(s_BM_cs_women_dominated))))- (exp(xb(s_BM_cs_women_dominated))/(exp(xb(s_BM_cs_not_involved))+ exp(xb(s_BM_cs_men_dominated))+ exp(xb(s_BM_cs_gender_integrated)) + exp(xb(s_BM_cs_women_dominated)))))
mat m_baseline = r(table)'
mat list m_baseline

*Network model - Probability differentials to be join menmajority vs. womenmajority organization. 
margins woman, expression((exp(xb(s_N_cs_men_dominated))/(exp(xb(s_N_cs_not_involved))+ exp(xb(s_N_cs_men_dominated))+ exp(xb(s_N_cs_gender_integrated)) + exp(xb(s_N_cs_women_dominated))))- (exp(xb(s_N_cs_women_dominated))/(exp(xb(s_N_cs_not_involved))+ exp(xb(s_N_cs_men_dominated))+ exp(xb(s_N_cs_gender_integrated)) + exp(xb(s_N_cs_women_dominated)))))
mat m_networks = r(table)'
mat list m_networks

*Difference between the probability differentials, incl. significance:
margins woman, expression(((exp(xb(s_BM_cs_men_dominated))/(exp(xb(s_BM_cs_not_involved))+ exp(xb(s_BM_cs_men_dominated))+ exp(xb(s_BM_cs_gender_integrated)) + exp(xb(s_BM_cs_women_dominated))))- (exp(xb(s_BM_cs_women_dominated))/(exp(xb(s_BM_cs_not_involved))+ exp(xb(s_BM_cs_men_dominated))+ exp(xb(s_BM_cs_gender_integrated)) + exp(xb(s_BM_cs_women_dominated))))) - ((exp(xb(s_N_cs_men_dominated))/(exp(xb(s_N_cs_not_involved))+ exp(xb(s_N_cs_men_dominated))+ exp(xb(s_N_cs_gender_integrated)) + exp(xb(s_N_cs_women_dominated))))- (exp(xb(s_N_cs_women_dominated))/(exp(xb(s_N_cs_not_involved))+ exp(xb(s_N_cs_men_dominated))+ exp(xb(s_N_cs_gender_integrated)) + exp(xb(s_N_cs_women_dominated))))))
mat m_difference = r(table)'
mat list m_difference

putexcel set ${data}/networks_all_cs, replace
putexcel A2 = "Men - baseline"
putexcel A3 = "Women - baseline"
putexcel A4 = "Men - [+networks]"
putexcel A5 = "Women - [+networks]"
putexcel A6 = "Men - difference"
putexcel A7 = "Women - difference"
putexcel B1 = matrix(m_baseline), colnames
putexcel B4 = matrix(m_networks)
putexcel B6 = matrix(m_difference)

*Gender norms:
*Loop over gender (g) and values of gender norms (k)
forvalues g = 0(1)1 {
forvalues k = 1(1)4 {
	mlogit ws_volorg6_4cat woman##c.gendn_ind4 if finsamstart==1, baseoutcome(0)
	margins if woman==`g' & gendn_ind4==`k' & finsamstart==1, coeflegend post //predicted probability for each outcome level (uninvolved, VOM, VOE, VOW) for the subgroup of those with particular values on woman and gendn1_n variable.
	lincom   _b[2._predict] -  _b[4._predict] //probability differential VOM vs VOW.
	mat res_`k' = [r(estimate), r(se), r(lb), r(ub)]
}
mat fin_`g' = [res_1 \ res_2 \ res_3 \ res_4] //store results in matrix 
mat colnames fin_`g' = coef se lci hci
mat rownames fin_`g' = "Very egalitarian" "Egalitarian" "Traditional" "Very traditional"
}

mat list fin_0 //results for men
mat list fin_1 //results for women

*store the results in excel
putexcel set ${data}/gendn_index_men_cs, replace
putexcel A1 = matrix(fin_0), colnames

putexcel set ${data}/gendn_index_women_cs, replace
putexcel A1 = matrix(fin_1), colnames

***********************
***QUITTING ANALYSIS***
***********************


tab ws_qmodel quitevent if woman==0, row
tab ws_qmodel quitevent if woman==1, row

tab qmodel_3mew quitevent if woman==0, row
tab qmodel_3mew quitevent if woman==1, row
tab quitvot_f qmodel_3mew, row

*t-tests
ttest quitevent if ws_qmodel_3cat<3, by(ws_qmodel_3cat)
ttest quitevent if ws_qmodel_3cat>1, by(ws_qmodel_3cat)
ttest quitevent if ws_qmodel_3cat!=2, by(ws_qmodel_3cat)

***H1b: QUITTING***
*quitting model:
logit quitevent woman##i.qmodel_3mew, vce(robust)
est sto BM_q
margins, at(woman=(0 1) qmodel_3mew=(1 2 3)) //predicted probability for each outcome level (uninvolved, VOM, VOE, VOW) for the subgroup of those involved in particular type of org. 
margins, at(woman=(0 1) qmodel_3mew=(1 2 3)) pwcompare(pveffects)
*comparisons 1 vs 4 insig. (among VOM), 2 vs 5 sign (among VOE), 3 vs 6 insig (among VOW). 
dis 0.4903/0.3569 
mat quit = r(table)'
mat colnames quit = coef se z_value p_value lci hci df crit_val eform

mat list quit

putexcel set ${data}/quit, replace
putexcel C1 = matrix(quit), colnames
putexcel A1 = "Gender"
putexcel A2 = "Men"
putexcel A3 = "Men"
putexcel A4 = "Men"
putexcel A5 = "Women"
putexcel A6 = "Women"
putexcel A7 = "Women"
putexcel B1 = "Organization"
putexcel B2 = "VOM"
putexcel B3 = "VOE"
putexcel B4 = "VOW"
putexcel B5 = "VOM"
putexcel B6 = "VOE"
putexcel B7 = "VOW"

*pairwise comparisons:
qui logit quitevent woman##i.qmodel_3mew, vce(robust)
margins woman#qmodel_3mew, pwcompare(pveffects)
*Men: no big differences, only more likely to quit in VOW than in VOE. 
*Women: in contrast to our hypothesis: more likely to quit in VOM than in VOE or VOW. 

*Quitting - with controls:
logit quitevent woman##i.qmodel_3mew migrant college cdu6 cd614 cd1418 age_6 religiosity_6 i.occstfpt_6 anyvol_6, vce(robust)
est sto BM_q_r
margins woman#qmodel_3mew, pwcompare(pveffects)

margins, at(woman=(0 1) qmodel_3mew=(1 2 3)) pwcompare(pveffects)
mat quit_r = r(table)'

mat list quit_r

putexcel set ${data}/quit_r, replace
putexcel C1 = matrix(quit_r), colnames
putexcel A1 = "Gender"
putexcel A2 = "Men"
putexcel A3 = "Men"
putexcel A4 = "Men"
putexcel A5 = "Women"
putexcel A6 = "Women"
putexcel A7 = "Women"
putexcel B1 = "Organization"
putexcel B2 = "VOM"
putexcel B3 = "VOE"
putexcel B4 = "VOW"
putexcel B5 = "VOM"
putexcel B6 = "VOE"
putexcel B7 = "VOW"