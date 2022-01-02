/*
"You Cannot Be Serious" Analysis File
Markus Prior, Gaurav Sood, and Kabir Khanna
Updated: September 7, 2015
*/

*Set working directory
cd ""

* Set GLLAMM directory to working directory
local dir `c(pwd)'
adopath + dir


*** Tables ***

*Table 1

	*Proportion Congenial, Correct, and Uncongenial by Condition
	use "psk_long_recode.dta", clear
	
	proportion cong_33 if r_bushref == 0, over(pay)
	proportion cong_33 if pollid == 2004 & r_bushref == 0, over(pay)
	proportion cong_33 if pollid == 2008 & r_bushref == 0, over(rd)
	
	*Bootstrap Standard Errors
	run "PSK_Functions.do" /*Must run PSK_Functions.do to define functions called by boostrap commands below*/

	*Studies 1 & 2
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum congenial if r_bushref == 0 & pay == 0 /*SE: .011*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum correct if r_bushref == 0 & pay == 0 /*SE: .012*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum uncongenial if r_bushref == 0 & pay == 0 /*SE: .010*/

	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum congenial if r_bushref == 0 & pay == 1 /*SE: .012*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum correct if r_bushref == 0 & pay == 1 /*SE: .013*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum uncongenial if r_bushref == 0 & pay == 1 /*SE: .010*/
	
	bootstrap eff_cong = r(diff), cluster(caseid) reps(1000) seed(123456): eff_cong /*SE=.016*/
	bootstrap eff_corr = r(diff), cluster(caseid) reps(1000) seed(123456): eff_corr /*SE=.017*/
	bootstrap eff_uncong = r(diff), cluster(caseid) reps(1000) seed(123456): eff_uncong /*SE=.014*/

	bootstrap bias_pay0 = r(diff), cluster(caseid) reps(1000) seed(123456): bias_pay0 /*SE=.017*/
	bootstrap bias_pay1 = r(diff), cluster(caseid) reps(1000) seed(123456): bias_pay1 /*SE=.017*/	

	bootstrap did = r(diff), cluster(caseid) reps(1000) seed(123456): did /*SE=.026*/

	*Study 1
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum congenial if pollid == 2004 & pay == 0 /*SE: .016*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum correct if pollid == 2004 & pay == 0 /*SE: .018*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum uncongenial if pollid == 2004 & pay == 0 /*SE: .013*/
	
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum congenial if pollid == 2004 & pay == 1 /*SE: .017*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum correct if pollid == 2004 & pay == 1 /*SE: .017*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum uncongenial if pollid == 2004 & pay == 1 /*SE: .014*/

	bootstrap eff_cong1 = r(diff), cluster(caseid) reps(1000) seed(123456): eff_cong1 /*SE=.023*/
	bootstrap eff_corr1 = r(diff), cluster(caseid) reps(1000) seed(123456): eff_corr1 /*SE=.023*/
	bootstrap eff_uncong1 = r(diff), cluster(caseid) reps(1000) seed(123456): eff_uncong1 /*SE=.019*/

	bootstrap bias_pay0 = r(diff), cluster(caseid) reps(1000) seed(123456): bias_pay01 /*SE=.022*/
	bootstrap bias_pay1 = r(diff), cluster(caseid) reps(1000) seed(123456): bias_pay11 /*SE=.025*/

	bootstrap did1 = r(diff), cluster(caseid) reps(1000) seed(123456): did1 /*SE=.034*/

	*Study 2
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum congenial if pollid == 2008 & r_bushref == 0 & rd == 0 /*SE: .015*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum correct if pollid == 2008 & r_bushref == 0 & rd == 0 /*SE: .015*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum uncongenial if pollid == 2008 & r_bushref == 0 & rd == 0 /*SE: .014*/
	
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum congenial if pollid == 2008 & r_bushref == 0 & rd == 1 /*SE: .014*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum correct if pollid == 2008 & r_bushref == 0 & rd == 1 /*SE: .015*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum uncongenial if pollid == 2008 & r_bushref == 0 & rd == 1 /*SE: .014*/

	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum congenial if pollid == 2008 & r_bushref == 0 & rd == 2 /*SE: .016*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum correct if pollid == 2008 & r_bushref == 0 & rd == 2 /*SE: .018*/
	bootstrap mean=r(mean), cluster(caseid) reps(1000) seed(123456): sum uncongenial if pollid == 2008 & r_bushref == 0 & rd == 2 /*SE: .014*/

	bootstrap app_cong = r(diff), cluster(caseid) reps(1000) seed(123456): app_cong /*SE=.021*/
	bootstrap app_corr = r(diff), cluster(caseid) reps(1000) seed(123456): app_corr /*SE=.022*/
	bootstrap app_uncong = r(diff), cluster(caseid) reps(1000) seed(123456): app_uncong /*SE=.021*/

	bootstrap mon_cong = r(diff), cluster(caseid) reps(1000) seed(123456): mon_cong /*SE=.023*/
	bootstrap mon_corr = r(diff), cluster(caseid) reps(1000) seed(123456): mon_corr /*SE=.023*/
	bootstrap mon_uncong = r(diff), cluster(caseid) reps(1000) seed(123456): mon_uncong /*SE=.022*/

	bootstrap bias_rd0 = r(diff), cluster(caseid) reps(1000) seed(123456): bias_rd0 /*SE=.026*/
	bootstrap bias_rd1 = r(diff), cluster(caseid) reps(1000) seed(123456): bias_rd1 /*SE=.023*/
	bootstrap bias_rd2 = r(diff), cluster(caseid) reps(1000) seed(123456): bias_rd2 /*SE=.026*/

	bootstrap app_bias = r(diff), cluster(caseid) reps(1000) seed(123456): app_bias /*SE=.035*/
	bootstrap mon_bias = r(diff), cluster(caseid) reps(1000) seed(123456): mon_bias /*SE=.038*/

	*N(Responses) by Condition
	tab pay if r_bushref == 0 & cong_33 != .
	tab pay if pollid == 2004 & r_bushref == 0 & cong_33 != .
	tab rd if pollid == 2008 & r_bushref == 0 & cong_33 != .

	*N(Respondents) by Condition - only include partisans that responded to at least one item
	use "psk_wide_recode.dta", clear

	gen flag08 = 0 if pollid == 2008
	replace flag08 = 1 if pollid == 2008 & (l1 != . | l2 != . | l3 != . | l4 != . | l5 != .)
	gen flag04 = 0 if pollid == 2004
	replace flag04 = 1 if pollid == 2004 & (l6 != . | l7 != . | l8 != . | l9 != . | l10 != .)

	tab rd if dem != . & r_bushref == 0 & rd != 1 & (flag04 == 1 | flag08 == 1)
	tab rd if dem != . & r_bushref == 0 & pollid == 2004 & flag04 == 1
	tab rd if dem != . & r_bushref == 0 & pollid == 2008 & flag08 == 1


*Table 2
use "psk_long_recode.dta", clear

	*Column 1
	gllamm cong_33 pay seq1 seq2 seq3 seq4 seq5 seq6 seq7 seq8 seq9 if r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	estimates store tab2col1
	
	*Column 2a
	gllamm cong_33 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	estimates store tab2col2a
	test mon = app

	*Column 2b
	gllamm cong_33 acc seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)

	*Column 3a
	gllamm cong_33 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0 & consis == 1, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon = app

	*Column 3b
	gllamm cong_33 acc seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0 & consis == 1, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)


*Table 3

	*Column 1
	gllamm cong_33 mon app r_bushref monxbush appxbush seq1 seq2 seq3 seq4, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	estimates store tab3col1
	test (mon = app) (monxbush = appxbush)

	*Column 2
	gllamm cong_33 acc r_bushref accxbush seq1 seq2 seq3 seq4, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	estimates store tab3col2


*Table 4 (Note: takes long time to converge!)
	gllamm cong_33r acc pk accxpk seq1 seq2 seq3 seq4, i(caseid) link(mlogit) adapt prior(gamma, scale(10000) shape(2) var)
	estimates store tab4

	*Hausman-McFadden Test of IIA
	gllamm cong_33r acc pk accxpk seq1 seq2 seq3 seq4 if cong_33 !=  1, i(caseid) link(mlogit) adapt prior(gamma, scale(10000) shape(2) var)
	estimates store omit1
	hausman tab4 omit1, alleqs constant /*Chi(9) = 1.66 (.9958)*/

	gllamm cong_33r acc pk accxpk seq1 seq2 seq3 seq4 if cong_33 !=  0, i(caseid) link(mlogit) adapt prior(gamma, scale(10000) shape(2) var)
	estimates store omit2
	hausman tab4 omit2, alleqs constant /*Chi(9) = .58 (.9999)*/

	gllamm cong_33r acc pk accxpk seq1 seq2 seq3 seq4 if cong_33 != -1, i(caseid) link(mlogit) adapt prior(gamma, scale(10000) shape(2) var)
	estimates store omit3
	hausman tab4 omit3, alleqs constant /*Chi(9) = -21.26*/

	*Test whether relationship is same for both types of incentive (footnote 12)
	gllamm cong_33r mon app pk monxpk appxpk seq1 seq2 seq3 seq4, i(caseid) link(mlogit) adapt prior(gamma, scale(10000) shape(2) var)
	lrtest tab4 /*Chi2(4)=6.47, p = .1667*/


*** Figures ***

*Figure 1
label define cond 0 "Control" 1 "Accuracy Appeal" 2 "Monetary Incentive"	

	*Panel A: Studies 1 & 2
	
	*Generate predicted probability for each observation (note: confidence intervals calculated using Jags in R)
	estimates restore tab2col1
	gllapred p1, mu marginal above(-1)
	gllapred p2, mu marginal above(0)
	
	mean p1 if pay == 0
	mat p_uncong_pay0 = 100 - 100 * e(b)
	mean p2 if pay == 0
	mat p_cong_pay0 = 100 * e(b)
		mean p1 if pay == 1
	mat p_uncong_pay1 = 100 - 100 * e(b)
	mean p2 if pay == 1
	mat p_cong_pay1 = 100 * e(b)	
	mat p1 = (p_uncong_pay0, p_uncong_pay1, p_cong_pay0, p_cong_pay1)

	matrix pay=(0,2,0,2)
	matrix cong=(-1,-1,1,1)
	matrix lo = (25.35, 27.85, 36.92, 33.95) /*Calculated 95% CI in PSK_JAGS.R (output in study12jagsresults.csv)*/
	matrix hi = (28.74, 31.33, 40.73, 37.92) /*Calculated 95% CI in PSK_JAGS.R (output in study12jagsresults.csv)*/
	matrix ppay=pay',cong',p1',lo',hi'
	matrix list ppay
	svmat ppay, names(pp)
	label val pp1 cond

	twoway (bar pp3 pp2, barw(1) fcolor(white) lwidth(thin) lcolor(black)) (rcap pp4 pp5 pp2), by(pp1, col(2) legend(off) note("") bgcolor(white) plotregion(lcolor(none) lwidth(zero) ilcolor(none) style(none) color(white)) graphregion(color(white))) ///
		yscale(range(0, 60)) ///
		ylabel(0 (10) 60, nogrid) ///
		xscale(range(0, 1)) ///
		xlabel(-1 "Uncongenial" 1 "Congenial") ///
		xtitle("") ///
		ytitle("")


	*Panel B: Study 2 Only

	*Generate predicted probability for each observation (http://www.gllamm.org/gllapred.hlp)
	estimates restore tab2col2a
	gllapred p1rd, mu marginal above(-1)
	gllapred p2rd, mu marginal above(0)
	
	mean p1rd if rd == 0
	mat p_uncong_rd0 = 100 - 100 * e(b)
	mean p2rd if rd == 0
	mat p_cong_rd0 = 100 * e(b)
	
	mean p1rd if rd == 1
	mat p_uncong_rd1 = 100 - 100 * e(b)
	mean p2rd if rd == 1
	mat p_cong_rd1 = 100 * e(b)	

	mean p1rd if rd == 2
	mat p_uncong_rd2 = 100 - 100 * e(b)
	mean p2rd if rd == 2
	mat p_cong_rd2 = 100 * e(b)	
	
	mat p1rd = (p_uncong_rd0, p_uncong_rd1, p_uncong_rd2, p_cong_rd0, p_cong_rd1, p_cong_rd2)

	matrix rd=(0,1,2,0,1,2)
	matrix cong2=(-1,-1,-1,1,1,1)
	matrix lo2 = (25.98, 29.06, 28.50, 35.45, 32.30, 32.30) /*Calculated 95% CI in PSK_JAGS.R (output in study2jagsresults.csv)*/
	matrix hi2 = (30.60, 33.47, 33.43, 40.56, 37.03, 37.86) /*Calculated 95% CI in PSK_JAGS.R (output in study2jagsresults.csv)*/
	matrix ppay2=rd',cong2',p1rd',lo2',hi2'
	matrix list ppay2
	svmat ppay2, names(ppay2)
	label val ppay21 cond

	twoway (bar ppay23 ppay22, barw(1) fcolor(white) lwidth(thin) lcolor(black)) (rcap ppay24 ppay25 ppay22), by(ppay21, col(3) legend(off) note("") bgcolor(white) plotregion(lcolor(none) lwidth(zero) ilcolor(none) style(none) color(white)) graphregion(color(white))) ///
		yscale(range(0, 60)) ///
		ylabel(0 (10) 60, nogrid) ///
		xscale(range(0, 1)) ///
		xlabel(-1 "Uncongenial" 1 "Congenial") ///
		xtitle("") ///
		ytitle("")


*Figure 2

	*Panel A: Raw Data (Lowess Smoothed)
	gen prob0 = 1 - correct - congenial
	gen prob1 = 1 - congenial

	twoway  (lowess prob0 pk if acc == 0, lpattern(dash) bw(.6) lw(med) lcol("0 0 0") scheme(s1color)) ///
			(lowess prob1 pk if acc == 0, lpattern(solid) bw(.6) lw(med) lcol("0 0 0")) ///
			(lowess prob0 pk if acc == 1, lpattern(dash) bw(.6) lw(med) lcol("0 200 0")) ///
			(lowess prob1 pk if acc == 1, lpattern(solid) bw(.6) lw(med) lcol("0 200 0")), ///
			ylabel(.1 [.1] .8) xlabel(0[2]13) xscale(range(0, 13)) yscale(range(.1, .8))  ///
			ytitle("") xtitle("General Political Knowledge") ///
			text(.20 9.5 "Uncongenial", `format' placement(9)) ///
			text(.45 9 "Correct", `format' placement(9)) ///
			text(.70 9.2 "Congenial", `format' placement(9)) ///
			legend(cols(2) lab(1 "Control: P(Uncongenial)") lab(2 "Control: P(Correct or Uncongenial)") lab(3 "Acc. Incentive: P(Uncongenial)") lab(4 "Acc. Incentive: P(Correct or Uncongenial)") size(2.5))


	*Panel B: Predicted Probabilities (Hierarchical Multinomial Logit)
	estimates restore tab4
	gllapred p3_uncong, mu marginal outcome(1)
	gllapred p3_corr, mu marginal outcome(0)

	gen p3_cong = 1 - p3_corr - p3_uncong
	gen pred0 = p3_uncong
	gen pred1 = 1 - p3_cong

	twoway  (lowess pred0 pk if acc == 0, lpattern(dash) bw(.6) lw(med) lcol("0 0 0") scheme(s1color)) ///
			(lowess pred1 pk if acc == 0, lpattern(solid) bw(.6) lw(med) lcol("0 0 0")) ///
			(lowess pred0 pk if acc == 1, lpattern(dash) bw(.6) lw(med) lcol("0 200 0")) ///
			(lowess pred1 pk if acc == 1, lpattern(solid) bw(.6) lw(med) lcol("0 200 0")), ///
			ylabel(.1 [.1] .8) xlabel(0[2]13) xscale(range(0, 13)) yscale(range(.1, .8))  ///
			ytitle("") xtitle("General Political Knowledge") ///
			text(.20 9.5 "Uncongenial", `format' placement(9)) ///
			text(.45 9 "Correct", `format' placement(9)) ///
			text(.70 9.2 "Congenial", `format' placement(9)) ///
			legend(cols(2) lab(1 "Control: P(Uncongenial)") lab(2 "Control: P(Correct or Uncongenial)") lab(3 "Acc. Incentive: P(Uncongenial)") lab(4 "Acc. Incentive: P(Correct or Uncongenial)") size(2.5))

	
	*Panel C: Raw Data (Lowess Smoothed)
	gen bias = congenial - prob0
	twoway  (lowess bias pk if acc == 0, lpattern(solid) bw(1) lw(med) lcol("0 0 0") scheme(s1color)) ///
			(lowess bias pk if acc == 1, lpattern(solid) bw(1) lw(med) lcol("0 200 0")), ///
			ylabel(0 [.1] .4) xlabel(0[2]13) xscale(range(0, 13)) yscale(range(0, .25))  ///
			ytitle("") xtitle("General Political Knowledge") ///
			legend(cols(2) lab(1 "Control: P(Congenial) - P(Uncongenial)") lab(2 "Acc. Incentive: P(Congenial) - P(Uncongenial)") size(2.25))


	*Panel D: Predicted Level of Bias (Hierarchical Multinomial Logit)
	gen predbias = p3_cong - p3_uncong
	twoway  (lowess predbias pk if acc == 0, lpattern(solid) bw(.6) lw(med) lcol("0 0 0") scheme(s1color)) ///
			(lowess predbias pk if acc == 1, lpattern(solid) bw(.6) lw(med) lcol("0 200 0")), ///
			ylabel(0 [.1] .4) xlabel(0[2]13) xscale(range(0, 13)) yscale(range(0, .2))  ///
			ytitle("") xtitle("General Political Knowledge") ///
			legend(cols(2) lab(1 "Control: P(Congenial) - P(Uncongenial)") lab(2 "Acc. Incentive: P(Congenial) - P(Uncongenial)") size(2.25))


*** Main Text and Footnotes ***

use "psk_long_recode.dta", clear

*Percent correct by item
tab item tri_pp, row

*Percent of respondents reporting unemployment rate greater than 30%
gen flag1 = .
replace flag1 = 0 if item == "Unemployment 2008" & l_raw != . & l_raw <= 30
replace flag1 = 1 if item == "Unemployment 2008" & l_raw != . & l_raw > 30
tab flag1 /*9.97 percent*/

*Percent of respondents reporting uninsured rate greater than or equal to 50%
gen flag2 = .
replace flag2 = 0 if item == "Uninsured 2008" & l_raw != . & l_raw < 50
replace flag2 = 1 if item == "Uninsured 2008" & l_raw != . & l_raw >= 50
tab flag2 /*9.78 percent*/

*Percent of respondents reporting double-digit gas price
gen flag3 = .
replace flag3 = 0 if item == "Gas Price 2008" & l_raw != . & l_raw < 10
replace flag3 = 1 if item == "Gas Price 2008" & l_raw != . & l_raw >= 10
tab flag3 /*9.46 percent*/

*Percent congenial and uncongenial responses across all ten items in control condition
tab cong_33 if rd == 0 & r_bushref == 0

*Distribution of partisanship in each study's sample (percent Independent noted in text)
tab pid7 pollid, col

*Bias (i.e., % Congenial - % Uncongenial) in control condition among "consistent" partisans
proportion cong_33 if rd == 0 & r_bushref == 0 & consis == 1
mat prop = e(b)
mat bias = prop[1,3] - prop[1,1]
mat list bias /*14.6 percentage points*/

*Median screen completion time by condition in Study 2 (footnote 2)
tabstat t_disp if pollid == 2008, by(rd) statistics(median)


use "psk_wide_recode.dta", clear

*Percent of 2008 sample who are Republicans disapproving of Bush
gen rep_app = 1 if dem == 0 & approve == 0
tab rep_app if complete == 1, miss

*Percent of 2008 sample who are Democrats approving of Bush
gen dem_dis = 1 if dem == 1 & approve == 1
tab dem_dis if complete == 1, miss

*Percent of respondents selecting next question without answering in each study (footnote 5)
gen l1ref = .
replace l1ref = 1 if l1 == -1
replace l1ref = 0 if l1 != -1 & l1 != .
proportion l1ref
mat l1 = e(b)
mat l1 = l1[1, 2]

gen l2ref = .
replace l2ref = 1 if l2 == -1
replace l2ref = 0 if l2 != -1 & l2 != .
proportion l2ref
mat l2 = e(b)
mat l2 = l2[1, 2]

gen l3ref = .
replace l3ref = 1 if l3 == -1
replace l3ref = 0 if l3 != -1 & l3 != .
proportion l3ref
mat l3 = e(b)
mat l3 = l3[1, 2]

gen l4ref = .
replace l4ref = 1 if l4 == -1
replace l4ref = 0 if l4 != -1 & l4 != .
proportion l4ref
mat l4 = e(b)
mat l4 = l4[1, 2]

gen l5ref = .
replace l5ref = 1 if l5 == -1
replace l5ref = 0 if l5 != -1 & l5 != .
proportion l5ref
mat l5 = e(b)
mat l5 = l5[1, 2]

mat ref08 = (l1 + l2 + l3 + l4 + l5) * 100 / 5
mat list ref08 /*1.7 percent*/


gen l6ref = .
replace l6ref = 1 if l6 == -1
replace l6ref = 0 if l6 != -1 & l6 != .
proportion l6ref
mat l6 = e(b)
mat l6 = l6[1, 2]

gen l7ref = .
replace l7ref = 1 if l7 == -1
replace l7ref = 0 if l7 != -1 & l7 != .
proportion l7ref
mat l7 = e(b)
mat l7 = l7[1, 2]

gen l8ref = .
replace l8ref = 1 if l8 == -1
replace l8ref = 0 if l8 != -1 & l8 != .
proportion l8ref
mat l8 = e(b)
mat l8 = l8[1, 2]

gen l9ref = .
replace l9ref = 1 if l9 == -1
replace l9ref = 0 if l9 != -1 & l9 != .
proportion l9ref
mat l9 = e(b)
mat l9 = l9[1, 2]

gen l10ref = .
replace l10ref = 1 if l10 == -1
replace l10ref = 0 if l10 != -1 & l10 != .
proportion l10ref
mat l10 = e(b)
mat l10 = l10[1, 2]

mat ref = (l6 + l7 + l8 + l9 + l10) * 100 / 5
mat list ref /*3.2 percent*/



*** Appendix A ***
use "psk_wide_recode.dta", clear

*No Treatment Effects on Question Completion
tab rd if pollid == 2004 & dropout == 1
tab rd if pollid == 2008 & dropout == 1

*Figure A1. Distribution of General Political Knowledge by Partisanship
twoway histogram pk if dem == 0, percent lcolor(black) color(red) ///
|| histogram pk if dem == 1, percent lcolor(black) color(blue) by(dem, col(1) note("") legend(off) graphregion(fcolor(white))) ///
	xtitle("General Political Knowledge") xlabel(0[1]13) xscale(range(0, 13)) ///
	ytitle("Percent") ylabel(0[2]10, nogrid)



*** Appendix B ***
use "psk_long_recode.dta", clear

*Table B1
	*All (Table 2, Column 2a)
	gllamm cong_33 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon = app

	*Column 1
	gllamm cong_33 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0 & t_l <= 60, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app

	*Column 2
	gllamm cong_33 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0 & t_l <= 45, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app

	*Column 3
	gllamm cong_33 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0 & t_l <= 40, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app

	*Column 4
	gllamm cong_33 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0 & t_l <= 35, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app


*Allow Table 2, Column 1 coefficient to vary by nohispeed dummy
gllamm cong_33 pay nohispeed payxnohs seq1 seq2 seq3 seq4 seq5 seq6 seq7 seq8 seq9 if r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
estimates store tab2col1_hs
gllamm cong_33 pay seq1 seq2 seq3 seq4 seq5 seq6 seq7 seq8 seq9 if r_bushref == 0 & nohispeed != ., i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
lrtest tab2col1_hs /*Chi-sqared with 2 df is 3.44 (p = .18)*/


*Allow Table 2, Column 2a coefficients to vary by nohispeed dummy
gllamm cong_33 mon app nohispeed monxnohs appxnohs seq1 seq2 seq3 seq4 if r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
estimates store tab2col2a_nohs
gllamm cong_33 mon app seq1 seq2 seq3 seq4 if r_bushref == 0 & nohispeed != ., i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
lrtest tab2col2a_nohs /*Chi-sqared with 3 df is 2.46 (p = .48)*/



*Table B2

	*Column 1
	gllamm cong_po pay seq1 seq2 seq3 seq4 seq5 seq6 seq7 seq8 seq9 if r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)

	*Column 2
	gllamm cong_10 pay seq1 seq2 seq3 seq4 seq5 seq6 seq7 seq8 seq9 if r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)

	*Column 3
	gllamm cong_23 pay seq1 seq2 seq3 seq4 seq5 seq6 seq7 seq8 seq9 if r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)

	*Column 4
	gllamm cong_po mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app

	*Column 5
	gllamm cong_10 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app

	*Column 6
	gllamm cong_23 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app

	*Column 7
	gllamm cong_po mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0 & consis == 1, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app

	*Column 8
	gllamm cong_10 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0 & consis == 1, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app

	*Column 9
	gllamm cong_23 mon app seq1 seq2 seq3 seq4 if pollid == 2008 & r_bushref == 0 & consis == 1, i(caseid) link(ologit) adapt prior(gamma, scale(10000) shape(2) var)
	test mon=app


*Table B3
	ologit cong_33 pay if item == "Unemployment 2004"
	ologit cong_33 pay if item == "Estate Tax 2004"
	ologit cong_33 pay if item == "Debt 2004"
	ologit cong_33 pay if item == "Uninsured 2004"
	ologit cong_33 pay if item == "Poverty 2004"

	ologit cong_33 mon app if r_bushref == 0 & item == "Unemployment 2008"
	test mon = app
	ologit cong_33 mon app  if r_bushref == 0 & item == "Estate Tax 2008"
	test mon = app
	ologit cong_33 mon app  if r_bushref == 0 & item == "Debt 2008"
	test mon = app
	ologit cong_33 mon app  if r_bushref == 0 & item == "Uninsured 2008"
	test mon = app
	ologit cong_33 mon app  if r_bushref == 0 & item == "Gas Price 2008"
	test mon = app
