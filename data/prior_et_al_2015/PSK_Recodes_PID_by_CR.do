/*
"You Cannot Be Serious" Recode File
Markus Prior, Gaurav Sood, and Kabir Khanna
Updated: July 29, 2015
*/

*Set working directory below
cd ""

*Studies 1 and 2: Wide Format
use "psk_wide.dta", clear

*Completed Study 2
gen complete = .
replace complete = 0 if pollid == 2008 & l5 == .
replace complete = 1 if pollid == 2008 & l5 != .
label def complete 0 "No" 1 "Yes"
label val complete complete
label var complete "Completed Study 2"

*Dropped Out After Randomization (Study 2)
replace dropout = 0 if pollid == 2008
replace dropout = 1 if complete == 0 & r_PIDloc == 0 & h1c != .
replace dropout = 1 if complete == 0 & r_PIDloc == 1 & pres_app != .

*Keeping standard PID7 variable
gen pid_7_standard=pid7

*Dummy variable contrasting Republicans (0) and Democrats (1)
recode pid7 (1/3 = 0) (4 = .) (5/7 = 1), gen(dem)
label def dem 0 "Republicans" 1 "Democrats"
label val dem dem

*Dichotomize Presidential Approval
recode pres_app (1/2 = 1) (3/4 = 0), gen(approve)
label def approve 1 "Approve" 0 "Disapprove"
label val approve approve

*Code "consistent" partisans, i.e. all partisans in 2004, and Democrats disapproving of Bush and Republicans approving of Bush in 2008
gen consis = .
replace consis = 0 if pollid == 2008 & dem == 0 & approve == 0
replace consis = 1 if pollid == 2008 & dem == 0 & approve == 1
replace consis = 1 if pollid == 2008 & dem == 1 & approve == 0
replace consis = 0 if pollid == 2008 & dem == 1 & approve == 1
label var consis "Consistent Partisan Dummy (i.e., PID and Pres App match)"

*General Political Knowledge (code 'refused' and 'don't know' as 0)
recode e1 -1=0 1/2=0 3=1 4/5=0, gen(kne1)
recode e2 -1=0 1=1 2/5=0, gen(kne2)
recode e3 -1=0 1=0 2=1 3/5=0, gen(kne3)
recode e4 -1=0 1=1 2/5=0, gen(kne4)
recode e5 -1=0 1/2=0 3=1 4/5=0, gen(kne5)
recode e6 -1=0 1=0 2=1 3/5=0, gen(kne6)
recode e7 -1=0 1=1 2/5=0, gen(kne7)
recode e8 -1=0 1=1 2/5=0, gen(kne8)
recode e9_1 -1=0 1=0 2=1 3/5=0, gen(kne9_1)
recode e9_2 -1=0 1=1 2/5=0, gen(kne9_2)
recode e9_3 -1=0 1=1 2/5=0, gen(kne9_3)
recode e9_4 -1=0 1=1 2/5=0, gen(kne9_4)
recode kn_snmaj 1=.25 2=.50 3=.75 4=1, gen(kn_snmajr)

gen pk = kne1 + kne2 + kne3 + kne4 + kne5 + kne6 + kne7 + kne8 + kne9_1 + kne9_2 + kne9_3 + kne9_4 + kn_snmajr
label var pk "General Political Knowledge (0-13)"

*Create Dummy for No Hi-Speed Internet Connection
recode cu07serv -2 -1=. 2/6=0 1=1, gen(nohispeed)
replace nohispeed = 1 if ppnet == 0 & nohispeed == .
replace nohispeed = 1 if inet_sp == 0
replace nohispeed = 1 if inet_sp == 1
replace nohispeed = 0 if inet_sp == 2
label var nohispeed "0: High-speed connection, 1: No high-speed connection"

save "psk_wide_recode.dta", replace

********************************************************************************

*Studies 1 and 2: Long Format
use "psk_wide_recode.dta", clear
reshape long l t_l, i(caseid)

*Reshape command creates 10 rows per respondents, so remove extra rows (i.e., Study 1 rows for Study 2 respondents and Study 2 rows for Study 1 respondents)
keep if (pollid == 2008 & _j <= 5) | (pollid == 2004 & _j >= 6)

gen l_raw = l
label var l_raw "Economic Knowledge Item Response (Raw)"
label var l "Economic Knowledge Item Response (Clean)"
label var t_l "Item Completion Time"

gen item = ""
replace item = "Debt 2008" if _j == 1
replace item = "Unemployment 2008" if _j == 2
replace item = "Uninsured 2008" if _j == 3
replace item = "Estate Tax 2008" if _j == 4
replace item = "Gas Price 2008" if _j == 5
replace item = "Unemployment 2004" if _j == 6
replace item = "Estate Tax 2004" if _j == 7
replace item = "Debt 2004" if _j == 8
replace item = "Uninsured 2004" if _j == 9
replace item = "Poverty 2004" if _j == 10
label var item "Economic Knowledge Item"

*Set responses to missing if item not asked or refused
replace l = . if l < 0

*Correct gas price estimates greater than $100/gallon
replace l = l / 100  if item == "Gas Price 2008" & l >= 100

*Omit uninsured rate estimates greater than 100 percent (two respondents in 2004)
replace l = .  if (item == "Uninsured 2004" | item == "Uninsured 2008") & l > 100


*Trichotomize responses such that 33 percent of responses are correct
label define tri 0 "Under" 1 "Correct" 2 "Over"

gen tri_33 = .
label val tri_33 tri
label var tri_33 "Trichotomized Response (33% Window)"

replace tri_33 = 0 if item == "Unemployment 2004" & l == 1
replace tri_33 = 1 if item == "Unemployment 2004" & l == 2
replace tri_33 = 2 if item == "Unemployment 2004" & l >= 3

replace tri_33 = 0 if item == "Debt 2004" & l <= 4
replace tri_33 = 1 if item == "Debt 2004" & l == 5
replace tri_33 = 2 if item == "Debt 2004" & l >= 6

replace tri_33 = 1 if item == "Estate Tax 2004" & l == 1
replace tri_33 = 2 if item == "Estate Tax 2004" & l >= 2

replace tri_33 = 0 if item == "Debt 2008" & l <= 4
replace tri_33 = 1 if item == "Debt 2008" & l == 5
replace tri_33 = 2 if item == "Debt 2008" & l >= 6

replace tri_33 = 1 if item == "Estate Tax 2008" & l == 1
replace tri_33 = 2 if item == "Estate Tax 2008" & l >= 2


replace tri_33 = 0 if item == "Uninsured 2004" & l < 2
replace tri_33 = 1 if item == "Uninsured 2004" & l >= 2 & l <= 29.2
replace tri_33 = 2 if item == "Uninsured 2004" & l > 29.2

replace tri_33 = 0 if item == "Poverty 2004" & l < 4.5
replace tri_33 = 1 if item == "Poverty 2004" & l >= 4.5 & l <= 20.5
replace tri_33 = 2 if item == "Poverty 2004" & l > 20.5

replace tri_33 = 0 if item == "Unemployment 2008" & l < 4.2
replace tri_33 = 1 if item == "Unemployment 2008" & l >= 4.2 & l <= 5.4
replace tri_33 = 2 if item == "Unemployment 2008" & l > 5.4

replace tri_33 = 0 if item == "Uninsured 2008" & l < 12.6
replace tri_33 = 1 if item == "Uninsured 2008" & l >= 12.6 & l <= 19.0
replace tri_33 = 2 if item == "Uninsured 2008" & l > 19.0

replace tri_33 = 0 if item == "Gas Price 2008" & l < 3.22
replace tri_33 = 1 if item == "Gas Price 2008" & l >= 3.22 & l <= 3.32
replace tri_33 = 2 if item == "Gas Price 2008" & l > 3.32

replace tri_33 = . if l == .

*Trichotomize responses such that 23 percent of responses are correct (robustness check)
gen tri_23 = .
label val tri_23 tri
label var tri_23 "Trichotomized Response (23% Window)"

replace tri_23 = tri_33 if (item == "Unemployment 2004" | item == "Debt 2004" | item == "Estate Tax 2004" | item == "Debt 2008" | item == "Estate Tax 2008")

replace tri_23 = 0 if item == "Uninsured 2004" & l < 6.6
replace tri_23 = 1 if item == "Uninsured 2004" & l >= 6.6 & l <= 24.6
replace tri_23 = 2 if item == "Uninsured 2004" & l > 24.6

replace tri_23 = 0 if item == "Poverty 2004" & l < 6.5
replace tri_23 = 1 if item == "Poverty 2004" & l >= 6.5 & l <= 18.5
replace tri_23 = 2 if item == "Poverty 2004" & l > 18.5

replace tri_23 = 0 if item == "Unemployment 2008" & l < 4.6
replace tri_23 = 1 if item == "Unemployment 2008" & l >= 4.6 & l <= 5.0
replace tri_23 = 2 if item == "Unemployment 2008" & l > 5.0

replace tri_23 = 0 if item == "Uninsured 2008" & l < 13.59
replace tri_23 = 1 if item == "Uninsured 2008" & l >= 13.59 & l <= 17
replace tri_23 = 2 if item == "Uninsured 2008" & l > 17

replace tri_23 = 0 if item == "Gas Price 2008" & l < 3.25
replace tri_23 = 1 if item == "Gas Price 2008" & l >= 3.25 & l <= 3.29
replace tri_23 = 2 if item == "Gas Price 2008" & l > 3.29

replace tri_23 = . if l == .

*Trichotomize responses such that answers within 10 percent of correct answer are correct (robustness check)
gen tri_10 = .
label val tri_10 tri
label var tri_10 "Trichotomized Response (+/- 10% Window)"

replace tri_10 = tri_33 if (item == "Unemployment 2004" | item == "Debt 2004" | item == "Estate Tax 2004" | item == "Debt 2008" | item == "Estate Tax 2008")

replace tri_10 = 0 if item == "Uninsured 2004" & l < 14.04
replace tri_10 = 1 if item == "Uninsured 2004" & l >= 14.04 & l <= 17.16
replace tri_10 = 2 if item == "Uninsured 2004" & l > 17.16

replace tri_10 = 0 if item == "Poverty 2004" & l < 11.25
replace tri_10 = 1 if item == "Poverty 2004" & l >= 11.25 & l <= 13.75
replace tri_10 = 2 if item == "Poverty 2004" & l > 13.75

replace tri_10 = 0 if item == "Unemployment 2008" & l < 4.32
replace tri_10 = 1 if item == "Unemployment 2008" & l >= 4.32 & l <= 5.28
replace tri_10 = 2 if item == "Unemployment 2008" & l > 5.28

replace tri_10 = 0 if item == "Uninsured 2008" & l < 14.22
replace tri_10 = 1 if item == "Uninsured 2008" & l >= 14.22 & l <= 17.38
replace tri_10 = 2 if item == "Uninsured 2008" & l > 17.38

replace tri_10 = 0 if item == "Gas Price 2008" & l < 2.943
replace tri_10 = 1 if item == "Gas Price 2008" & l >= 2.943 & l <= 3.597
replace tri_10 = 2 if item == "Gas Price 2008" & l > 3.597

replace tri_10 = . if l == .

*Trichotomize responses such that answers that received bonus payments are correct (robustness check)
gen tri_po = .
label val tri_po tri
label var tri_po "Trichotomized Response (Payout Window)"

replace tri_po = tri_33 if (item == "Unemployment 2004" | item == "Debt 2004" | item == "Estate Tax 2004" | item == "Debt 2008" | item == "Estate Tax 2008")

replace tri_po = 0 if item == "Uninsured 2004" & l < 12.6
replace tri_po = 1 if item == "Uninsured 2004" & l >= 12.6 & l <= 18.6
replace tri_po = 2 if item == "Uninsured 2004" & l > 18.6

replace tri_po = 0 if item == "Poverty 2004" & l < 9.5
replace tri_po = 1 if item == "Poverty 2004" & l >= 9.5 & l <= 15.5
replace tri_po = 2 if item == "Poverty 2004" & l > 15.5

replace tri_po = 0 if item == "Unemployment 2008" & l < 3.8
replace tri_po = 1 if item == "Unemployment 2008" & l >= 3.8 & l <= 5.8
replace tri_po = 2 if item == "Unemployment 2008" & l > 5.8

replace tri_po = 0 if item == "Uninsured 2008" & l < 14.8
replace tri_po = 1 if item == "Uninsured 2008" & l >= 14.8 & l <= 16.8
replace tri_po = 2 if item == "Uninsured 2008" & l > 16.8

replace tri_po = 0 if item == "Gas Price 2008" & l < 3.17
replace tri_po = 1 if item == "Gas Price 2008" & l >= 3.17 & l <= 3.37
replace tri_po = 2 if item == "Gas Price 2008" & l > 3.37

replace tri_po = . if l == .


*Trichotomize responses such that answers within 1 percentage point are correct - necessary to include?
gen tri_pp = .
label val tri_pp tri
label var tri_pp "Trichotomized Response (1 Pct Pt Window)"

replace tri_pp = tri_33 if (item == "Unemployment 2004" | item == "Debt 2004" | item == "Estate Tax 2004" | item == "Debt 2008" | item == "Estate Tax 2008")
replace tri_pp = tri_po if item == "Uninsured 2008"

replace tri_pp = 0 if item == "Uninsured 2004" & l < 14.6
replace tri_pp = 1 if item == "Uninsured 2004" & l >= 14.6 & l <= 16.6
replace tri_pp = 2 if item == "Uninsured 2004" & l > 16.6

replace tri_pp = 0 if item == "Poverty 2004" & l < 11.5
replace tri_pp = 1 if item == "Poverty 2004" & l >= 11.5 & l <= 13.5
replace tri_pp = 2 if item == "Poverty 2004" & l > 13.5

replace tri_pp = 0 if item == "Unemployment 2008" & l < 3.8
replace tri_pp = 1 if item == "Unemployment 2008" & l >= 3.8 & l <= 5.8
replace tri_pp = 2 if item == "Unemployment 2008" & l > 5.8

replace tri_pp = 0 if item == "Gas Price 2008" & l < 3.17
replace tri_pp = 1 if item == "Gas Price 2008" & l >= 3.17 & l <= 3.27
replace tri_pp = 2 if item == "Gas Price 2008" & l > 3.27

replace tri_pp = . if l == .


*Recode trichotomized responses as correct, congenial, or uncongenial, depending on respondent partisanship
gen cong_33 = .
gen cong_23 = .
gen cong_10 = .
gen cong_po = .

label var cong_33 "Recoded DV (33% Window)"
label var cong_23 "Recoded DV (23% Window)"
label var cong_10 "Recoded DV (+/- 10% Window)"
label var cong_po "Recoded DV (Payout Window)"

label define congeniality -1 "Uncongenial" 0 "Correct" 1 "Congenial"
label val cong_33 cong_23 cong_10 cong_po congeniality

*33 Percent Window (Default)
replace cong_33 =  1 if dem == 0 & tri_33 == 0 /*Underestimates are congenial for Republicans*/
replace cong_33 =  0 if dem == 0 & tri_33 == 1
replace cong_33 = -1 if dem == 0 & tri_33 == 2 /*Overestimates are uncongenial for Republicans*/

replace cong_33 = -1 if dem == 1 & tri_33 == 0 /*Underestimates are uncongenial for Democrats*/
replace cong_33 =  0 if dem == 1 & tri_33 == 1
replace cong_33 =  1 if dem == 1 & tri_33 == 2 /*Overestimates are congenial for Democrats*/

*Estate tax items coded differently (see footnote 7)
replace cong_33 =  0 if dem == 0 & tri_33 == 1 & (item == "Estate Tax 2004" | item == "Estate Tax 2008")
replace cong_33 =  1 if dem == 0 & tri_33 == 2 & (item == "Estate Tax 2004" | item == "Estate Tax 2008")
replace cong_33 =  0 if dem == 1 & tri_33 == 1 & (item == "Estate Tax 2004" | item == "Estate Tax 2008")
replace cong_33 = -1 if dem == 1 & tri_33 == 2 & (item == "Estate Tax 2004" | item == "Estate Tax 2008")


*Reverse code so that gllamm makes Congenial the reference category in multinomial specification*/
gen cong_33r = -cong_33
label var cong_33r "Reverse Coded DV (33% Window)"

*23 Percent Window
replace cong_23 =  1 if dem == 0 & tri_23 == 0 /*Underestimates are congenial for Republicans*/
replace cong_23 =  0 if dem == 0 & tri_23 == 1
replace cong_23 = -1 if dem == 0 & tri_23 == 2 /*Overestimates are uncongenial for Republicans*/

replace cong_23 = -1 if dem == 1 & tri_23 == 0 /*Underestimates are uncongenial for Democrats*/
replace cong_23 =  0 if dem == 1 & tri_23 == 1
replace cong_23 =  1 if dem == 1 & tri_23 == 2 /*Overestimates are congenial for Democrats*/

replace cong_23 = cong_33 if item == "Estate Tax 2004" | item == "Estate Tax 2008"

*Plus/Minus 10 Percent Window
replace cong_10 =  1 if dem == 0 & tri_10 == 0 /*Underestimates are congenial for Republicans*/
replace cong_10 =  0 if dem == 0 & tri_10 == 1
replace cong_10 = -1 if dem == 0 & tri_10 == 2 /*Overestimates are uncongenial for Republicans*/

replace cong_10 = -1 if dem == 1 & tri_10 == 0 /*Underestimates are uncongenial for Democrats*/
replace cong_10 =  0 if dem == 1 & tri_10 == 1
replace cong_10 =  1 if dem == 1 & tri_10 == 2 /*Overestimates are congenial for Democrats*/

replace cong_10 = cong_33 if item == "Estate Tax 2004" | item == "Estate Tax 2008"

*Payout Window
replace cong_po =  1 if dem == 0 & tri_po == 0 /*Underestimates are congenial for Republicans*/
replace cong_po =  0 if dem == 0 & tri_po == 1
replace cong_po = -1 if dem == 0 & tri_po == 2 /*Overestimates are uncongenial for Republicans*/

replace cong_po = -1 if dem == 1 & tri_po == 0 /*Underestimates are uncongenial for Democrats*/
replace cong_po =  0 if dem == 1 & tri_po == 1
replace cong_po =  1 if dem == 1 & tri_po == 2 /*Overestimates are congenial for Democrats*/

replace cong_po = cong_33 if item == "Estate Tax 2004" | item == "Estate Tax 2008"


*Dummy variables indicating congenial, correct, and uncongenial responses (default window)
recode cong_33 -1 0 = 0 1 = 1, gen(congenial)
recode cong_33 -1 1 = 0 0 = 1, gen(correct)
recode cong_33 	0 1 = 0 -1 = 1, gen(uncongenial)


*Dummy variable indicating monetary incentives (1) vs. control (0) in both studies
gen pay = .
replace pay = 0 if rd == 0
replace pay = 1 if rd == 2

*Dummy variable indicating textual appeal and monetary incentive treatments in Study 2
gen app = .
replace app = 0 if pollid == 2008 & (rd == 0 | rd == 2)
replace app = 1 if pollid == 2008 & rd == 1

gen mon = .
replace mon = 0 if pollid == 2008 & (rd == 0 | rd == 1)
replace mon = 1 if pollid == 2008 & rd == 2

*Dummy variable indicating any accuracy incentive (1) vs. control (0) in Study 2
gen acc = .
replace acc = 0 if pollid == 2008 & rd == 0
replace acc = 1 if pollid == 2008 & (rd == 1 | rd == 2)

/*Item Fixed Effects*/
gen seq = mod(_j-1,10) + 1

gen seq1=1 if seq==1
replace seq1=0 if seq~=1
				
gen seq2=1 if seq==2
replace seq2=0 if seq~=2
				
gen seq3=1 if seq==3
replace seq3=0 if seq~=3
				
gen seq4=1 if seq==4
replace seq4=0 if seq~=4
	
gen seq5=1 if seq==5
replace seq5=0 if seq~=5
	
gen seq6=1 if seq==6
replace seq6=0 if seq~=6
	
gen seq7=1 if seq==7
replace seq7=0 if seq~=7
				
gen seq8=1 if seq==8
replace seq8=0 if seq~=8
				
gen seq9=1 if seq==9
replace seq9=0 if seq~=9

drop _j

*Interactions between Bush reference condition and treatments
gen monxbush = mon * r_bushref
gen appxbush = app * r_bushref
gen accxbush = acc * r_bushref

*Interactions between high-speed Internet and treatments
gen payxnohs = pay * nohispeed
gen monxnohs = mon * nohispeed
gen appxnohs = app * nohispeed

*Interaction between general political knowledge and treatments
gen monxpk = mon * pk
gen appxpk = app * pk
gen accxpk = acc * pk

*Interaction between general political knowledge and bush reference condition
gen bushxpk = r_bushref * pk

*Three-way interaction between accuracy incentives, bush reference, and general political knowledge 
gen accxbushxpk = acc * r_bushref * pk

save "psk_long_recode.dta", replace
