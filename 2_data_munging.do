clear
set more off
cd "F:\Next Steps 1-8"

* 1. Set Macros ----
global stata_fld	"stata11"
global dta_fld		"Projects/NEET Book/Data"
global do_fld		"Projects/NEET Book/Code"
global write_fld	"Projects/NEET Book/Writing"
global gap			1


* 1. Collect Variables ----
use  NSID W8DFATHER W8DMOTHER W8DNSSEC5 W8DINCW W8DHANVQH ///
	W8DAUDIT W8DETHN6 W8DCHNO4 W8DCHNO11 ///
	using "${stata_fld}/ns8_2015_derived", clear
merge 1:1 NSID using "${stata_fld}/ns8_2015_main_interview", ///
	nogen keepusing(W8FINWT W8INTMTH W8INTYEAR W8BDAT? W8CMSEX W8DACTIVITY ///
	W8FLEXSHIFT W8ZEROH W8PERM W8QMAFI W8GENA W8ADULT0A W8ADULT0B W8ADULT0C W8GROW)
merge 1:1 NSID using "${stata_fld}/ns8_2015_self_completion", ///
	nogen keepusing(W8OSATIS W8SMOKING W8GHQ12_* W8LOCUS0*)
merge 1:1 NSID using "${stata_fld}/wave_five_lsype_young_person_march_2010", ///
	nogen keepusing(W5SexYP)
merge 1:1 NSID using "${stata_fld}/wave_four_lsype_young_person_september_2009", ///
	nogen keepusing(W4ethgrpYP W4Boost W4SexYP)
merge 1:1 NSID using "${stata_fld}/wave_one_lsype_family_background_file_21_05_08", ///
	nogen keepusing(W1hous12HH W1englangHH W1ed3?P W1ed4?P W1hiqualg?P ///
	W1agemum W1famtyp W1nssecfam W1ethgrpYP)
merge 1:1 NSID using "${stata_fld}/wave_one_lsype_young_person_file_16_05_08", ///
	nogen keepusing(SampPSU SampStratum W1FinWt Dob*YP W1sexYP W1disabYP W1risk)
merge 1:1 NSID using "${stata_fld}/wave_two_lsype_family_background_file_16_06_08", ///
	nogen keepusing(gor IMDRSCORE W2ethgrpYP)
merge 1:1 NSID using "${stata_fld}/wave_two_lsype_young_person_file_16_06_08", ///
	nogen keepusing(W2hea1cYP W2yschat1 W2risk W2ghq12scr W2Fat?YP W2SexYP W2FinWt W2disabYP)
merge 1:1 NSID using "${stata_fld}/wave_three_lsype_young_person_file_16_06_08", ///
	nogen keepusing(W3sexYP)
preserve
	use NSID W8NCHDOBM W8NCHDOBY W8DNCHRAGE W8NCHREL ///
		using "${stata_fld}/ns8_2015_children.dta", clear 
	merge m:1 NSID using "${stata_fld}/ns8_2015_main_interview", ///
		nogen keepusing(W8BDAT? W8INT*)	
	keep if W8NCHREL==1
	gen Birth_MY=ym(W8BDATY,W8BDATM)
	gen IntDate_MY=ym(W8INTYEAR,W8INTMTH) if W8INTYEAR>0 & W8INTMTH>0
	gen Child_MY=ym(W8NCHDOBY,W8NCHDOBM) if W8NCHDOBY>0 & W8NCHDOBY>0
	replace Child_MY=IntDate_MY-(W8DNCHRAGE*12)-6 if missing(Child_MY) & W8DNCHRAGE>=0

	gen FirstChild=(Child_MY-Birth_MY)/12
	by NSID (FirstChild), sort: replace FirstChild=.m if FirstChild[_N]==.
	by NSID (FirstChild), sort: keep if _n==1
	keep NSID FirstChild
	tempfile Temp
	save "`Temp'", replace
restore
merge 1:1 NSID using "`Temp'", nogen
preserve
	use "${dta_fld}/Activity Sequences", clear
	egen Months_Miss=rowmiss(Act*)
	egen Months_Edu = rcount(Act*), cond(@ == 1) 
	egen Months_Emp = rcount(Act*), cond(@ == 2) 
	egen Months_VET = rcount(Act*), cond(@ == 3) 
	egen Months_NEET = rcount(Act*), cond(@ == 4) 
	keep NSID Months*
	save "`Temp'", replace
restore
merge 1:1 NSID using "`Temp'", nogen
compress
save "${dta_fld}/Collected Variables", replace


* 2. Clean Data ----
use "${dta_fld}/Collected Variables", clear	
numlabel, add
ds NSID FirstChild Months_*, not
rename `r(varlist)', lower

* Cross-Wave
foreach i of numlist 2 8{
	gen Survey_Weight_W`i'=w`i'finwt
	}
drop w*finwt

foreach stub in Edu Emp VET NEET {
	gen Any_`stub'=(Months_`stub'>0)
	replace Any_`stub'=. if Months_`stub'==0 & Months_Miss>0
	replace Months_`stub'=. if Months_Miss>0
}
label define Binary 0 "No" 1 "Yes"
label values Any_* Binary

* Fixed Characteristics
egen Female=rowmax(w?sexyp)
replace Female = w8cmsex if !inlist(Female,1,2)
replace Female = cond(Female>0, Female - 1,.)

egen Ethnicity=rowmax(w?ethgrpyp)
gen NonWhite = (Ethnicity!=1) if !missing(Ethnicity) & Ethnicity>0

gen Birth_MY=ym(dobyearyp, dobmonthyp)
replace Birth_MY=ym(w8bdaty, w8bdatm) if missing(Birth_MY)
gen Survey_Boost=(w4boost==1)

label values Female NonWhite Binary

* Wave 8 
ds w8*
recode w8ghq* w8locus0* w8int* w8dfather w8dmother w8adult* (min/-1=.)

gen GHQ_W8_Likert=0
foreach var of varlist w8ghq* {
	replace GHQ_W8_Likert=GHQ_W8_Likert+`var'-1
	}

forval i = 1/4{
	local j: word `i' of `c(alpha)'
	gen LOC_W8_Item_`i' = w8locus0`j' - 1 if inrange(w8locus0`j', 1, 4)
}
replace LOC_W8_Item_3 = 3 - LOC_W8_Item_3

gen LifeSat_W8=5-w8osatis if inrange(w8osatis,1,5)

gen Child_W8 = (w8dchno4!=0 | w8dchno11!=0) if !missing(w8dchno4,w8dchno11)
replace FirstChild=.i if Child_W8==0
replace FirstChild=.m if Child_W8==1 & missing(FirstChild)
gen FirstChildxFemale=FirstChild*Female if Female==1
gen ChildxFemale_W8=Child_W8*Female

gen AUDIT_W8 = w8daudit if inrange(w8daudit,0,12)

gen Education_W8 = 6 - w8dhanvqh if inrange(w8dhanvqh, 1, 5)
replace Education_W8 = 6 if inlist(w8dhanvqh, 95, 96)
label define Education 1 "NVQ 5" 2 "NVQ 4" 3 "NVQ 3" 4 "NVQ 2" 5 "NVQ 1" 6 "No/Other Qual"
label values Education_W8 Education

gen Status_W8 = 1 if inrange(w8dactivity, 1, 4) | inlist(w8dactivity, 8, 11, 12)
replace Status_W8 = 2 if inrange(w8dactivity, 5, 6)
replace Status_W8 = 3 if inlist(w8dactivity, 9, 10, 13, 14)
replace Status_W8 = 4 if w8dactivity==5
label define Status_W8 1 "Employed" 2 "Education" 3 "Inactive" 4 "Unemployed"
label values Status_W8 Status_W8

gen LogPay_W8=ln(w8grow+0.01) 
replace LogPay_W8=. if Status_W8!=1 

gen ShiftWork_W8=(w8flexshift==1) if inrange(w8flexshift,1,2) & Status_W8==1

gen ZeroHours_W8=(w8zeroh==1) if inrange(w8zeroh,1,2) & Status_W8==1
gen Temp_W8=(w8perm==2) if inrange(w8perm,1,2) & Status_W8==1
gen Precarious_W8=1 if inlist(1,ZeroHours_W8,Temp_W8)
replace Precarious_W8=0 if !inlist(., ZeroHours_W8, Temp_W8) & Precarious_W8!=1

gen FinDiff_W8=(inrange(w8qmafi,4,5)) if inrange(w8qmafi,1,5)

gen PoorHealth_W8=(inrange(w8gena,4,5)) if inrange(w8gena,1,5)

gen Adult_W8=w8adult0a+w8adult0b+w8adult0c

gen IntDate_W8=ym(w8intyear,w8intmth)

label values 	ChildxFemale Child_W8 ShiftWork_W8 ///
				Precarious_W8 FinDiff_W8 PoorHealth_W8 Binary

				
* Wave 2
gen SchoolAtt_W2=w2yschat1 if inrange(w2yschat1,0,48)

local Int_LOC w2fat1yp w2fat5yp w2fat8yp
local Ext_LOC w2fat2yp w2fat4yp w2fat7yp
foreach type in Int Ext{
	local i = 0
	foreach var of local `type'_LOC{
		local i = `i' + 1
		if "`type'"=="Int"{
			gen Int_LOC_W2_Item`i' = 5 - `var' if inrange(`var', 1, 2)
			replace Int_LOC_W2_Item`i' = 2 if `var'==-1
			replace Int_LOC_W2_Item`i' = 4 - `var' if inrange(`var', 3, 4)
		}	
		else{
			gen Ext_LOC_W2_Item`i' = `var' if inrange(`var', 3, 4)
			replace Ext_LOC_W2_Item`i' = 2 if `var'==-1
			replace Ext_LOC_W2_Item`i' = `var' - 1 if inrange(`var', 1, 2)
		} 		
	}
}

gen GenHealth_W2 = w2hea1cyp - 2 if inrange(w2hea1cyp,3,5)
replace GenHealth_W2 = 3 if w2hea1cyp == 6
label define GenHealth 1 "Very Good" 2 "Fairly Good" 3 "Not Good"
label values GenHealth* GenHealth

gen GHQ_W2_Caseness=w2ghq12scr if inrange(w2ghq12scr, 0, 12)

gen Disabled_W2 = (inlist(w2disabyp,1,2)) if inrange(w2disabyp, 1, 3)
label values Disabled_W2 Binary

gen IMD_W2=imdrscore if inrange(imdrscore,0,100)

* Wave 1
gen Risk_W1 = w1risk if inrange(w1risk,0,8)

gen NSSEC8_W1 = w1nssecfam if inrange(w1nssecfam,1,8)
gen NSSEC3_W1 = 1 if inrange(NSSEC8_W1,1,2)
replace NSSEC3_W1 = 2 if inrange(NSSEC8_W1,3,4)
replace NSSEC3_W1 = 3 if inrange(NSSEC8_W1,5,7)
replace NSSEC3_W1 = 4 if inlist(NSSEC8_W1,8)
label define NSSEC3 1 "Higher" 2 "Intermediate" 3 "Routine/Manual" 4 "Long-Term Unemployed"
label values NSSEC3_W1 NSSEC3

gen HHType_W1= inrange(w1famtyp,3,5) if inrange(w1famtyp, 1, 5)	
label define HHType 0 "Couple" 1 "Lone/No Parent"
label values HHType_W1 HHType

gen ParentEduc5_W1 = w1hiqualgmp ///
    if inrange(w1hiqualgmp, 1, 7) & w1hiqualgsp == -98
replace ParentEduc5_W1 = min(w1hiqualgmp, w1hiqualgsp) ///
    if inrange(w1hiqualgmp, 1, 7) & inrange(w1hiqualgsp, 1, 7)
replace ParentEduc5_W1 = 5 if inrange(ParentEduc5_W1, 5, 7)
label define ParentEduc5_W1 1 "Degree" 2 "Other HE" 3 "A-Level" 4 "GCSE A-C" 5 "Other/None"
label values ParentEduc5_W1 ParentEduc5_W1

recode w1ed*sp (-98=4)
recode w1ed* (min/-1=.)
gen XX=min(w1ed3mp, w1ed4mp) 
gen YY=min(w1ed3sp, w1ed4sp)
gen GParentUni_W1=min(XX,YY)
replace GParentUni_W1=2 if GParentUni_W1==3
replace GParentUni_W1=. if GParentUni_W1==4
replace GParentUni_W1=0 if GParentUni_W1==2

gen ForLangHH_W1=(w1englanghh==3) if inrange(w1englanghh,1,4)

label values GParentUni_W1 ForLangHH_W1 Binary

* 3. Format Dataset

keep 	NSID Survey_Weight_W8 Survey_Weight_W2 ///
		IMD_W2 NSSEC3_W1 HHType_W1 ParentEduc5_W1 GParentUni_W1  ///
		Disabled_W2 GHQ_W2_Caseness GenHealth_W2 ///
		Risk_W1 SchoolAtt_W2 *LOC_W2_Item* ///
		Status_W8 FinDiff_W8 Precarious_W8 ShiftWork_W8 LogPay_W8 ///
		AUDIT_W8 PoorHealth_W8  LifeSat_W8 GHQ_W8_Likert ///
		Adult_W8 LOC_W8_Item* ///
		Female Education_W8 NonWhite ForLangHH_W1  ///
		Child*W8 FirstChild* ///
		Any_VET Any_NEET Months_NEET
order NSID Survey_Weight_W*
// format *MY %tm
save "${dta_fld}/Dataset", replace
