clear
set more off
cd "F:\Next Steps 1-8"

* 1. Set Macros ----
global stata_fld	"stata11"
global dta_fld		"Projects/NEET Book/Data"
global do_fld		"Projects/NEET Book/Code"
global write_fld	"Projects/NEET Book/Writing"
global gap			1

do "${do_fld}/create_programmes.do"

* 2. Data Cleaning ----
* Waves 4-7
use "${stata_fld}\lsype_main_activity_w4-7_nov2011_suppressed", clear
local m=8
local y=2006
foreach var of varlist W7Fin* {
	local m=`m'+1
	if `m'==13{
		local m=1
		local y=`y'+1
	}
	rename `var' Act`=ym(`y',`m')'
	}
reshape long Act, i(NSID) j(MY)
recode Act (min/0=.) (4=2) (5=3) (6=4)
qui labelbook
label drop `r(names)'

format MY %tm
by NSID (MY), sort: gen XX=(Act!=Act[_n-1])
by NSID (MY), sort: gen Spell=sum(XX)
by NSID Spell (MY), sort: gen Start_MY=MY[1]
keep NSID Spell Act Start_MY
duplicates drop
by NSID (Spell), sort: replace Spell=_n
gen Wave=7
gen IntDate_MY=ym(2010,5)
compress
tempfile Wave7
save "`Wave7'", replace


* Wave 8
use NSID W8INT* W8START* W8DACTIVITY ///
	using "${stata_fld}\ns8_2015_main_interview", clear
numlabel, add
prog_startdates W8INTMTH W8INTYEAR
rename Start_* IntDate_*
prog_startdates W8STARTM W8STARTY
gen Act=.
replace Act=1 if inlist(W8DACTIVITY,6,7)
replace Act=2 if inrange(W8DACTIVITY,1,4) | W8DACTIVITY==11
replace Act=3 if inlist(W8DACTIVITY,8,12)
replace Act=4 if inlist(W8DACTIVITY,5,9,10,13,14)
gen Spell=0
keep NSID Spell Act IntDate* St*
tempfile Wave8
save "`Wave8'", replace

use NSID W8HISTID W8HISTSTPM W8HISTSTPY W8DACTIVITYP ///
	using "${stata_fld}\ns8_2015_activity_history", clear
numlabel, add
rename W8HISTID Spell
gen Act=.
replace Act=1 if inlist(W8DACTIVITYP,6,7)
replace Act=2 if inrange(W8DACTIVITYP,1,4) | W8DACTIVITYP==11
replace Act=3 if inlist(W8DACTIVITYP,8,12)
replace Act=4 if inlist(W8DACTIVITYP,5,9,10,13,14)
prog_startdates W8HISTSTPM W8HISTSTPY
keep NSID Spell Act Start*
append using "`Wave8'"

foreach var of varlist IntDate*{
	by NSID (Spell), sort: replace `var'=`var'[1]
}
replace Start_M=. if missing(Start_Y)
by NSID (Spell), sort: replace Spell=_N-_n+1
gen Wave=8


* Append
append using "`Wave7'"
foreach stub in Start IntDate{
	replace `stub'_M=month(dofm(`stub'_MY)) if !missing(`stub'_MY) & missing(`stub'_M)
	replace `stub'_Y=year(dofm(`stub'_MY)) if !missing(`stub'_MY) & missing(`stub'_Y)
}
sort NSID Spell
format *MY %tm


* Clean
do "${do_fld}/clean_activity_histories.do"
replace Start_MY=ym(2006,9) if Start_MY<ym(2006,9)
preserve
	use NSID W8INT* using "${stata_fld}/ns8_2015_main_interview", clear
	gen MY=ym(W8INTYEAR,W8INTMTH) if W8INTYEAR>0 & W8INTMTH>0
	sum MY
	local min=r(min)
restore
replace End_MY=`min' if End_MY>`min'
drop if Start_MY>=End_MY


* Format and Save Data
drop *_M IntDate_MY
sort NSID Spell
order NSID Spell
prog_labels
label values Act Act
format Act %9.0g
numlabel, add
save "${dta_fld}/Activity Histories", replace

* Reshape to Wide Format
// use "${dta_fld}/Activity Histories", clear
by NSID (Spell), sort: gen XX=Start_MY[_n+1]-End_MY
by NSID (Spell), sort: gen N=(XX>0) 	// MEANS THAT MISSING GAPS CAN USE PROPER END MONTH.
expand End_MY-Start_MY+N
by NSID Spell, sort: gen MY=Start_MY+_n-1
keep NSID MY Act
numlabel, remove
reshape wide Act, i(NSID) j(MY)
local m=8
local y=2006
foreach var of varlist Act* {
	local m=`m'+1
	if `m'==13{
		local m=1
		local y=`y'+1
	}
	if `m'<10	local m 0`m'
	rename `var' Act`y'`m'
	}
merge 1:1 NSID using "${stata_fld}/wave_one_lsype_young_person_file_16_05_08", ///
	keepusing(W1FinWt) nogen
merge 1:1 NSID using "${stata_fld}/wave_four_lsype_young_person_september_2009", ///
	keepusing(W4Boost) nogen
drop W1FinWt W4Boost
save "${dta_fld}/Activity Sequences", replace
