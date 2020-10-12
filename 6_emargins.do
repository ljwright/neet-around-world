// https://stats.idre.ucla.edu/stata/faq/how-can-i-get-margins-and-marginsplot-with-multiply-imputed-data/

clear
set more off
cd "\\ad.ucl.ac.uk\homer\rmjllwr\Documents\PhD\Empirical Work\NEET Book"
// cd "D:\Next Steps 1-8\Projects\NEET Book"

* 1. Set Up ----
// use "Data/mice_long.dta", clear
// compress
// save "Data/mice_long.dta", replace
use "Data/mice_long.dta", clear
capture log close
log using "Notes/regressions.log", replace

* Recode Binary Variables
foreach var of varlist Any_NEET Employed_W8 FinDiff_W8 PoorHealth_W8 ShiftWork_W8 Precarious_W8{
	recode `var' (1 = 0) (2 = 1) 
	local lbl_1: label `var' 1
	local lbl_2: label `var' 2
	label define `var' 0 "`lbl_1'" 1 "`lbl_2'", replace
}

gen FemalexChild = 0 if Female == 1 & Child_W8 == 1
replace FemalexChild = 1 if Female == 2 & Child_W8 == 1
replace FemalexChild = 2 if Female == 1 & Child_W8 == 2
replace FemalexChild = 3 if Female == 2 & Child_W8 == 2
label define FemalexChild 	0 "Male, no child" 1 "Female, no child" ///
							2 "Male, with child" 3 "Female, with child"
label values FemalexChild FemalexChild
gen FemalexFirstChild = cond(Female==2, FirstChild, 0)


* Define Macros
global covars	i.FemalexChild c.FirstChild c.FemalexFirstChild /// i.Female##i.Child_W8##c.FirstChild ///
				/// 1.Female#0.Child_W8 0.Female#1.Child_W8 1.Female#1.Child_W8 ///
				/// 0.Female#1.Child_W8#c.FirstChild 1.Female#1.Child_W8#c.FirstChild ///
				i.Any_VET	///
				c.GHQ_W2_Caseness i.GenHealth_W2 i.Disabled_W2	///
				c.Risk_W1 c.SchoolAtt_W2 i.Education_W8 c.LOC_Factor_W2	///
				i.HHType_W1 i.ForLangHH_W1 c.IMD_W2 /// 
				i.ParentEduc5_W1 i.GParentUni_W1 i.NonWhite i.NSSEC3_W1

* Create Programs
capture program drop prog_regsave
program define prog_regsave
	args outcome estimator Any_NEET Months_NEET cluster4 type file
	
	if "`file'"!="replace" local file append	
	regsave using "Data/regsave_ame.dta", ///
		addlabel(outcome, `outcome', estimator, `estimator', ///
		Any_NEET, `Any_NEET', Months_NEET, `Months_NEET', ///
		cluster4, `cluster4', type, `type') pval ci `file'
end

capture program drop prog_observed
program define prog_observed
	args var
	capture drop observed
	by NSID (imp), sort: gen observed = !missing(`var'[1])
end

capture program drop emargins
program emargins, eclass properties(mi)
  version 15
  args mod_string
  svy: `mod_string'
  margins, dydx(*) post
end


* 2. Run Regressions ----
mi import flong, id(NSID) m(imp) clear
mi svyset [pweight=Survey_Weight_W8]
mi xtset, clear


* Predicting Any/Months NEET & Cluster Membership
prog_observed Any_NEET
mi estimate, cmdok: emargins ///
	"logit Any_NEET $covars if observed==1"
prog_regsave Any_NEET logit FALSE FALSE FALSE mi replace

prog_observed Months_NEET	
mi estimate, cmdok: emargins ///
	"zinb Months_NEET $covars if observed==1, inflate(${covars})"
prog_regsave Months_NEET zinb FALSE FALSE FALSE mi

prog_observed cluster4	
local covars: subinstr global covars "i.Education_W8" ""
mi estimate, cmdok: emargins ///
	"mlogit cluster4 `covars' if observed == 1 & Any_NEET == 1, rrr baseoutcome(2)"
prog_regsave cluster4 mlogit FALSE FALSE FALSE mi


* Predicting Wave 8 Outcomes
	* Q1: Does Cluster Membership Add Any Information Over Months NEET?
		* Three models: + Months; + Cluster; + Months + Cluster
			* Qualitatively and with Likelihood Ratio Test			
global reg			GHQ_W8_Likert LifeSat_W8 LOC_Factor_W8 AUDIT_W8 Adult_W8
global tobit 		AUDIT_W8 Adult_W8
global logit		Employed_W8 FinDiff_W8 PoorHealth_W8
global heckman		LogPay_W8 
global heckprobit 	ShiftWork_W8 Precarious_W8

foreach glb in reg logit {
	foreach var of global `glb' {		
		prog_observed `var'	
		
		local cond
		if "`glb'"=="tobit"{
			sum `var'
			local cond ll(`r(min)') ul(`r(max)') 
			}
		
		mi estimate, cmdok: emargins ///
			"`glb' `var' i.Any_NEET $covars if observed==1, `cond'"
		prog_regsave `var' `glb' TRUE FALSE FALSE mi	
		
		mi estimate, cmdok: emargins ///
			"`glb' `var' i.cluster4 $covars if observed==1, `cond'"
		prog_regsave `var' `glb' FALSE FALSE TRUE mi
			
		mi estimate, cmdok: emargins ///
			"`glb' `var' i.Any_NEET c.Months_NEET $covars if observed==1, `cond'"
		prog_regsave `var' `glb' TRUE TRUE FALSE mi	
		
		mi estimate, cmdok: emargins ///
			"`glb' `var' i.cluster4 c.Months_NEET $covars if observed==1, `cond'"
		prog_regsave `var' `glb' FALSE TRUE TRUE mi
		}
	}
	
foreach glb in heckman heckprobit {
	foreach var of global `glb' {
		capture drop observed
		by NSID (imp), sort: gen observed = Employed_W8[1]==0 | (Employed_W8[1]==1 & !missing(`var'[1]))
		
		mi estimate, cmdok: emargins ///
			"`glb' `var' i.Any_NEET $covars if observed==1, select(Employed_W8 = i.Any_NEET $covars)"
		prog_regsave `var' `glb' TRUE FALSE FALSE mi	
		
		mi estimate, cmdok: emargins ///
			"`glb' `var' i.cluster4 $covars if observed==1, select(Employed_W8 = i.cluster4 $covars)"
		prog_regsave `var' `glb' FALSE FALSE TRUE mi
			
		mi estimate, cmdok: emargins ///
			"`glb' `var' i.Any_NEET c.Months_NEET $covars if observed==1, select(Employed_W8 = i.Any_NEET c.Months_NEET $covars)"
		prog_regsave `var' `glb' TRUE TRUE FALSE mi		
		
		mi estimate, cmdok: emargins ///
			"`glb' `var' i.cluster4 c.Months_NEET $covars if observed==1, select(Employed_W8 = i.cluster4 c.Months_NEET $covars)"
		prog_regsave `var' `glb' FALSE TRUE TRUE mi
		}
	}
log close
