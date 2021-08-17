clear
set more off
cd "F:\Next Steps 1-8\Projects\NEET Book"

* 1. Set Up ----
use "Data/mice_long.dta", clear
compress
if c(changed)	save "Data/mice_long.dta", replace
capture log close
log using "Notes/regressions.log", replace

* Recode Binary Variables
foreach var of varlist Any_NEET Employed_W8 FinDiff_W8 PoorHealth_W8 ShiftWork_W8 Precarious_W8{
	recode `var' (1 = 0) (2 = 1) 
	local lbl_1: label `var' 1
	local lbl_2: label `var' 2
	label define `var' 0 "`lbl_1'" 1 "`lbl_2'", replace
}

* Define Macros
global covars	i.Female##i.Child_W8##c.FirstChild ///
				i.Any_VET	///
				c.GHQ_W2_Caseness i.GenHealth_W2 i.Disabled_W2	///
				c.Risk_W1 c.SchoolAtt_W2 i.Education_W8 c.LOC_Factor_W2	///
				i.HHType_W1 i.ForLangHH_W1 c.IMD_W2 /// 
				i.ParentEduc5_W1 i.GParentUni_W1 i.NonWhite i.NSSEC3_W1

* Create Programs
capture program drop prog_regsave
program define prog_regsave
	args outcome estimator Any_NEET Months_NEET neet_cluster type file
	
	if "`file'"!="replace" local file append	
	regsave using "Data/regsave.dta", ///
		addlabel(outcome, `outcome', estimator, `estimator', ///
		Any_NEET, `Any_NEET', Months_NEET, `Months_NEET', ///
		neet_cluster, `neet_cluster', type, `type') pval ci `file'
end

capture program drop prog_observed
program define prog_observed
	args var dta_type
	capture drop observed weight_norm
	by NSID (imp), sort: gen observed = !missing(`var'[1])
	by NSID observed (imp), sort: egen total_weight = sum(Survey_Weight_W8)
	by NSID observed (imp), sort: gen weight_norm = Survey_Weight_W8 * _N / total_weight
	drop total_weight
	
	`dta_type' svyset [pweight = weight_norm]
end


* 2. Run Regressions ----
foreach mi in mi { // obs{

if "`mi'" == "mi"{
	mi import flong, id(NSID) m(imp) clear
	mi xtset, clear 
	local svy "mi estimate, post cmdok: svy"
	local dta_type mi
	local file replace
}
else{
	mi unset, asis
	keep if imp == 0
	local svy "svy"
	local dta_type
	local file
}


* Predicting Any/Months NEET & Cluster Membership
prog_observed Any_NEET `dta_type'
`svy': logit Any_NEET	$covars if observed==1
prog_regsave Any_NEET logit FALSE FALSE FALSE `mi' `file'

prog_observed Months_NEET `dta_type'
`svy': zinb Months_NEET $covars if observed==1, inflate(${covars})
prog_regsave Months_NEET zinb FALSE FALSE FALSE `mi'

prog_observed neet_cluster `dta_type'
local covars: subinstr global covars "i.Education_W8" ""
`svy': mlogit neet_cluster `covars' if observed == 1 & Any_NEET == 1, rrr baseoutcome(2)
prog_regsave neet_cluster mlogit FALSE FALSE FALSE `mi' 


* Predicting Wave 8 Outcomes
	* Q1: Does Cluster Membership Add Any Information Over Months NEET?
		* Three models: + Months; + Cluster; + Months + Cluster
			* Qualitatively and with Likelihood Ratio Test			
global reg			GHQ_W8_Likert LifeSat_W8 LOC_Factor_W8 AUDIT_W8 Adult_W8
global tobit 		AUDIT_W8 Adult_W8
global logit		Employed_W8 FinDiff_W8 PoorHealth_W8
global heckman		LogPay_W8 
global heckprobit 	ShiftWork_W8 Precarious_W8

foreach glb in reg logit tobit {
	foreach var of global `glb' {		
		prog_observed `var' `dta_type'	
		
		local cond
		if "`glb'"=="tobit"{
			sum `var'
			local cond ll(`r(min)') ul(`r(max)') 
			}
		
		`svy': `glb' `var' i.Any_NEET $covars if observed==1, `cond'
		prog_regsave `var' `glb' TRUE FALSE FALSE `mi'	
		
		`svy': `glb' `var' i.neet_cluster $covars if observed==1, `cond'
		prog_regsave `var' `glb' FALSE FALSE TRUE `mi'
			
// 		`svy': `glb' `var' i.Any_NEET c.Months_NEET $covars if observed==1, `cond'
// 		prog_regsave `var' `glb' TRUE TRUE FALSE `mi'	
//		
// 		`svy': `glb' `var' i.neet_cluster c.Months_NEET $covars if observed==1, `cond'
// 		prog_regsave `var' `glb' FALSE TRUE TRUE `mi'
		}
	}
	
foreach glb in heckman heckprobit {
	
	local maximize = cond("`glb'" == "heckprobit", "iterate(500)", "")
	
	foreach var of global `glb' {
		
		capture drop select_var
		gen select_var = 1 if Employed_W8 == 0 | (Employed_W8==1 & !missing(`var'))
		prog_observed select_var `dta_type'
		
		`svy': `glb' `var' i.Any_NEET $covars if observed==1, ///
			select(Employed_W8 = i.Any_NEET ${covars}) `maximize'
		prog_regsave `var' `glb' TRUE FALSE FALSE `mi'	
		
		`svy': `glb' `var' i.neet_cluster $covars if observed==1, ///
			select(Employed_W8 = i.neet_cluster ${covars}) `maximize'
		prog_regsave `var' `glb' FALSE FALSE TRUE `mi'
			
// 		`svy': `glb' `var' i.Any_NEET c.Months_NEET $covars if observed==1, ///
// 			select(Employed_W8 = i.Any_NEET c.Months_NEET ${covars}) `maximize'
// 		prog_regsave `var' `glb' TRUE TRUE FALSE `mi'		
//		
// 		`svy': `glb' `var' i.neet_cluster c.Months_NEET $covars if observed==1, ///
// 			select(Employed_W8 = i.neet_cluster c.Months_NEET ${covars}) `maximize'
// 		prog_regsave `var' `glb' FALSE TRUE TRUE `mi'
		}
	}
}
log close
