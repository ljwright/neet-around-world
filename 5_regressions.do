* Set Up
** DO IN R ***
// replace imp=0 if imp==.
// replace FirstChild=. if Child_W8!=1
// replace LogPay_W8=. if  Status_W8!=1
// replace Precarious_W8=. if  Status_W8!=1
// replace ShiftWork_W8=. if  Status_W8!=1
// replace cluster4=0 if Months_NEET==0
// replace Any_NEET=0 if Months_NEET==0 
// replace Any_NEET=1 if Months_NEET>0 & !missing(Months_NEET)
// gen Employed_W8=(Status_W8==1) if !missing(Status_W8)
//
// svyset [pweight=Survey_Weight_W8]
// svy: mean FirstChild if imp>0
// matrix mean=e(b)
// replace FirstChild=(FirstChild-mean[1,1])
// label variable FirstChild "Mean=`=mean[1,1]'"
// replace FirstChild=0 if FirstChild==. & Child_W8==0



	* Define Macros
global covars	1.Female#0.Child_W8 0.Female#1.Child_W8 1.Female#1.Child_W8 ///
				0.Female#1.Child_W8#c.FirstChild 1.Female#1.Child_W8#c.FirstChild ///
				i.Any_VET	///
				c.GHQ_W2_Caseness i.Health_W2 i.Disabled_W1	///
				c.Risk_W2 c.SchoolAtt_W2 i.Education_W8 c.LOC_W2_Factor	///
				i.HHType_W1 i.EngLangHH_W1 c.IMD_W2 /// 
				i.ParentEduc_W1 i.GParentUni_W1 i.NonWhite i.NSSEC3_W1

log using "Notes/regressions.log", replace

capture program drop prog_regsave
program define prog_regsave
	args outcome estimator Any_NEET Months_NEET cluster4 type file
	
	if "`file'"!="replace" local file append	
	regsave using "${dta_fld}/regsave.dta", ///
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


* 2. Run Regressions ----
local i = 0	
foreach mi in mi obs{

if "`mi'" == "mi"{
	mi import flong, id(NSID) m(imp) clear
	mi svyset [pweight=Survey_Weight_W8]
	mi xtset, clear 

	misstable sum * if imp>0, all
	local svy "mi estimate, post cmdok: svy"
}
else{
	mi unset, asis
	keep if imp == 0
	svyset [pweight = Survey_Weight_W8]
	local svy "svy"
}


* Predicting Any/Months NEET & Cluster Membership
local i = `i' + 1
if `i' == 1 local file replace
prog_observed Any_NEET
`svy': logit Any_NEET	$covars if observed==1, or
prog_regsave Any_NEET logit FALSE FALSE FALSE `mi' `file'

prog_observed Months_NEET	
`svy': zinb Months_NEET $covars if observed==1, inflate(${covars})
prog_regsave Months_NEET zinb FALSE FALSE FALSE `mi'

prog_observed cluster4	
`svy': mlogit cluster4 $covars if observed==1, rrr
prog_regsave cluster4 mlogit FALSE FALSE FALSE `mi'


* Predicting Wave 8 Outcomes
	* Q1: Does Cluster Membership Add Any Information Over Months NEET?
		* Three models: + Months; + Cluster; + Months + Cluster
			* Qualitatively and with Likelihood Ratio Test			
global reg			GHQ_W8_Likert LifeSat_W8 LOC_W8_Factor AUDIT_W8 Adult_W8
global tobit 		AUDIT_W8 Adult_W8
global logit		Smoker_W8 Employed_W8 FinDiff_W8 PoorHealth_W8
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
		
		`svy': `glb' `var' i.Any_NEET $covars if observed==1, `cond'
		prog_regsave `var' `glb' TRUE FALSE FALSE `mi'	
		
		`svy': `glb' `var' i.cluster4 $covars if observed==1, `cond'
		prog_regsave `var' `glb' FALSE FALSE TRUE `mi'
			
		`svy': `glb' `var' i.Any_NEET c.Months_NEET $covars if observed==1, `cond'
		prog_regsave `var' `glb' TRUE TRUE FALSE `mi'	
		
		`svy': `glb' `var' i.cluster4 c.Months_NEET $covars if observed==1, `cond'
		prog_regsave `var' `glb' FALSE TRUE TRUE `mi'
		}
	}
	
foreach glb in heckman heckprobit {
	foreach var of global `glb' {
		capture drop observed
		by NSID (imp), sort: gen observed = Employed_W8[1]==0 | (Employed_W8[1]==1 & !missing(`var'[1]))
		
		`svy': `glb' `var' i.Any_NEET $covars if observed==1, ///
			select(Employed_W8 = i.Any_NEET $covars)
		prog_regsave `var' `glb' TRUE FALSE FALSE `mi'	
		
		`svy': `glb' `var' i.cluster4 $covars if observed==1, ///
			select(Employed_W8 = i.cluster4 $covars)
		prog_regsave `var' `glb' FALSE FALSE TRUE `mi'
			
		`svy': `glb' `var' i.Any_NEET c.Months_NEET $covars if observed==1, ///
			select(Employed_W8 = i.Any_NEET c.Months_NEET $covars)
		prog_regsave `var' `glb' TRUE TRUE FALSE `mi'		
		
		`svy': `glb' `var' i.cluster4 c.Months_NEET $covars if observed==1, ///
			select(Employed_W8 = i.cluster4 c.Months_NEET $covars)
		prog_regsave `var' `glb' FALSE TRUE TRUE `mi'
		}
	}
}
log close