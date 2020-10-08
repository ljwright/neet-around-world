
https://stats.idre.ucla.edu/stata/faq/how-can-i-get-margins-and-marginsplot-with-multiply-imputed-data/
capture program drop emargins
program emargins, eclass properties(mi)
  version 15
  svy: logit Any_NEET $covars if observed==1
  margins, dydx(*) post
end
mi estimate, cmdok: emargins


keep if imp == 1
mi unset, asis
svyset [pweight = Survey_Weight_W8]
local svy "svy"

prog_observed Any_NEET
svy: logit Any_NEET	$covars if observed==1
margins, dydx(*) post

prog_observed Months_NEET	
svy: zinb Months_NEET $covars if observed==1, inflate(${covars})
margins, dydx(*) post

prog_observed cluster4	
local covars: subinstr global covars "i.Education_W8" ""
svy: mlogit cluster4 `covars' if observed == 1 & Any_NEET == 1, rrr baseoutcome(2)
margins, dydx(*) post

svy: reg GHQ_W8_Likert i.cluster4 c.Months_NEET $covars if observed==1
margins, dydx(*) post

sum AUDIT_W8
local cond ll(`r(min)') ul(`r(max)') 
svy: tobit 	AUDIT_W8 i.cluster4 c.Months_NEET $covars if observed==1, `cond'
margins, dydx(*) post

capture drop observed
by NSID (imp), sort: gen observed = Employed_W8[1]==0 | (Employed_W8[1]==1 & !missing(`var'[1]))
svy: heckman LogPay_W8 i.cluster4 c.Months_NEET $covars if observed==1, ///
	select(Employed_W8 = i.cluster4 c.Months_NEET $covars)
margins, dydx(*) post

capture drop observed
by NSID (imp), sort: gen observed = Employed_W8[1]==0 | (Employed_W8[1]==1 & !missing(`var'[1]))
svy: heckprobit ShiftWork_W8 i.cluster4 c.Months_NEET $covars if observed==1 [pweight = Survey_Weight_W8, ///
	select(Employed_W8 = i.cluster4 c.Months_NEET $covars)
margins, dydx(*) post