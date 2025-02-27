// Prep_Census_State


// 2001 census
import excel using "./Data/Census/Census_2001.xlsx", firstrow clear

foreach var of varlist total sc st {
replace `var' = subinstr(`var', "-", "", 1)
replace `var' = subinstr(`var', ",", "", 3)
destring `var', replace
replace `var' = 0 if missing(`var')
}

statenames, sv(state)
drop if code=="unknown"

save "./Data/_gen/state/census_2001.dta", replace





// 2011 census

import excel using "./Data/Census/Census_2011_AD.xlsx", firstrow clear
ren *, lower

keep if level=="STATE" & tru=="Total"
ren name stateuni
statenames, sv(stateuni)

keep code tot_p p_sc p_st

ren (tot_p p_sc p_st) (total sc st)

drop if code=="unknown"
gen year=2011
save "./Data/_gen/state/census_2011.dta", replace
append using "./Data/_gen/state/census_2001.dta"

// State in old configurations...
cap drop id
replace code="BR" if code=="JH"
replace code="MP" if code=="CT"
replace code="UP" if code=="UT"

collapse (sum) sc st total, by(code year)



save "./Data/_gen/state/census.dta", replace




