// State codes

import excel using "./Data/State_elections_own/state_codes.xlsx", clear
ren (A B) (code_num state)

statenames, sv(state)

egen code_s = sieve(code_num), keep(numeric)
replace code_num = code_s 
drop code_s

save "./Data/_gen/state/state_codes.dta", replace
