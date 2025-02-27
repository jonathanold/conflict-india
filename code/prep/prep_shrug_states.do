// Shrug states
use "./Data/SHRUG/shrug-v1.2.samosa-assembly-dta/con_shrug_2008.dta", replace

gen code_num = substr(ac08_id, 6,2)

merge m:1 code_num using "./Data/_gen/state/state_codes.dta"

collapse (sum) *_pca_* *_emp_* *_sch *_area, by(code)

ren * a_*
ren a_code code
save "./Data/_gen/state/state_shrug_08.dta", replace


use "./Data/SHRUG/shrug-v1.2.samosa-assembly-dta/con_shrug_2007.dta", replace

gen code_num = substr(ac07_id, 6,2)

merge m:1 code_num using "./Data/_gen/state/state_codes.dta"

collapse (sum) *_pca_* *_emp_* *_sch *_area, by(code)

ren * b_*
ren b_code code
save "./Data/_gen/state/state_shrug_07.dta", replace

merge 1:1 code using "./Data/_gen/state/state_shrug_08.dta"
