// prep_delim_districts.do


/* [> Add delimitation data <] */ 
use  "${gen}/delimitations_all.dta", clear
gen district_crosswalk = state + "_" + district_name

replace district_crosswalk = strltrim(district_crosswalk)
encode district_crosswalk, gen(district_id)

bysort district_id : gen seq=_n
bysort district_id : gen max=_N


gsort district_id
br
keep if seq==max

replace district_crosswalk = lower(district_crosswalk)

save  "${gen}/delimitations_district.dta", replace

keep district_crosswalk district_id
save  "${gen}/delimitations_district_ids.dta", replace





