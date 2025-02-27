

// prep_district_data_from_delim_comm.do
use "${gen}/delimitations/all_states", clear

gen ac_name= name 
gen district_name =  district
replace state = upper(state)
// duplicates list ac_name district_name

merge 1:1 ac_name district_name state using "${gen}/delimitations_all_const.dta"
drop _merge 

drop if missing(district)



order name district state total sc perc_sc sc_reserved st_reserved 
gsort state district name 

label var name "Assembly constituency name"
label var district "District name"
label var state "State code"
label var total "Total population at assembly constituency level"
label var sc "SC population at assembly constituency level"
label var perc_sc "Share of SC population at assembly constituency level"
label var sc_reserved "AC reserved for SCs"
label var st_reserved "AC reserved for STs"

drop sc_seats n m is_district


drop if missing(district)
cap drop one
gen one = 1
label var one "One (for computations)"

cap {
bysort district state: egen dt_sc_reserved_total = sum(sc_reserved)
label var dt_sc_reserved_total "Reserved seats for SCs at district level"
bysort district state: egen dt_seats_total = sum(one)
label var dt_seats_total "Number of seats at district level"
gen dt_seats_perc_sc_reserved = dt_sc_reserved_total / dt_seats_total
label var dt_seats_perc_sc_reserved "Share of seats at district level reserved for SCs"

bysort district state: egen dt_pop_sc = sum(sc)
label var dt_pop_sc "Total SC population at district level"

bysort district state: egen dt_pop_total = sum(total)
label var dt_pop_total "Total population at district level"

gen dt_perc_sc_pop = dt_pop_sc/dt_pop_total
label var dt_perc_sc_pop "SC population share at district level"


bys state: egen st_pop_sc = sum(sc)
label var st_pop_sc "Scheduled caste population at state level"
cap drop st_tot
bys state: egen st_pop_total = sum(total)
label var st_pop_total "Total population at state level"
bys state: egen st_sc_reserved_total = sum(sc_reserved)
label var st_sc_reserved_total "Reserved seats for SCs at state level"
}

gsort state district name 

cap{
/* [> Generate variables for district-level variation <] */
gen dt_sc_seats_by_rule = dt_pop_sc/st_pop_sc*st_sc_reserved_total
label var dt_sc_seats_by_rule "Expected SC seats at district level (if rule followed)"

gen dt_sc_seats_by_alt_rule = dt_pop_sc/dt_pop_total*dt_seats_total
label var dt_sc_seats_by_rule "Expected SC seats at district level (if alternative rule followed - for placebo check)"
}
// gen dt_rounding_error = dt_sc_reserved_total-dt_sc_seats_by_rule
// label var dt_rounding_error "Difference between actual and expected SC seats at district level"

foreach v of varlist _all {
	local t`v' : variable label `v'
}

drop if missing(matchitname)
save "${gen}/delimitations_all_dist.dta", replace 


use "${gen}/delimitations_all_dist.dta", replace


do "$code/prep/prep_trivedi.do"


use "${gen}/trivedi_merged.dta", clear

collapse (firstnm) dt_sc_reserved_total dt_seats_total dt_seats_perc_sc_reserved dt_pop_sc dt_pop_total dt_perc_sc_pop st_pop_sc st_pop_total st_sc_reserved_total dt_sc_seats_by_rule dt_sc_seats_by_alt_rule dt_sc_reserved_total_trivedi dt_st_reserved_total_trivedi dt_sc_reserved_total_trivediL1 dt_st_reserved_total_trivediL1 dt_trivedi_seats , by(district state)
gsort state district


foreach v of varlist _all {
	cap label var `v' "`t`v''"
}

gen district_crosswalk = lower(state)+"_"+lower(district)
encode district_crosswalk, gen(id)
save "${gen}/district_delim_data.dta", replace 

