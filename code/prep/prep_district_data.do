
 
/*----------------------------------------------------*/
   /* [>   1.  Prepare underlying data   <] */ 
/*----------------------------------------------------*/
do "${code}/prep/prep_district_data_from_delim_comm.do"
do "$code/prep/prep_fetzer.do"
do "$code/prep/prep_bhavnani.do"
do "$code/prep/prep_sc_district_crimes.do"
do "$code/prep/prep_ovde.do"



/*----------------------------------------------------*/
   /* [>   2.  Merge data   <] */ 
/*----------------------------------------------------*/
use "${gen}/district_delim_data.dta", replace 
cap ren district_name district
/* [> Merge Fetzer data <] */ 
merge 1:1 district_crosswalk using "$gen/matchit_fetzer.dta"
drop _merge 
replace district_fetzer = district_crosswalk if missing(district_fetzer)
merge 1:m district_fetzer using "$gen/fetzer_district.dta"
drop _merge 
drop if mi(district)

/* [> Merge Bhavnani data <] */ 
merge m:1 district_crosswalk using "$gen/matchit_bhavnani.dta"
drop _merge 
replace district_bhavnani = district_crosswalk if missing(district_bhavnani)
replace district_bhavnani = district_fetzer if missing(district_bhavnani)
merge m:1 district_bhavnani using "$gen/bhavnani_district.dta"
drop _merge 

/* [> Merge Crime data <] */ 
merge m:1 district_crosswalk using "$gen/matchit_crime.dta"
drop _merge 
replace district_crime = district_crosswalk if missing(district_crime)
replace district_crime = district_fetzer if missing(district_crime)
replace district_crime = district_bhavnani if missing(district_crime)
merge m:1 district_crime year using "$gen/crimes_sc_district.dta"
drop _merge 


/* [> Merge OVDE data <] */ 
merge m:1 district_crosswalk using "$gen/matchit_ovde.dta"
drop _merge 
replace district_ovde = district_crosswalk   if missing(district_ovde)
replace district_ovde = district_fetzer      if missing(district_ovde)
replace district_ovde = district_bhavnani    if missing(district_ovde)
replace district_ovde = district_crime       if missing(district_ovde)
merge m:1 district_ovde year using "$gen/ovde_district.dta"
drop _merge 



merge m:1 code year using "$gen/state_election_years.dta"
drop if _merge==2
replace election_year=0 if _merge==1
drop _merge
label var election_year "State election held in year"

merge m:1 code year using "${gen}/state_treat.dta"
drop if _merge==2  




compress

order district_crosswalk id state year obs*
drop if missing(id)
gsort  id year

foreach v of varlist   obs* {
   replace `v'=0 if `v'==.
}


bys district_crosswalk: egen max_nax_pre2008 = max(deathevent) if year<=2008
replace max_nax_pre2008 = 0 if mi(max_nax_pre2008)
replace max_nax_pre2008 = 1 if max_nax_pre2008>0 & !mi(max_nax_pre2008)
bys district_crosswalk: egen any_nax_event = max(max_nax_pre2008)

bys district_crosswalk: egen total_nax_pre2008 = total(deathevent) if year<=2008
replace total_nax_pre2008 = 0 if mi(total_nax_pre2008)
bys district_crosswalk: egen total_nax_event = max(total_nax_pre2008)


save "${gen}/dataset_districts.dta", replace


keep year district_crosswalk any_nax_event
collapse (mean) any_nax_event, by(district_crosswalk)
save "${gen}/ovde_district_for_constituency.dta", replace


/*----------------------------------------------------*/
   /* [>   3.  Clean dataset   <] */ 
   /* Add if required. Otherwise in 04_analysis_district.do */
/*----------------------------------------------------*/
use "${gen}/dataset_districts.dta", replace

/*------------------------- 
Real SC share: perc_sc (AC) OR pop_perc_sc (district)
Real SC reservations: sc_reserved_total
Real seats total: seats_total
-------------------------*/ 
gsort id year 
xtset id year

cap drop nax
gen nax = 0
replace nax = 1 if code=="AP"
replace nax = 1 if code=="JH"
replace nax = 1 if code=="BR"
replace nax = 1 if code=="CT"
replace nax = 1 if code=="OR"
replace nax = 1 if code=="WB"
label var nax "Naxalites active (State-level)"

/* [> Amritsar is wrongly coded, correct this here. <] */ 

gen treatyear_bhavnani = substr(stateyear, -4,4)
destring treatyear_bhavnani, replace

bys state: egen treatyear = min(year) if state_treat==1
bys state: egen treatyearx = min(treatyear)
replace treatyear = treatyearx 
drop treatyearx

cap drop before after
gen before = (year<treatyear)
gen after = (year>=treatyear) & !mi(treatyear)



forv i=1/17{
   local j=1997+`i'
   gen y`i'=(year==`j')
}


gen lcrimes = log(totalcrimes)
gen ltotalevents = log(totalevents+1)





 
/*----------------------------------------------------*/
   /* [>  Generate variables relevant for identification   <] */ 
/*----------------------------------------------------*/
gen diff_pre = (Rpre - Fpre)
label var diff_pre "Difference between actual and expected seats before delimitation"

gen dt_diff_post = dt_sc_reserved_total-dt_sc_seats_by_rule
label var dt_diff_post "Difference between actual and expected SC seats at district level"

cap drop dt_rounding_error
gen dt_rounding_error = (dt_sc_reserved_total/dt_seats_total)-(dt_pop_sc/st_pop_sc)
label var dt_rounding_error "Difference between reserved seats share and population share of SCs"

gen diff_post_alt = (dt_sc_reserved_total/dt_seats_total - dt_perc_sc_pop)*dt_seats_total
label var diff_post_alt "Difference between actual and expected seats (based on population shares) after delim. Alternative rule"

gen diff_post = dt_diff_post
label var dt_diff_post "Difference between actual and expected SC seats at district level (based on state rule)"


gen seats_treat = dt_sc_reserved_total*after
label var seats_treat "Seats for SCs after delimitation"

gen diff_treat = dt_diff_post*after
label var diff_treat "Rounding error after delimitation (with state-level rule)"

gen dt_extra_seat = 0
replace dt_extra_seat = 1  if dt_diff_post>=0.2
replace dt_extra_seat = -1 if dt_diff_post<=-0.2
replace dt_extra_seat = . if dt_diff_post==.

gen dt_rounding_error_alt = (dt_sc_reserved_total/dt_seats_total - dt_perc_sc_pop) // before: Rpost - pop_perc_sc
label var dt_rounding_error_alt "Difference between reserved seats share and population share of SCs"

gen diff_treat_alt = dt_rounding_error_alt*after
label var diff_treat_alt "Rounding error after delimitation (with district-level rule)"

gen dt_seats_fraction_alt = dt_perc_sc_pop*dt_seats_total 
label var dt_seats_fraction_alt "Seats for SC if fractional seats could be reserved"


gen dt_extra_seat_alt = 0
replace dt_extra_seat_alt = 1  if diff_post>=0.2
replace dt_extra_seat_alt = -1 if diff_post<=-0.2
replace dt_extra_seat_alt = . if diff_post==.



gen dt_sc_perc_sc_reserved_trivedi = dt_sc_reserved_total_trivedi/dt_trivedi_seats
gen dt_sc_perc_st_reserved_trivedi = dt_st_reserved_total_trivedi/dt_trivedi_seats
gen dt_sc_perc_sc_reserved_trivediL1 = dt_sc_reserved_total_trivediL1/dt_trivedi_seats
gen dt_sc_perc_st_reserved_trivediL1 = dt_st_reserved_total_trivediL1/dt_trivedi_seats


sc  dt_seats_perc_sc_reserved dt_sc_perc_sc_reserved_trivedi, col(%2) jitter(15)

save "${gen}/dataset_districts.dta", replace


preserve 
collapse (firstnm) dt_extra_seat dt_extra_seat_alt diff_post any_nax_event total_nax_event , by(id district_crosswalk)
save "${gen}/district_data_for_ac.dta", replace
restore 


