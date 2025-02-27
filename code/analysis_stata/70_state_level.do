// Analysis on State-level


// Conflict data
use "./Data/_gen/state/fetzer_state.dta", clear


// Merge with reservation introduction staggered data
merge 1:1 code year using "./Data/_gen/state/crime_complete.dta"
drop _merge
merge m:1 code using "./Data/_gen/state/reservationstart.dta"
drop _merge

merge 1:1 code year using "./Data/_gen/state/crimes_sc_st.dta"
drop _merge
merge 1:1 code year using "./Data/_gen/state/state_gdp.dta"
drop _merge 

encode code, gen(id)
xtset id year
gen syear = -year

gsort id year
replace state_name = state_name[_n-1] if missing(state_name) & code == code[_n-1]
gsort id syear
xtset id syear
replace state_name = state_name[_n-1] if missing(state_name) & code == code[_n-1]
gsort id year

// Set dataset as panel
xtset id year


foreach var of varlist elec_scres elec_womres {
egen t_`var' = max(`var')
replace `var' = t_`var' if missing(`var')
drop t_`var'
}



// Make treatment variable
gen treat = (year>=elec_scres)
replace treat = . if missing(elec_scres)



// Simple DiD
/*
drop if code=="AR"

gen lriot = log(riots+0.001)

gen dummy_riot = (riots>0 & !missing(riots))
gen dummy_totalevents = (totalevents>0 & !missing(totalevents))
xtreg lriot i.year i.treat, fe vce(cluster id)

xtreg riot i.year i.treat, fe vce(cluster id)
xtreg murder i.year i.treat, fe vce(cluster id)
xtreg dacoity i.year i.treat, fe vce(cluster id)


tab state if missing(treat)


drop if missing(treat)
*/
replace state_name="Jammu \& Kashmir" if state_name=="Jammu and Kashmir"

// drop if year>=2010
drop id


order code state_name year syear treat gdp 
saveold "./Data/_gen/state/state_dataset.dta", replace version(12)
