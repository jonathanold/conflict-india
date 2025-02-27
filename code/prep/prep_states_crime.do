
// 1992-2000

import excel using "./Data/India-Official_Crime_Stats/SC_ST_State/SC_1992.xls", firstrow clear
ren *, lower
cap gen crimes_against_sc = totali
cap replace crimes_against_sc = "" if crimes_against_sc=="NA"
cap destring crimes_against_sc, replace
cap gen state = stateut

gen year = 1992
keep year state crimes_against_sc
save "./Data/_gen/state/crimes_sc_st.dta",  replace


forvalues z=1993/2000 {
import excel using "./Data/India-Official_Crime_Stats/SC_ST_State/SC_`z'.xls", firstrow clear
ren *, lower
cap gen crimes_against_sc = incidence
cap gen crimes_against_sc = totalincidencei
cap gen crimes_against_sc = totali

cap gen state = stateut
cap gen state = statesut
cap gen state = stateutcity
cap replace crimes_against_sc = "" if crimes_against_sc=="NA"

cap destring crimes_*, replace

gen year = `z'
keep year state crimes_against_sc
append using "./Data/_gen/state/crimes_sc_st.dta"
save "./Data/_gen/state/crimes_sc_st.dta",  replace
}

drop if substr(state,1,4)=="Tota"


save "./Data/_gen/state/crimes_sc_st.dta",  replace


// 2014-2016
import excel using "./Data/India-Official_Crime_Stats/SC_ST_State/SC_2014_16.xls", firstrow clear
ren * ,lower
ren year* crimes_against_sc*
cap gen state = stateut
reshape long crimes_against_sc, i(state) j(year)
keep year state crimes_against_sc
drop if substr(state,1,4)=="Tota"

append using "./Data/_gen/state/crimes_sc_st.dta"
save "./Data/_gen/state/crimes_sc_st.dta",  replace


// 2001-2012
import excel using "./Data/India-Official_Crime_Stats/SC_District_2001-2012.xls", firstrow clear
ren *, lower
egen crimes_against_sc = rowtotal(murder rape kidn dacoi robber arson hurt prevent protect other)
keep if district=="TOTAL"
ren stateut state
keep year state crimes_against_sc
append using "./Data/_gen/state/crimes_sc_st.dta"
save "./Data/_gen/state/crimes_sc_st.dta",  replace


// 2013
import excel using "./Data/India-Official_Crime_Stats/SC_ST_State/SC_2013.xls", firstrow clear
ren *, lower
ren stateut state
keep if substr(crimehead, 1,3) == "Tot"
drop crimehead
ren c crimes_against_sc
gen year=2013
drop if substr(state,1,4)=="Tota"

append using "./Data/_gen/state/crimes_sc_st.dta"


statenames, sv(state)
label var crimes_against_sc "Total crimes against SC (aggregated on state level)"

gsort code year
save "./Data/_gen/state/crimes_sc_st.dta",  replace
use "./Data/_gen/state/crimes_sc_st.dta",  replace

