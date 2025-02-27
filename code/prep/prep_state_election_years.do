
use "${data}/Bhavnani_State_Elections/Bhavnani India State Election Dataset v 3.0.dta", clear

collapse electors, by(st_name year)
keep st_name year

statenames, sv(st_name)
gen election_year=1
save "$gen/state_election_years.dta", replace
