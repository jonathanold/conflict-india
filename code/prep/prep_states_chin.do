// Prep_States Chin_Prakash

use "./Data/Census_Prakash/india_poverty_july2010.dta", clear

cap drop stdum* 
cap drop yrdum*

statenames, sv(statenm)

append using "./Data/_gen/state/census.dta"

drop state
encode code, gen(id)


xtset id year
gsort id year
order code id year 
tsfill
gsort id year


cap drop copy
expand 2 if year==2011, gen(copy)
replace year = 2021 if copy==1
drop copy 
tsfill

gsort id year
replace code = code[_n-1] if id==id[_n-1] & missing(code)



ren total t
ren poplastcensus tlastcensus
foreach var of varlist sclastcensus stlastcensus {
replace `var' = `var' * tlastcensus / 100
}

foreach var of varlist sc st t {
cap drop `var'pop
gen `var'pop = `var'lastcensus if substr(string(year), 4,1)=="1"

replace `var'pop = `var' if !missing(`var')

cap drop gr
gen gr = (`var'pop/L10.`var'pop)^(1/10) - 1

forvalues i=1/9 {
replace gr = F1.gr if !missing(F1.gr)
}
replace gr = L1.gr if missing(gr) & year>=2011


replace `var'pop = L1.`var'pop*(1+gr) if missing(`var'pop)
replace `var'pop=`var'1pop if year<=1990

replace `var'pop = L1.`var'pop if missing(`var'pop) & L1.`var'pop==0
}

drop id

save "./Data/_gen/state/state_prakash.dta", replace


import excel using "./Data/State_elections_own/states_new_seats.xlsx", firstrow clear
statenames, sv(state)
save "./Data/_gen/state/state_seats.dta", replace


import excel using "./Data/State_elections_own/states_elections_delim.xlsx", firstrow clear
statenames, sv(state)
merge 1:1 code using  "./Data/_gen/state/state_seats.dta"
drop _merge

replace code="BR" if code=="JH"
replace code="MP" if code=="CT"
replace code="UP" if code=="UT"

collapse (sum) sc st sc_before st_before seats (min) year , by(code)
ren (sc st sc_before st_before) (sc_seats st_seats sc_before_seats st_before_seats)

save "./Data/_gen/state/state_seats.dta", replace


use "./Data/_gen/state/state_prakash.dta", clear
merge 1:1 code year using "./Data/_gen/state/state_seats.dta"


encode code, gen(id)
xtset id year
gsort id year


foreach var of varlist sc_seats st_seats seats {
replace `var'=. if seats==0 | seats==.
}

replace scseat1 = sc_seats if !missing(sc_seats)
replace stseat1 = st_seats if !missing(st_seats)

foreach var of varlist scseat1 stseat1 tseat1 {
replace `var'=L1.`var' if missing(`var')
}

gen sc_lcensus  = sclastcensus
gen st_lcensus = stlastcensus
gen pop_lcensus = tlastcensus

replace sc_lcensus = sc if missing(sc_lcensus)
replace st_lcensus = st if missing(st_lcensus)
replace pop_lcensus = t if missing(pop_lcensus)

foreach var of varlist sc_lcensus st_lcensus pop_lcensus {
replace `var' = L1.`var' if missing(`var')
}

keep code id year scseat1 stseat1 tseat1 scpop stpop tpop sc_lcensus st_lcensus pop_lcensus elecdum lstinclg ruralshare h1 h2 pg1 spg1 pg2 spg2

drop if year==2021 // Just auxiliary

drop if missing(tseat1)

local t = "sc st"
foreach var of  local t {
gen `var'_seatshare = `var'seat1 / tseat1
gen `var'_popshare	= `var'pop / tpop
gen `var'_censusshare = `var'_lcensus / pop_lcensus
}



drop id
save "./Data/_gen/state/state_prakash.dta", replace
merge 1:1 code year using "./Data/_gen/state/state_dataset.dta"



// Generate control variables
encode code, gen(id)
xtset id year
gsort id year
replace area = L1.area if missing(area)
xtset id syear
gsort id syear
replace area = L1.area if missing(area)
xtset id year
gsort id year

cap gen lpop=log(tpop)
cap gen lgdp = log(gdp)
cap gen density = tpop/area
cap gen ldensity = log(density)


foreach var of varlist totalevents killedinjured riots crimes_against_sc  {
cap drop l`var'
cap drop n`var'
gen l`var' = log(`var')
gen n`var' = `var'/tpop*10^6
xtreg  n`var' sc_seatshare ldensity lgdp i.year , fe vce(cluster id)
estimates store `var'_1

xtreg  n`var' sc_seatshare sc_censusshare sc_popshare ldensity  lgdp i.year, fe vce(cluster id)
estimates store `var'_2
}




#delimit ;
estout 
totalevents_1 totalevents_2
killedinjured_1 killedinjured_2
riots_1 riots_2
crimes_against_sc_1  crimes_against_sc_2
using "$output/t1.tex", style(tex)  
varlabels(sc_seatshare "\textbf{Share reserved seats}" lgdp "log(GDP per capita)"
sc_popshare "Share SC population" sc_censusshare "Share last census"
ldensity "Log(density)") 
drop(_cons *year)
order(sc_seatshare sc_popshare sc_censusshare lgdp ldensity)
cells(b(star fmt(%9.2f)) se(par)) 
stats(N  , fmt(%9.0fc) labels("N" ))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none) ; 
#delimit cr









