// prep_states_iyer.do

use "$data/Iyer_2012/tables1to5.dta", clear

keep if year==2007
keep stateuni majstate elec_womres elec_scres

replace stateuni = proper(stateuni)
gen code = ""
replace code="AP" if stateuni=="Andhra Pradesh"
replace code="AR" if stateuni=="Arunachal Pradesh"
replace code="AS" if stateuni=="Assam"
replace code="BR" if stateuni=="Bihar"
replace code="CT" if stateuni=="Chhattisgarh"
replace code="GA" if stateuni=="Goa"
replace code="GJ" if stateuni=="Gujarat"
replace code="HR" if stateuni=="Haryana"
replace code="HP" if stateuni=="Himachal Pradesh"
replace code="JK" if stateuni=="Jammu and Kashmir"
replace code="JK" if stateuni=="Jammu & Kashmir"
replace code="JH" if stateuni=="Jharkhand"
replace code="KA" if stateuni=="Karnataka"
replace code="KL" if stateuni=="Kerala"
replace code="MP" if stateuni=="Madhya Pradesh"
replace code="MH" if stateuni=="Maharashtra"
replace code="MN" if stateuni=="Manipur"
replace code="ML" if stateuni=="Meghalaya"
replace code="MZ" if stateuni=="Mizoram"
replace code="NL" if stateuni=="Nagaland"
replace code="OR" if stateuni=="Orissa"
replace code="PB" if stateuni=="Punjab"
replace code="RJ" if stateuni=="Rajasthan"
replace code="SK" if stateuni=="Sikkim"
replace code="TN" if stateuni=="Tamil Nadu"
replace code="TR" if stateuni=="Tripura"
replace code="UT" if stateuni=="Uttarakhand"
replace code="UP" if stateuni=="Uttar Pradesh"
replace code="WB" if stateuni=="West Bengal"
replace code="TN" if stateuni=="Tamil Nadu"
replace code="TR" if stateuni=="Tripura"
replace code="AN" if stateuni=="Andaman and Nicobar Islands"
replace code="CH" if stateuni=="Chandigarh"
replace code="DN" if stateuni=="Dadra and Nagar Haveli"
replace code="DD" if stateuni=="Daman and Diu"
replace code="DL" if stateuni=="Delhi"
replace code="LD" if stateuni=="Lakshadweep"
replace code="PU" if stateuni=="Pondicherry"


ren stateuni state_name


// Manually enter new states

save "$data/_gen/state/reservationstart.dta", replace



import excel using  "$data/State_elections_own/state_year_elections.xlsx", firstrow clear
keep state_name code elec_scres
gen majstate=0
append using "$data/_gen/state/reservationstart.dta"

save "$data/_gen/state/reservationstart.dta", replace
