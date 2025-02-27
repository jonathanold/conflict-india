// Prepare INSCR Conflict data

import excel using "$data/Polity/CIv2006-2.xls", firstrow clear

ren state code
keep if status == 1
drop if missing(code)

duplicates drop code year , force // Somehow, Manipur in 1971 has a duplicate observation

ren adunit stateuni

// State codes are coded differently
replace code="unknown"
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
replace code="JH" if stateuni=="Jharkhard"

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
replace code="UT" if stateuni=="Uttaranchal"


replace code="UP" if stateuni=="Uttar Pradesh"
replace code="WB" if stateuni=="West Bengal"
replace code="AN" if stateuni=="Andaman and Nicobar Islands"
replace code="AN" if stateuni=="Andaman/Nicobar"

replace code="CH" if stateuni=="Chandigarh"
replace code="DN" if stateuni=="Dadra and Nagar Haveli"
replace code="DN" if stateuni=="Dadra/Nagar Haveli"

replace code="DD" if stateuni=="Daman and Diu"
replace code="DL" if stateuni=="Delhi"
replace code="LD" if stateuni=="Lakshadweep"
replace code="PU" if stateuni=="Pondicherry"

ren stateuni state_name
drop if code=="unknown"

drop state_name


save "$gen/state/inscr_state.dta", replace







/* Scraped state-level data
*/

// Prepare INSCR Conflict data

import excel using "./Data/India-Official_Crime_Stats/Crimestats_Own.xlsx", firstrow clear

ren adunit stateuni
replace stateuni = proper(stateuni)
forvalues i = 1/10{
replace stateuni = subinstr(stateuni, " ", "", 1)
replace stateuni = subinstr(stateuni, " ", "", 1)
}
// State codes are coded differently
gen code="unknown"
replace code="AP" if stateuni=="AndhraPradesh"
replace code="AR" if stateuni=="ArunachalPradesh"
replace code="AS" if stateuni=="Assam"
replace code="BR" if stateuni=="Bihar"
replace code="CT" if stateuni=="Chhattisgarh"
replace code="GA" if stateuni=="Goa"
replace code="GJ" if stateuni=="Gujarat"
replace code="HR" if stateuni=="Haryana"
replace code="HP" if stateuni=="HimachalPradesh"
replace code="JK" if stateuni=="JammuAndKashmir"
replace code="JK" if stateuni=="Jammu&Kashmir"
replace code="JH" if stateuni=="Jharkhand"
replace code="JH" if stateuni=="Jharkhard"

replace code="KA" if stateuni=="Karnataka"
replace code="KL" if stateuni=="Kerala"
replace code="MP" if stateuni=="MadhyaPradesh"
replace code="MH" if stateuni=="Maharashtra"
replace code="MN" if stateuni=="Manipur"
replace code="ML" if stateuni=="Meghalaya"
replace code="MZ" if stateuni=="Mizoram"
replace code="NL" if stateuni=="Nagaland"
replace code="OR" if stateuni=="Orissa"
replace code="OR" if stateuni=="Odisha"

replace code="PB" if stateuni=="Punjab"
replace code="RJ" if stateuni=="Rajasthan"
replace code="SK" if stateuni=="Sikkim"
replace code="TN" if stateuni=="TamilNadu"
replace code="TR" if stateuni=="Tripura"
replace code="UT" if stateuni=="Uttarakhand"
replace code="UT" if stateuni=="Uttaranchal"

replace code="UP" if stateuni=="UttarPradesh"
replace code="WB" if stateuni=="WestBengal"
replace code="AN" if stateuni=="AndamanandNicobar Islands"
replace code="AN" if stateuni=="Andaman/Nicobar"
replace code="AN" if stateuni=="A&NIslands"


replace code="CH" if stateuni=="Chandigarh"
replace code="DN" if stateuni=="Dadra and Nagar Haveli"
replace code="DN" if stateuni=="Dadra/Nagar Haveli"
replace code="DN" if stateuni=="D&NHaveli"


replace code="DD" if stateuni=="Daman and Diu"
replace code="DD" if stateuni=="Daman&Diu"

replace code="DL" if stateuni=="Delhi"
replace code="DL" if stateuni=="DelhiUt"

replace code="LD" if stateuni=="Lakshadweep"
replace code="PU" if stateuni=="Pondicherry"
replace code="PU" if stateuni=="Puducherry"
replace code="TG" if stateuni=="Telangana"

ren stateuni state_name
drop if code=="unknown"

drop state_name

foreach var of varlist murder dacoity riots arson {
cap destring `var', replace
}

save "./Data/_gen/state/crime_state.dta", replace

use "./Data/_gen/state/inscr_state.dta", replace

append using "./Data/_gen/state/crime_state.dta"
gsort year code

save "./Data/_gen/state/crime_complete.dta", replace

