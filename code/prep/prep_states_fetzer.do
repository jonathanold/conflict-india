// prep_fetzer

use "$data/fetzer_2019_jeea/data files/ANALYSIS.DATA.dta", clear


collapse (sum) 	attacks arrest kill surrender abduct civilianattack securityattack terroristattack gtdattacks ///
				totalevents killedinjured killed arrested militantskilled civilianskilled securitykilled, ///
				by(code year)

gen state=""		
replace state="Andhra Pradesh" if code=="AP"
replace state="Arunachal Pradesh" if code=="AR"
replace state="Assam" if code=="AS"
replace state="Bihar" if code=="BR"
replace state="Chhattisgarh" if code=="CT"
replace state="Goa" if code=="GA"
replace state="Gujarat" if code=="GJ"
replace state="Haryana" if code=="HR"
replace state="Himachal Pradesh" if code=="HP"
replace state="Jammu and Kashmir" if code=="JK"
replace state="Jharkhand" if code=="JH"
replace state="Karnataka" if code=="KA"
replace state="Kerala" if code=="KL"
replace state="Madhya Pradesh" if code=="MP"
replace state="Maharashtra" if code=="MH"
replace state="Manipur" if code=="MN"
replace state="Meghalaya" if code=="ML"
replace state="Mizoram" if code=="MZ"
replace state="Nagaland" if code=="NL"
replace state="Orissa" if code=="OR"
replace state="Punjab" if code=="PB"
replace state="Rajasthan" if code=="RJ"
replace state="Sikkim" if code=="SK"
replace state="Tamil Nadu" if code=="TN"
replace state="Tripura" if code=="TR"
replace state="Uttarakhand" if code=="UT"

replace state="Uttar Pradesh" if code=="UP"
replace state="West Bengal" if code=="WB"
replace state="Andaman and Nicobar Islands" if code=="AN"
replace state="Chandigarh" if code=="CH"
replace state="Dadra and Nagar Haveli" if code=="DN"
replace state="Daman and Diu" if code=="DD"
replace state="Delhi" if code=="DL"
replace state="Lakshadweep" if code=="LD"
replace state="Pondicherry" if code=="PU"
				
				
ren state state_name
save "$gen/state/fetzer_state.dta", replace
