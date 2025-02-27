


import excel using "$data/India-Official_Crime_Stats/SC_District_2013.xls", firstrow clear

ren * , lower
cap ren statesuts state
cap ren stateut state
replace state = lower(state)

statenames, sv(state)
replace district = lower(district)
drop if district=="total"
replace code = lower(code)

ren protectionofcivilrightspcr pcr 
ren preventionofatrocitiespoaa poa 
ren othercrimesagainstscs other

egen total = rowtotal(murder rape kidnappingandabduction dacoity robbery arson hurt pcr poa other)

save "$gen/crimes_sc_district_13.dta", replace



import excel using "$data/India-Official_Crime_Stats/SC_District_2014.xls", firstrow clear

ren * , lower
cap ren statesuts state
cap ren stateut state
replace state = lower(state)

statenames, sv(state)
replace district = lower(district)
drop if district=="total"
replace code = lower(code)

ren totalcrimesagainstscs total 
egen murder = rowtotal(poa_murder ipc_murder)
save "$gen/crimes_sc_district_14.dta", replace





import excel using "$data/India-Official_Crime_Stats/SC_District_2001-2012.xls", firstrow clear

ren * , lower
cap ren statesuts state
cap ren stateut state
replace state = lower(state)

statenames, sv(state)
replace district = lower(district)
drop if district=="total"

replace code = lower(code)

ren protectionofcivilrightspcr pcr 
ren preventionofatrocitiespoaa poa 
ren othercrimesagainstscs other

egen total = rowtotal(murder rape kidnappingandabduction dacoity robbery arson hurt pcr poa other)

save "$gen/crimes_sc_district_12.dta", replace


append using "$gen/crimes_sc_district_13.dta"
append using "$gen/crimes_sc_district_14.dta"

replace district="nawanshahr" if district=="sbs nagar"
replace district="mohali" if district=="sas nagar"

gen district_crime = code + "_" + district

gen kachchh = 0 
replace kachchh = 1 if district_crime == "gj_kachchh east(g)"
replace kachchh = 1 if district_crime == "gj_kachchh west(g)"
preserve
keep if kachchh==0
tempfile a
save `a'
restore 
collapse (sum) murder rape kidnappingandabduction dacoity robbery arson hurt poa pcr other  total protectionofcivilrightsact poa_murder poa_attempttocommitmurder poa_rape poa_attempttocommitrape poa_assaultonwomenwithintent poa_sexualharassment k poa_voyeurism poa_stalking poa_othersexualharassment poa_insulttothemodestyofwom poa_kidnappingabduction_grand poa_kidnapingabduction_total poa_kidnapingabductioninord poa_kidnappingforransom poa_kidnappingabductionofwo poa_otherkidnapping poa_dacoity poa_dacoitywithmurder poa_otherdacoity poa_robbery poa_arson poa_grievoushurt poa_hurt poa_acidattack poa_attempttoacidattack poa_riots poa_otheripccrimes poa_scstpreventionofatroc totalofscstpreventionofat ipc_murder ipc_attempttocommitmurder ipc_rape ipc_attempttocommitrape ipc_assaultonwomenwithintent ipc_sexualharassment ao ipc_voyeurism ipc_stalking ipc_othersexualharassment ipc_insulttothemodestyofwom ipc_kidnappingabduction ipc_kidnapingabduction ipc_kidnapingabductioninord ipc_kidnappingforransom ipc_kidnappingabductionofwo ipc_otherkidnapping ipc_dacoity ipc_dacoitywithmurder ipc_otherdacoity ipc_robbery ipc_arson ipc_grievoushurt ipc_hurt ipc_acidattack ipc_attempttoacidattack ipc_riots ipc_otheripccrimes totalipccrimesagainstscs manualscavengersandconstructi othersllcrimeagainstscs, ///
	by(kachchh year)
keep if kachchh==1

append using `a'
replace district_crime = "gj_kachchh" if kachchh==1
encode district_crime, gen(id_crime)

drop kachchh



ren total totalcrimes

ren (murder rape kidnappingandabduction dacoity robbery arson hurt) (cmurder crape ckidnap cdacoity crobbery carson churt)


duplicates drop district_crime year, force
gen obs_in_crimes = 1

save "$gen/crimes_sc_district.dta", replace


gen one = 1
cap drop dunique
unique district_crime, by(district_crime) gen(dunique)
keep if dunique==1

save "$gen/crimes_sc_district_ids.dta", replace



/*----------------------------------------------------*/
   /* [>   2.  Match IDs with delimitation data   <] */ 
/*----------------------------------------------------*/
matchit id_crime district_crime using "${gen}/delimitations_district_ids.dta" , idusing(district_id) txtusing(district_crosswalk) override


// Drop matched districts from different states
drop if  substr(district_crime,1,2)!=substr(district_crosswalk,1,2)

replace similscore = round(similscore, 0.0001)
cap drop max_*

bysort district_crime: egen max_1 = max(similscore)
bysort district_crosswalk: egen max_2 = max(similscore) 

cap drop keep
gen keep = 0
replace keep = 1 if round(similscore,0.0001) == round(max_1,0.0001) & round(max_1,0.0001) == round(max_2,0.0001)
bysort district_crime: egen max_3 = max(keep)

drop if keep==0 // & max_3==1
// br if keep==0

keep district_crime district_crosswalk

save "$gen/matchit_crime.dta", replace





