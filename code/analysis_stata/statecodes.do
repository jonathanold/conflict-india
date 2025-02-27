// do.File to generate state codes from names

// State codes are used for merging
cap program drop statenames
program statenames, eclass
  	syntax [ , sv(name) * ]
	// Clean state variable
replace `sv' = lower(`sv')
replace `sv' = subinstr(`sv', " and", "", 1)
replace `sv' = subinstr(`sv', " nct", "", 1)
replace `sv' = subinstr(`sv', " ut", "", 1)
replace `sv' = subinstr(`sv', " of", "", 1)
replace `sv' = subinstr(`sv', "and ", "", 1)
replace `sv' = subinstr(`sv', "nct ", "", 1)
replace `sv' = subinstr(`sv', "ut ", "", 1)
replace `sv' = subinstr(`sv', "of ", "", 1)

replace `sv' = subinstr(`sv', " ", "", 10)
egen `sv'_s = sieve(`sv'), keep(alphabetic)
replace `sv' = `sv'_s
drop `sv'_s

	// State code variable
gen code = "unknown"
replace code="AP" if `sv'=="andhrapradesh"
replace code="AR" if `sv'=="arunachalpradesh"
replace code="AS" if `sv'=="assam"
replace code="BR" if `sv'=="bihar"
replace code="CT" if `sv'=="chhattisgarh"
replace code="GA" if `sv'=="goa"
replace code="GJ" if `sv'=="gujarat"
replace code="HR" if `sv'=="haryana"
replace code="HP" if `sv'=="himachalpradesh"
replace code="JK" if `sv'=="jammukashmir"
replace code="JK" if `sv'=="jammuandkashmir"

replace code="JH" if `sv'=="jharkhand"
replace code="JH" if `sv'=="jharkhard"

replace code="KA" if `sv'=="karnataka"
replace code="KL" if `sv'=="kerala"
replace code="MP" if `sv'=="madhyapradesh"
replace code="MH" if `sv'=="maharashtra"
replace code="MN" if `sv'=="manipur"
replace code="ML" if `sv'=="meghalaya"
replace code="MZ" if `sv'=="mizoram"
replace code="NL" if `sv'=="nagaland"
replace code="OR" if `sv'=="orissa"
replace code="OR" if `sv'=="odisha"

replace code="PB" if `sv'=="punjab"
replace code="RJ" if `sv'=="rajasthan"
replace code="SK" if `sv'=="sikkim"
replace code="TN" if `sv'=="tamilnadu"
replace code="TR" if `sv'=="tripura"
replace code="UT" if `sv'=="uttarakhand"
replace code="UT" if `sv'=="uttarkhand"
replace code="UT" if `sv'=="uttaranchal"
replace code="UT" if `sv'=="uttranchal"


replace code="UP" if `sv'=="uttarpradesh"
replace code="WB" if `sv'=="westbengal"
replace code="AN" if `sv'=="andamannicobarislands"
replace code="AN" if `sv'=="andamanmicobar"
replace code="AN" if `sv'=="andamanandnicobar"
replace code="AN" if `sv'=="anslands"
replace code="AN" if `sv'=="anislands"

replace code="CH" if `sv'=="chandigarh"
replace code="DN" if `sv'=="dadranagarhaveli"
replace code="DN" if `sv'=="dnhaveli"

replace code="DD" if `sv'=="damandiu"

replace code="DL" if `sv'=="delhi"
replace code="DL" if `sv'=="nationalcapitalterritorydelhi"

replace code="LD" if `sv'=="lakshdweep"
replace code="LD" if `sv'=="lakshadweep"
replace code="PU" if `sv'=="pondicherry"
replace code="PU" if `sv'=="puducherry"
replace code="TG" if `sv'=="telangana"
	
cap ren `sv' state
end
