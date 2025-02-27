
use "/Users/jonathanold/Dropbox/LSE/Extended Essay/Data/SHRUG/assembly_elections_clean.dta", clear
cap drop max_assembly
drop if pc01_state_name=="daman diu"
replace constituency_type="GEN" if tr_ac_name=="puri" & pc01_state_name=="orissa" 
replace constituency_type="GEN" if tr_ac_name=="titlagarh" & pc01_state_name=="orissa" 
replace constituency_type="GEN" if tr_ac_name=="palmaner" & pc01_state_name=="andhra pradesh" 

bysort pc01_state_id: egen max_assembly = max(assembly_no)

tab winner_type if constituency_type=="SC" | constituency_type=="ST" & assembly_no==max_assembly & bye_election==0
tab winner_type if constituency_type=="GEN" & assembly_no==max_assembly & bye_election==0

estpost tab winner_type if (constituency_type=="GEN" |  constituency_type=="SC" | constituency_type=="ST") &  assembly_no==max_assembly & bye_election==0
est store a
estpost tab winner_type if constituency_type=="GEN" & assembly_no==max_assembly & bye_election==0
est store b
estpost tab winner_type if (constituency_type=="SC" | constituency_type=="ST") & assembly_no==max_assembly & bye_election==0
est store c




esttab a b c using "/Users/jonathanold/Dropbox/LSE/Extended Essay/Output/sumstat.tex" , ///
	mgroups("All constituencies" "Unreserved" "Reserved", pattern(1 1 1)           ///
         prefix(\multicolumn{@span}{c}{) suffix(})   ///
         span erepeat(\cmidrule(lr){@span}))      ///
		 varlabels(GEN "General", blist(Total "\midrule\ "))  ///
	cells("b pct(fmt(2))") collabels("Number" "Percent") noobs nonumbers ///
	title("Assembly elections: Seats won by caste, 2014-2018 \label{tab:genscst}") ///
	replace  nomtitles ///
	booktabs 

