/*----------------------------------------------------*/
   /* [>   3.  Make balance graphs - perc_sc   <] */ 
/*----------------------------------------------------*/
// use  "${gen}/delimitations_all.dta", clear
use "${gen}/constituency/const_data.dta", clear
set graph off
twoway (histogram perc_sc if sc_reserved==1, start(0) width(3.333333) color(maroon%60)) ///        
       (histogram perc_sc if sc_reserved==0, start(0) width(3.333333) color(emerald%60)), ///   
       graphregion(color(white)) xsize(6.5) ysize(4.5) legend(order(1 "Reserved" 2 "Unreserved" )) ///
	   title("All constituencies", color(black))
graph export  "${output}/figures/hist_all_ac.pdf", replace
	   
keep if sample_2==1
twoway (histogram perc_sc if sc_reserved==1, start(0) width(3.333333) color(maroon%60)) ///        
       (histogram perc_sc if sc_reserved==0, start(0) width(3.333333) color(emerald%60)), ///   
       graphregion(color(white)) xsize(6.5) ysize(4.5) legend(order(1 "Reserved" 2 "Unreserved" )) ///
	   title("All constituencies", color(black))
graph export  "${output}/figures/hist_sample_ac.pdf", replace
	
	   
twoway (histogram perc_sc if sc_reserved==1, start(0) width(3.3333333) color(maroon%60)) ///        
       (histogram perc_sc if sc_reserved==0 & sc_reserved[_n-1]==1  , start(0) width(3.333333) color(emerald%60)), ///   
       graphregion(color(white)) xsize(6.5) ysize(4.5) legend(order(1 "Reserved" 2 "Runner-up" ))	  ///
	   title("Reserved vs. Runner-up", color(black))
graph export  "${output}/figures/hist_runnerup_ac.pdf", replace
set graph on
	   


	   

// ac_balance.do
replace sc_prop_01_norm = sc_prop_01
foreach var of varlist sc_prop_01 pc01_pca_tot_p pc01_td_area pc01_vd_area pc01_vd_m_sch pc01_vd_p_sch pc01_pca_p_lit pc01_vd_tar_road {
local t : variable label `var'

qui sum `var'_norm, d
local w = r(max)/25
// Primary schools
colorpalette mrc, select(1 3) nograph

twoway (histogram `var'_norm if sc_sample_1==1,   width(`w')   color("`r(p1)'%70")) ///        
       (histogram `var'_norm if runner_up_sc_1==1,width(`w') color("`r(p2)'%70")), ///   
       graphregion(color(white)) xsize(6.5) ysize(4.5) legend(order(1 "Reserved" 2 "Runner-up" ))	  ///
	   title("Balance check: `t'") xti("`t'")
graph export  "${output}/figures/hist_`var'.pdf", replace
   

twoway (kdensity `var'_norm if sc_sample_1==1,  color("`r(p1)'%70") recast(area)) (kdensity `var'_norm if runner_up_sc_1==1, color("`r(p2)'%70") recast(area)) , ///
		legend(order(1 "Reserved" 2 "Runner-up" )) ///
		title("Balance check: `t'", color(black)) yti("Density")  xti("`t'") ///
		graphregion(color(white)) xsize(6.5) ysize(4.5)
graph export  "${output}/figures/kernel_`var'.pdf", replace

// cap drop `var'_norm
}
set graph on



// The following for the balance table!
 ttest pc01_pca_tot_p_norm if sample_1==1 , by(sc_reserved)
 
 global depvarlist  pc01_pca_tot_p_norm pc01_td_area_norm pc01_vd_area_norm  pc01_vd_tar_road_norm pc01_vd_m_sch_norm pc01_vd_p_sch_norm pc01_pca_p_lit_norm

 
reg sc_reserved sc_prop_01_norm $depvarlist  if sample_2==1

test $depvarlist
local fval: display %9.3f r(F)
local fpval: display %9.3f r(p)
// local fpval: display %9.3f Ftail(e(df_m), e(df_r), e(F))
 
balancetable  sc_reserved sc_prop_01_norm $depvarlist if sample_2==1 using "${output}/tables/balancetable.tex", ///
	replace varlabels pvalues ///
	groups("Runner-up" "Reserved" "Difference", pattern(1 1 1)) ///
	ctitles("(sd)" "(sd)" "(p-value)") ///
	 prefoot("\addlinespace  \midrule  Wald test statistic (Jointly zero) & & &  `fval' \\ p-value & & & `fpval' \\ \addlinespace \midrule ") ///
	format(%9.3f) booktabs
	
	
	
	
	
	
	
	
	
	
	
	

