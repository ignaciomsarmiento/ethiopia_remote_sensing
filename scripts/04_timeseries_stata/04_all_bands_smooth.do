
/*
Replication files  
*/

clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Research/Ethiopia/REMOTE_SENSING"




*Load Data
use "data/panel_stata.dta"

set seed 10101

gen weight =1/ cloudcov

encode panel_id, gen(id)
drop panel_id

encode Name, gen(polygon)

encode kebele, gen(subdivision)
xtset id date,  delta(10 days) 


gen imp_for=(land_use=="Improved forage")
tab imp_for



mat def A=J(5,1,.)

local vars
forvalues i = 0(1)14 {
	local vars `vars' l`i'.ndvi_w_center_9  l`i'.blue_w_center_9  l`i'.red_w_center_9  l`i'.green_w_center_9  l`i'.nir_w_center_9 
	 crossfold logit  imp_for `vars'  i.subdivision  
	mat def B=r(est)
	mat A=A,B
  	display "`vars'"
}
mat list A




preserve
clear
svmat2 A

save "data/rmse_allbands_smooth_logit_stata.dta"  , replace
restore