
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

tab kebele, gen(subdiv)

gen imp_for=(land_use=="Improved forage")
tab imp_for



mat def A=J(5,1,.)

 cvauroc  imp_for ndvi  [pw=weight], kfold(2) seed(3489)

local vars
forvalues i = 0(1)2 {
	local vars `vars' l`i'.ndvi
	cvAUROC  imp_for `vars'  i.subdivision [pw=weight], kfold(5) seed(3489)
  	display "`vars'"
}
mat list A

/* crossfold logit  imp_for `vars'  i.subdivision  
	mat def B=r(est)
	mat A=A,B */


preserve
clear
svmat2 A

save "data/rmse_ndvi_logit_stata.dta"  , replace
restore