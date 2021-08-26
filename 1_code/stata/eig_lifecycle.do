* http://toni.marginalq.com/SJwriteup.pdf

clear all
set more off
capture log close

global workfold = "C:\\Dropbox\\Code\\Stata\\xtewreg\\"
cd ${workfold}
adopath + "${workfold}"

sjlog using "output", replace

use "ccm_a2.dta", clear
destring , replace

xtset gvkey
*xtset gvkey year, yearly

summarize year gvkey ia ia1 d1_ia LC CF q dROE sale Leverage at

tabulate LC, gen(LC)
list LC LC1 LC2 LC3 LC4 in 1/10, nolabel

regress ia LC1 LC2 LC4 CF q dROE sale Leverage at , vce(cluster gvkey)
xtewreg ia LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(3) mismeasured(4)

bootstrap t_LC1=(_b[LC1]/el(e(serr),1,1)) t_LC2=(_b[LC2]/el(e(serr),2,1)) t_LC4=(_b[LC4]/el(e(serr),3,1)) ///
		  t_CF=(_b[CF]/el(e(serr),4,1)) t_q=(_b[q]/el(e(serr),5,1)) t_dROE=(_b[dROE]/el(e(serr),6,1)) ///
		  t_sale=(_b[sale]/el(e(serr),7,1)) t_at=(_b[at]/el(e(serr),8,1)) t_cons=(_b[_cons]/el(e(serr),9,1)), rep(100) seed(1337) cluster(gvkey) notable: ///
	xtewreg ia LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(3) mismeasured(4) centmom(set)
estat bootstrap, p

regress ia1 LC1 LC2 LC4 CF q dROE sale Leverage at , vce(cluster gvkey)
xtewreg ia1 LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(5) mismeasured(4)

bootstrap t_LC1=(_b[LC1]/el(e(serr),1,1)) t_LC2=(_b[LC2]/el(e(serr),2,1)) t_LC4=(_b[LC4]/el(e(serr),3,1)) ///
		  t_CF=(_b[CF]/el(e(serr),4,1)) t_q=(_b[q]/el(e(serr),5,1)) t_dROE=(_b[dROE]/el(e(serr),6,1)) ///
		  t_sale=(_b[sale]/el(e(serr),7,1)) t_at=(_b[at]/el(e(serr),8,1)) t_cons=(_b[_cons]/el(e(serr),9,1)), rep(100) seed(1337) cluster(gvkey) notable: ///
	xtewreg ia1 LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(5) mismeasured(4) centmom(set)
estat bootstrap, p


regress d1_ia LC1 LC2 LC4 CF q dROE sale Leverage at , vce(cluster gvkey)
xtewreg d1_ia LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(5) mismeasured(4)

bootstrap t_LC1=(_b[LC1]/el(e(serr),1,1)) t_LC2=(_b[LC2]/el(e(serr),2,1)) t_LC4=(_b[LC4]/el(e(serr),3,1)) ///
		  t_CF=(_b[CF]/el(e(serr),4,1)) t_q=(_b[q]/el(e(serr),5,1)) t_dROE=(_b[dROE]/el(e(serr),6,1)) ///
		  t_sale=(_b[sale]/el(e(serr),7,1)) t_at=(_b[at]/el(e(serr),8,1)) t_cons=(_b[_cons]/el(e(serr),9,1)), rep(100) seed(1337) cluster(gvkey) notable: ///
	xtewreg d1_ia LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(5) mismeasured(4) centmom(set)
estat bootstrap, p

* Use data only with firms with valid EIG

use "ccm_a3.dta", clear
destring , replace

xtset gvkey

summarize year gvkey EIG d1_ia LC CF q dROE sale Leverage at

tabulate LC, gen(LC)
list LC LC1 LC2 LC3 LC4 in 1/10, nolabel

regress d1_ia LC1 LC2 LC4 CF q dROE sale Leverage at , vce(cluster gvkey)
xtewreg d1_ia LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(5) mismeasured(4)

bootstrap t_LC1=(_b[LC1]/el(e(serr),1,1)) t_LC2=(_b[LC2]/el(e(serr),2,1)) t_LC4=(_b[LC4]/el(e(serr),3,1)) ///
		  t_CF=(_b[CF]/el(e(serr),4,1)) t_q=(_b[q]/el(e(serr),5,1)) t_dROE=(_b[dROE]/el(e(serr),6,1)) ///
		  t_sale=(_b[sale]/el(e(serr),7,1)) t_at=(_b[at]/el(e(serr),8,1)) t_cons=(_b[_cons]/el(e(serr),9,1)), rep(100) seed(1337) cluster(gvkey) notable: ///
	xtewreg d1_ia LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(5) mismeasured(4) centmom(set)
estat bootstrap, p


regress EIG LC1 LC2 LC4 CF q dROE sale Leverage at , vce(cluster gvkey)
xtewreg EIG LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(5) mismeasured(4)

bootstrap t_LC1=(_b[LC1]/el(e(serr),1,1)) t_LC2=(_b[LC2]/el(e(serr),2,1)) t_LC4=(_b[LC4]/el(e(serr),3,1)) ///
		  t_CF=(_b[CF]/el(e(serr),4,1)) t_q=(_b[q]/el(e(serr),5,1)) t_dROE=(_b[dROE]/el(e(serr),6,1)) ///
		  t_sale=(_b[sale]/el(e(serr),7,1)) t_at=(_b[at]/el(e(serr),8,1)) t_cons=(_b[_cons]/el(e(serr),9,1)), rep(100) seed(1337) cluster(gvkey) notable: ///
	xtewreg EIG LC1 LC2 LC4 CF q dROE sale Leverage at, maxdeg(5) mismeasured(4) centmom(set)
estat bootstrap, p
** https://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/se_programming.htm
* regress EIG LC1 LC2 LC4 CF q dROE sale Leverage at, robust cluster(gvkey)
* cluster2 EIG LC1 LC2 LC4 CF q dROE sale Leverage at, fcluster(gvkey)  tcluster(year)
* areg EIG LC1 LC2 LC4 CF q dROE sale Leverage at, absorb(gvkey)

sjlog close

* https://lukestein.github.io/stata-latex-workflows/
