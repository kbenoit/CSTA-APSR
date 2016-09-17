/***
 *** Preliminary analysis of the master coding set
 ***
 ***
 ***
 ***
 ***/

// set correct working directory (for Ken's system)
if c(username)=="kbenoit" {
  chdir "~/Dropbox/Papers/CMP_recoding/newanalysis/3_addCFdata"
}
if c(username)=="ml127" {
  cd "C:\Users\ml127\Dropbox\Text\Crowdsourcing\newanalysis\3_addCFdata"
}
//
// Table of summary means by manifesto - mean of sentence means
//
use coding_all_long_2013-10-23.dta if manifestoid>0 & manifestoid<=6 & source!=3, clear
collapse (mean) code (count) Ncoded=code, by(scale party year manifestoid source sentenceid) 
statsby, by(source manifestoid party year scale) saving(../temp/prelimSummary.dta, replace): ci code
use ../temp/prelimSummary.dta, replace
table manifestoid source scale if scale>0, contents(mean mean mean se mean N) format(%6.2f)

//
// RE logit analysis in Stata
//
// use just Labour 1997 for now
use coding_all_long_2013-10-23.dta if manifestoid==6, clear
tab scale, gen(areadum) // generate policy area dummies
xtmelogit areadum1 || sentenceid:
predict sentenceRE, reffects
predict sentenceREse, reses
predict catprob
collapse (mean) sentenceRE sentenceREse catprob (count) Ncodes=coderid (sum) areadum*, by(sentenceid)
gen propnone = areadum1 / Ncodes
browse sentence* catprob propnone
scatter catprob propnone, msize(vsmall) m(Oh) scheme(s1mono) jitter(.5) ///
    xtitle(Empirical proportion of not econ or social) ytitle(RE logit predicted probability of not econ or social) ///
    yline(.5, lp(dash)) xline(.5, lp(dash))


//
// compute and compare 'overall' manifesto scores by policy area - separate crowd results
//
use coding_all_long_2013-10-23.dta if manifestoid>0 & manifestoid<=6, clear
preserve
gen source2 = stage
recode source2 (0/3 = 1) (4/5 = 2) (6 = 3) 
label define source2 1 Experts 2 DrewCF 3 KenCF
label values source2 source2
collapse (mean) code (count) Ncoded=code, by(scale party year manifestoid source2 sentenceid) 
table manifestoid source2 scale if scale>0, contents(sd code n code) format(%6.2f)
// save summary results for ES to a dataset
statsby, by(source2 manifestoid party year scale) saving(../temp/prelimSummary.dta, replace): ci code
use ../temp/prelimSummary.dta, clear
drop if scale=="None":scale
keep mean manifestoid source2 scale
decode source2, gen(source)
// decode scale, gen(scalet)
drop source2 // scale
reshape wide mean, i(manifestoid scale) j(source) string
graph matrix mean* if scale=="Economic":scale, msize(vsmall) mlabel(manifestoid) scheme(s2mono) name(twoCF, replace)
list, clean
restore

//
// compute and compare 'overall' manifesto scores by policy area - combined crowd results
//
use coding_all_long_2013-10-23.dta if manifestoid>0 & manifestoid<=6, clear
preserve
drop if source==3
collapse (mean) code (count) Ncoded=code, by(scale party year manifestoid source sentenceid) 
table manifestoid source scale if scale>0, contents(sd code n code) format(%6.2f)
// save summary results for ES to a dataset
statsby, by(source manifestoid party year scale) saving(../temp/prelimSummary.dta, replace): ci code
use ../temp/prelimSummary.dta, clear
drop if scale=="None":scale
keep mean manifestoid source scale
decode source, gen(source2)
// decode scale, gen(scalet)
drop source // scale
reshape wide mean, i(manifestoid scale) j(source2) string
graph twoway (scatter mean*, msize(vsmall) mlabel(manifestoid)) ///
             (lfit mean*, lcol(gray) lstyle(dash)) if scale=="Economic":scale, ///
    legend(off) scheme(s1mono) ///
    ytitle(Crowd Mean) ///
    xscale(range(-1 1)) yscale(range(-1 1)) ylabel(-1(.5)1) xlabel(-1(.5)1) ///
    ysize(7) xsize(7) name(oneCF, replace)
list, clean
restore


tab scale, gen(areadum)
tab code, gen(scaledum)
collapse (count) Ncodes=coderid (sum) areadum* scaledum*, by(stage sentenceid)

gen propnone = areadum1 / Ncodes
gen propecon = areadum2 / Ncodes
gen propsoc  = areadum3 / Ncodes

gen propleft   = (scaledum1 + scaledum2)/Ncodes
gen propcentre = (scaledum3)/Ncodes
gen propright  = (scaledum4 + scaledum5)/Ncodes

drop *dum* 

decode stage, gen(stagetxt)
drop stage Ncodes
reshape wide prop*, i(sentenceid) j(stagetxt) string

graph matrix propnone*, jitter(2) msize(tiny) name(none, replace)
graph matrix propecon*, jitter(2) msize(tiny) name(econ, replace)
graph matrix propsoc*, jitter(2) msize(tiny) name(soc, replace)


// NOT WORKING YET
// calculate Fleiss's kappa statistics, after reshaping to wide
//
gen coder = string(coderid) // encode as text
drop coderid
gen sentence=string(sentenceid)
generate codethree = code
recode codethree (-2 = -1) (2 = 1)
gen economic = code if scale=="Economic":scale
gen social  = code if scale=="Social":scale
// keep if er=="Sequential":er  // keep sequential only
drop if source=="SemiExperts":source
reshape wide scale code codethree econ soc, i(coder source) j(sentenceid) string



