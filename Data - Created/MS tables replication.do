/*** DONT PUT ANALYSIS FILES INTO THIS FOLDER 
 ***
 ***       please :-)
 ***
 ***/

use "coding_all_long_2013-10-31.dta"

*****Tabulate coder and country frequencies*****
table country if source == 2
tab  coderid if source == 2
