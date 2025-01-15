***************************************************
*** 	Families in Households Typology (FHT) 	***
***************************************************


/*

*/


//foreach x of global wave {

//use "$DATA/silc20`x'_grandparent.dta", clear

use "$DATA/silc2022_grandparent.dta", clear

browse country year hid pid mother_id father_id partner_id child1_id hx040 siblings grandparent


gen sp = (child1_id != . & partner_id == .)
egen single_parents = total(sp), by(country year hid)
lab var single_parents "Number of single parents in the HH"

gen prtnr = (partner_id != .)
egen partners = total(prtnr), by(country year hid)
lab var partners "Number of persons with partners in the HH"


gen t7_1 = (hx040 == 1) // single person HH
egen typ7_1 = max(t7_1), by(country hid)
gen t7_2 = (child1_id == . & partner_id != . & mother_id == . & father_id == . & unrelated == 0) // couples
egen typ7_2 = max(t7_2), by(country hid)
gen t7_3 = (child1_id != . & partner_id == . & mother_id == . & father_id == . & unrelated == 0 & grandparent == 0) // single parent
egen typ7_3 = max(t7_3), by(country hid)
gen t7_4 = (child1_id != . & partner_id != . & mother_id == . & father_id == . & unrelated == 0 & grandparent == 0) // couple with child(ren)
egen typ7_4 = max(t7_4), by(country hid)
gen t7_5 = (child1_id != . & partner_id == . & (mother_id != . | father_id != .) & unrelated == 0 & siblings == 0) // single parent & grandparent
egen typ7_5 = max(t7_5), by(country hid)
gen t7_6 = (child1_id != . & partner_id != . & (mother_id != . | father_id != .) & unrelated == 0 & siblings == 0) // couple with child(ren) & grandparent
egen typ7_6 = max(t7_6), by(country hid)
gen t7_7 = (child1_id == . & partner_id != . & (mother_id != . | father_id != .) & unrelated == 0) // couple with parent
egen typ7_7 = max(t7_7), by(country hid)


foreach x of numlist 1/7 {
	drop t7_`x'
}

egen complex_new = rowtotal(typ7_*) // when complex = 0 or complex >= 2, it is a complex household (doesn't fit any of the specified profiles or has more than one)

gen fht7 = 1		if typ7_1 == 1 & complex_new == 1 // single person HH
replace fht7 = 2	if typ7_2 == 1 & complex_new == 1 // couples
replace fht7 = 3 	if typ7_3 == 1 & complex_new == 1 // single parent
replace fht7 = 4	if typ7_4 == 1 & complex_new == 1 // couples with child(ren)
replace fht7 = 5 	if typ7_5 == 1 // single parent & grandparent
replace fht7 = 6 	if typ7_6 == 1 // couple with children & grandparent 
replace fht7 = 7 	if typ7_7 == 1
replace fht7 = 8 	if fht7 == . 


lab var fht7 "Families in Household Typology (7 categories)" 
//lab def fht7_l 1 "Single adult HH" 2 "Couples" 3 "Single parents" 4 "Couples with children" 5 "Single parents, grandparents" 6 "Couples with children, grandparents" 7 "Other"
lab def fht7_l 1 "Single adult HH" 2 "Couples" 3 "Single parents" 4 "Couples with children" 5 "Single parents, grandparents" 6 "Couples with children, grandparents" 7 "Couples with parent" 8 "Other"

lab val fht7 fht7_l 



*** ORIGINAL

gen typ_7 = 1 			if hx040 == 1 // single person household
replace typ_7 = 2 		if hx040 == 2 & partner_id != . // couples
replace typ_7 = 3 		if child1_id != . & partner_id == . & mother_id == . & father_id == . & unrelated == 0 // single parent
replace typ_7 = 4 		if child1_id != . & partner_id != . & mother_id == . & father_id == . & unrelated == 0 // couple with child(ren)
replace typ_7 = 5		if child1_id != . & partner_id == . & (mother_id != . | father_id != .) & unrelated == 0 // single parent & grandparent
replace typ_7 = 6 		if child1_id != . & partner_id != . & (mother_id != . | father_id != .) & unrelated == 0 // couple with child(ren) & grandparent

egen fht7_org = max(typ_7), by(country year hid)
replace fht7_org = 7 		if fht7_org == .

lab var fht7_org "Families in Household Typology (7 categories)" 
lab def fht7org_l 1 "Single adult HH" 2 "Couples" 3 "Single parents" 4 "Couples with children" 5 "Single parents, grandparents" 6 "Couples with children, grandparents" 7 "Other"
lab val fht7_org fht7org_l 







egen dc = rowtotal(child*_dep)
egen depchild = max(dc), by(country year hid)
// dependent_child is redundant 
egen dependent_child = max(depchild), by(country year hid)
lab var dependent_child "Number of dependent children in the HH"

egen sibs = max(siblings), by(country year hid)

gen parent = (child1_id != .)
lab var parent "R is a parent"

gen complex = (parent == 1 & siblings != 0)
egen complex_hh = max(complex), by(country year hid)
lab var complex_hh "Potentially complex HH"

* single person HH
gen typ_11 = 1 			if hx040 == 1 

* couples
replace typ_11 = 2 		if hx040 == 2 & partner_id != . 

* single parent w/dependent children (i.e. at least one dependent child in the HH)
replace typ_11 = 3 		if child1_id != . & partner_id == . & mother_id == . & father_id == . ///
						& unrelated == 0 & depchild != 0 & single_parents == 1 & partners == 0 

* single parent w/adult children (i.e. all children in the HH are non-dependent)
replace typ_11 = 4 		if child1_id != . & partner_id == . & mother_id == . & father_id == . /// 
						& unrelated == 0 & dependent_child == 0 & single_parents == 1 & partners == 0 

* couple w/dependent children (i.e. at least one dependent child in the HH)
replace typ_11 = 5 		if child1_id != . & partner_id != . & mother_id == . & father_id == . /// 
						& unrelated == 0 & depchild != 0 & partners == 2 & grandparent == 0

//replace typ_11 = 5 		if child1_id != . & partner_id != . & unrelated == 0 & depchild != 0 & partners == 2 & sibs == 0 & grandparent == 0

* couple w/adult children (i.e. all children in the HH are non-dependent)
replace typ_11 = 6 		if child1_id != . & partner_id != . & mother_id == . & father_id == . /// 
						& unrelated == 0 & depchild == 0 & partners == 2 & grandparent == 0

* single parent w/dependent children & grandparents
replace typ_11 = 7		if child1_id != . & partner_id == . & (mother_id != . | father_id != .) /// 
						& unrelated == 0 & depchild != 0 & grandparent == 1 & complex_hh == 0

* single parent w/adult children & grandparents
replace typ_11 = 8		if child1_id != . & partner_id == . & (mother_id != . | father_id != .) /// 
						& unrelated == 0 & dependent_child == 0 & grandparent == 1 & complex_hh == 0

* couple w/dependent children & grandparents
replace typ_11 = 9 		if child1_id != . & partner_id != . & (mother_id != . | father_id != .) /// 
						& unrelated == 0 & depchild != 0 & grandparent == 1 & complex_hh == 0

* couple w/adult children & grandparent
replace typ_11 = 10 		if child1_id != . & partner_id != . & (mother_id != . | father_id != .) ///
							& unrelated == 0 & dependent_child == 0 & grandparent == 1 & complex_hh == 0

* couples with parents
replace typ_11 = 11			if child1_id == . & partner_id != . & (mother_id != . | father_id != .) & unrelated == 0 						
* other types of households
//replace typ_11 = 11 		if child1_id != . & mother_id == . & father_id == . & partner_id == . & !inrange(typ_11,1,4)

							
egen fht11 = max(typ_11), by(country year hid)
* other types of households
replace fht11 = 12		if fht11 == .



lab var fht11 "Families in Household Typology (11 categories)" 
//lab def fht11_l 1 "Single adult HH" 2 "Couples" 3 "Single parents with dependent child" 4 "Single parents with adult child" 5 "Couples with dependent children" 6 "Couples with adult children" 7 "Single parents with dependent children, grandparents" 8 "Single parents with adult children, grandparents" 9 "Couples with dependent children, grandparents" 10 "Couples with adult chilren, grandparents" 11 "Other"  

lab def fht11_l 1 "Single adult HH" 2 "Couples" 3 "Single parents with dependent child" 4 "Single parents with adult child" 5 "Couples with dependent children" 6 "Couples with adult children" 7 "Single parents with dependent children, grandparents" 8 "Single parents with adult children, grandparents" 9 "Couples with dependent children, grandparents" 10 "Couples with adult chilren, grandparents" 11 "Couples with parents" 12 "Other"  

lab val fht11 fht11_l 

save "$DATA/silc20`x'_FHT.dta", replace

//}
