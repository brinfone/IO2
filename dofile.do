cd "C:\Users\bfone\OneDrive\Documents\M1\Empirical IO\ps2"

use "cars_ps", clear

** IVs price in different countries
** IV fuel consumption using weight?? as affects fuel consumption

** IV exogeneity and relevance
** weight doesnt affect price???

** could use fuel of competitors or in diff markets?

** logit model with fixed effects for eahc vehicle model

** generating L as potential market size
** is it fair ro implicitly assume that every 3rd individual could buy a car every year?

gen L = pop/3

** define market as 1 country 1 year

**generate market share of potential market

gen sj = qu/L

** generate sum of all cars sold in a given market

bysort year country: egen qt = sum(qu)

** generate number of unsold potential cars

gen q0 = L-qt

** generate share of potential market that doesnt get sold

gen s0 = q0/L

** generate log of potential market share over unsold potential market share

gen log_sj_so = ln(sj/s0)

** instruments for price??

bysort country year firm: egen firm_height = sum(height)
bysort country year firm: egen firm_horsepower = sum(horsepower)
bysort country year firm: egen firm_weight = sum(weight)
bysort country year firm: egen firm_width = sum(width)

bysort country year: egen oth_firm_height = sum(height)
bysort country year: egen oth_firm_horsepower = sum(horsepower)
bysort country year: egen oth_firm_weight = sum(weight)
bysort country year: egen oth_firm_width = sum(width)

replace oth_firm_height = oth_firm_height-firm_height
replace oth_firm_horsepower = oth_firm_horsepower-firm_horsepower
replace oth_firm_weight = oth_firm_weight-firm_weight
replace oth_firm_width = oth_firm_width - firm_width

bysort country year firm: egen firm_n = total(year<100000)

bysort country year: egen oth_firm_n = total(year<100000)

replace oth_firm_n = oth_firm_n - firm_n

replace firm_height = (firm_height-height)/(firm_n-1)
replace firm_horsepower = (firm_horsepower-horsepower)/(firm_n-1)
replace firm_weight = (firm_weight-weight)/(firm_n-1)
replace firm_width = (firm_wid-width)/(firm_n-1)

replace oth_firm_height = (oth_firm_height)/oth_firm_n
replace oth_firm_horsepower = (oth_firm_horsepower)/oth_firm_n
replace oth_firm_weight = (oth_firm_weight)/oth_firm_n
replace oth_firm_width = (oth_firm_wid)/oth_firm_n

** instruments for fuel
bysort year co: egen for_fuel = sum(fuel)
bysort year co: egen for_co_n = total(year<100000)

** this is wrong

replace for_fuel = (for_fuel-fuel)/(for_co_n - 1)

**regress

**gen country_year = year*10 + country

egen yearcountry = group(year country), label

xtset co yearcountry

**xtset co year

** is weight a good instrument
** does weight affect safety and therefore demand
** as using fixed effects dont have to worry about price endogeneity
** as long as this endogeneity is constant
** can't use other country stuff as IV for fuel as its constant
** time??
** idea would be indivilausl

xtivreg log_sj_so horsepower width height weight (princ fuel= height width weight horsepower firm_height firm_weight firm_horsepower firm_width oth_firm_height oth_firm_horsepower oth_firm_weight oth_firm_width for_fuel),fe vce()


** Q2

** gen sg

bysort country year class: egen qg = sum(qu)

gen sg = qg / L

** gen ln sj/sg

gen log_sjg = ln(sj/sg)

** gen other chars

bysort country year class firm: egen own_tot_horsepower = sum(horsepower)
bysort country year class firm: egen own_tot_height = sum(height)
bysort country year class firm: egen own_tot_weight = sum(weight)
bysort country year class firm: egen own_tot_width = sum(width)

bysort country year class: egen tot_horsepower = sum(horsepower)
bysort country year class: egen tot_height = sum(height)
bysort country year class: egen tot_weight = sum(weight)
bysort country year class: egen tot_width = sum(width)

bysort country year class firm: egen g_n = total(year<100000)

replace tot_horsepower = (tot_horsepower-own_tot_horsepower)
replace tot_height = (tot_height-own_tot_height)
replace tot_weight = (tot_weight-own_tot_weight)
replace tot_width = (tot_width-own_tot_width)

xtivreg log_sj_so horsepower width height weight (princ fuel log_sjg =  tot_height tot_horsepower tot_width tot_weight height width weight horsepower firm_height firm_weight firm_horsepower firm_width oth_firm_height oth_firm_horsepower oth_firm_weight oth_firm_width for_fuel), fe vce()

** Q3

** high level group gd domestic or foreign
** low level group gc class of car

** share of domestic

bysort country year domestic: egen qgd = sum(qu)
gen sgd = qgd / L

** share of domestic*class

bysort country year domestic class: egen qgc = sum(qu)
gen sgc = qgc / L

** small group over big group

gen log_shg = ln(sgc/sgd)

** individual over small group

gen log_sjhg = ln(sj/sgc)

** instruments for domestic

bysort country year domestic firm: egen own_tot_horsepower2 = sum(horsepower)
bysort country year domestic firm: egen own_tot_height2 = sum(height)
bysort country year domestic firm: egen own_tot_weight2 = sum(weight)
bysort country year domestic firm: egen own_tot_width2 = sum(width)

bysort country year domestic: egen tot_horsepower2 = sum(horsepower)
bysort country year domestic: egen tot_height2 = sum(height)
bysort country year domestic: egen tot_weight2 = sum(weight)
bysort country year domestic: egen tot_width2 = sum(width)

bysort country year domestic: egen g_n2 = total(year<100000)

replace tot_horsepower2 = (tot_horsepower2-own_tot_horsepower2)
replace tot_height2 = (tot_height2-own_tot_height2)
replace tot_weight2 = (tot_weight2-own_tot_weight2)
replace tot_width2 = (tot_width2-own_tot_width2)

** instruments for domestic*class

bysort country year domestic class firm: egen own_tot_horsepower3 = sum(horsepower)
bysort country year domestic class firm: egen own_tot_height3 = sum(height)
bysort country year domestic class firm: egen own_tot_weight3 = sum(weight)
bysort country year domestic class firm: egen own_tot_width3 = sum(width)

bysort country year domestic class: egen tot_horsepower3 = sum(horsepower)
bysort country year domestic class: egen tot_height3 = sum(height)
bysort country year domestic class: egen tot_weight3 = sum(weight)
bysort country year domestic class: egen tot_width3 = sum(width)

bysort country year domestic class: egen g_n3 = total(year<100000)

replace tot_horsepower3 = (tot_horsepower3-own_tot_horsepower3)
replace tot_height3 = (tot_height3-own_tot_height3)
replace tot_weight3 = (tot_weight3-own_tot_weight3)
replace tot_width3 = (tot_width3-own_tot_width3)

** regression

xtivreg log_sj_so horsepower width height weight (princ fuel log_shg log_sjhg= tot_height2 tot_horsepower2 tot_width2 tot_height3 tot_horsepower3 tot_width3 height width weight horsepower firm_height firm_weight firm_horsepower firm_width oth_firm_height oth_firm_horsepower oth_firm_weight oth_firm_width for_fuel), fe vce()

** Q4

mergersim init, nests(domestic class) price(princ) quantity(qu) marketsize(L) firm(firm)

xtivreg M_ls horsepower width height weight (princ fuel M_lsjh M_lshg= tot_height2 tot_horsepower2 tot_width2 tot_height3 tot_horsepower3 tot_width3 height width weight horsepower firm_height firm_weight firm_horsepower firm_width oth_firm_height oth_firm_horsepower oth_firm_weight oth_firm_width for_fuel), fe vce()

** high fuel firms and low fuel firms

bysort year firm: egen ave_fuel = mean(fuel)

bysort year firm: gen k = _n

sum ave_fuel if year==1999 & k==1

hist ave_fuel if year==1999 & k==1, bin(17)

graph box ave_fuel if year==1999 & k==1

list firm fuel if year==1999 & k==1

mergersim simulate if year == 1999, seller(29) buyer(24) detail

mergersim simulate if year == 1999 & country==3, seller(9) buyer(24) detail