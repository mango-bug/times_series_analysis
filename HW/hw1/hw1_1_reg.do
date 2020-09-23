clear all

cd ../HW

import delimited hw1_1.csv

rename v1 date
rename v2 ex_rate
rename v3 ex_diff
rename v4 diff_lag

drop in 1

destring ex_rate ex_diff diff_lag, replace force

regress ex_diff diff_lag
