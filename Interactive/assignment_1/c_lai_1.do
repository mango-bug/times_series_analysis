import excel "USD3MTD156N.xls", sheet("FRED Graph") cellrange(A11:B2981) firstrow clear

line USD3MTD156N observation_date, ytitle("3-Month LIBOR, based on USD (%)") ///
		xtitle("Date") title("Daily 3-Month LIBOR (not seasonally adjusted)") ///
		aspectratio(0.618) ///
		xlabel(17532"2008" 18263"2010" 18993"2012" 19724"2014" 20454"2016" 21185"2018" 21915"2020")
