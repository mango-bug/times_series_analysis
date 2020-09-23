import excel "USD3MTD156N.xls", sheet("FRED Graph") cellrange(A11:B2981) firstrow clear

line USD3MTD156N observation_date, ytitle("Interest Rate (%)") ///
		xtitle("Year") title("3-Month USD LIBOR") ///
		aspectratio(0.618) ///
		xlabel(18263"2010" 18993"2012" 19724"2014" 20454"2016" 21185"2018" 21915"2020") ///
		note("Note: The data is based on the U.S. dollar and is not seasonally adjusted." ///
		"Source: Federal Reserve Bank of St. Louis," ///
		"retrieved from https://fred.stlouisfed.org/series/USD3MTD156N")
