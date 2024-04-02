### Readme for version 0.1 of e61_Tax-Sim
# Last update: 8-March-2024

e61-Tax-Sim is an R based tax calculator for Australia. Policy parameters for each quarter are available, and real income profiles can be compared for hypothetical households.

File structure:
	Main Folder

	The calc_XXX files represent a series of functions that will be called in the process of constructing relevant output measures. These individual modules capture a specific component of the tax and transfer system.
		calc_main_benefit.R:  XXX
		calc_benefit_gross.R:  XXX
		calc_benefit_abated.R:  XXX
	TAXBEN_calc.R represents the main file that runs the tax calculator for a given set of hypothetic assumptions about the individual of interest.
	

	Policy files Folder

	This folder contains the policy parameters for each Quarter of the year in Australia. At present only Q1 and Q4 2023 are available.