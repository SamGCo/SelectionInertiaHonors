# Choice overload's effect on Adverse Selection and Inertia Honors Research

This is my senior honor's thesis project where I am investigating the relationship of choice overload has with inertia and adverse selection in the ACA exchanges.<br/>

CombinePlanFiles takes data from the Robert Woods Johnson Foundation of individual market plans data. This script combines the different years and drops plans that are not marketed on the exchanges.<br/>

raDemoScript takes data from the CMS individual market demographic data on the exchanges from each year between 2015 and 2021. This then cleans and merges this data to be used in RA-2.

RA-1 is a script that imports risk adjustment data to then create a dataframe with the states and the absolute value sum of risk adjustment transfers divided by two in each state. <br/>

RA-2 is a script that takes insurance filing data to create a dataframe with the number of markets per state and then the number of plans per state to then create a dataframe where I have each state with the average number of plans per market per state to then run a regression with the risk adjustment sum from the last script. 
                                                        
RAFinal calls each of these previous scripts and loads the appropriate packages to run the final regression.
