##This script analyzes the data for the disease for Country X and Country Y
##This script will use source to use the custom functions, compile data into one .csv
##and process the data. 

library(ggplot2)
library(cowplot)

##Use functions from supportingFunctions.R
summary("allData.csv")

##Create plots to look at data

#Number of Screens Run 


#Percent Patients infected
ggplot(data = Percent, aes(x = Percent)) + geom_bar()


##In which country (X or Y) did the disease outbreak likely begin?


##If Country Y develops a vaccine for the disease, is it likely to work for the 
##citizens of Country X?

#If Country Y developed a vaccine for the disease, it would not likely work for the 
#citizens of Country X because the different markers that are regularly present in infections.