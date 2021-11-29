##This script has the functions needed to accomplish various data handling or summary tasks
##Set working directory
directory<-setwd("~/Desktop/Rproject2021")

##Function 1 
#Country X and Y have different traditions for the delimiter in their data files. 
#This function converts all files in a directory with a space or tab-delimited data
#into .csv files


##Function 2
#This function compiles the data from the original files and also adds two columns
#for day of the year and the country. The user should be able to choose whether they want 
#to remove NAs, include NAs in the compiled data but be warned, or include NAs without warning

compile<-function(){
  compiled<-data.frame()
  for (i in dir){
    
  }
}

##Function 3
#This function summarizes the number of screens run, percent of patients screened that were 
#infected, male vs. female patients, and the age distribution

summary<-function(){
  
}

