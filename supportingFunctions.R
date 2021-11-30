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



##Function 3
#This function summarizes the number of screens run, percent of patients screened that were 
#infected, male vs. female patients, and the age distribution

##First load the data from the combined file
files<-read.csv("allData.csv", header = TRUE, sep = ",")
#Determine the number of screens, which is equal to the number of rows
number<-nrow(files)

#Percentage of infected patients 
infected<-c()
noninfected<-c()
for (i in 1:nrow(files)){
  if (files$marker01[i]==1){
    infected<-c(infected, 1)}
  else if (files$marker02[i]==1){
    infected<-c(infected,1)}
  else if (files$marker03[i]==1){
    infected<-c(infected, 1)}
  else if (files$marker04[i]==1){
    infected<-c(infected, 1)}
  else if (files$marker05[i]==1){
    infected<-c(infected, 1)}
  else if (files$marker06[i]==1){
    infected<-c(infected, 1)}
  else if (files$marker07[i]==1){
    infected<-c(infected, 1)}
  else if (files$marker08[i]==1){
    infected<-c(infected, 1)}
  else if (files$marker09[i]==1){
    infected<-c(infected, 1)}
  else if (files$marker10[i]==1){
    infected<-c(infected, 1)}
  else{
    noninfected<-c(noninfected,1)
  }
}
#Find the number of infected individuals to noninfected
sum_inf<-length(infected)
sum_non<-length(noninfected)
total<-sum_inf + sum_non
per_inf<-(sum_inf/total)*100

#Determine the number of males to females 
female<-c()
male<-c()
for (i in 1:nrow(files)){
  if (files$gender[i]=="female"){
    female<-c(female,1)
    }else{
      male<-c(male,1)
  }
}
tot_fem<-length(female)
tot_mal<-length(male)

#Determine the age distribution 




    
