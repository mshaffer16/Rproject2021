##This script has the functions needed to accomplish various data handling or summary tasks
##Set working directory
dir<-setwd("~/Desktop/Rproject2021")

##Function 1 
#Country X and Y have different traditions for the delimiter in their data files. 
#This function converts all files in a directory with a space or tab-delimited data
#into .csv files. The path input must be directly to the files that are being converted.

CSV.converter <-function(dir){
  filelist = list.files(path = dir,pattern = ".txt")
  for (i in 1:length(filelist)){
    input<-filelist[i]
    output<-paste0(gsub("\\.txt$", "", input), ".csv")
    data<-read.table(input, sep = "", header = TRUE)   
    write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
    file.remove(input)
  }
}

##Function 2
#This function compiles the data from the original files and also adds two columns
#for day of the year and the country. The user should be able to choose whether they want 
#to remove NAs, include NAs in the compiled data but be warned, or include NAs without warning



##Function 3
#This function summarizes the number of screens run, percent of patients screened that were 
#infected, male vs. female patients, and the age distribution

summary<-function(files){
##First load the data from the combined file
files<-read.csv("allData.csv", header = TRUE, sep = ",")
#Determine the number of screens, which is equal to the number of rows
number<-nrow(files)
#Determine the total number for each country
fileX<-subset(files, country == "X")
Xnumber<-nrow(fileX)

fileY<-subset(files, country == "Y")
Ynumber<-nrow(fileY)

#Overall Percentage of infected patients 
infected<-c()
uninfected<-c()
for (i in 1:nrow(files)){
  if (files$marker01[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker02[i]==1){
    infected<-c(infected,1)
    uninfected<-c(uninfected,0)}
  else if (files$marker03[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker04[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker05[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker06[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker07[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker08[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker09[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker10[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else{
    infected<-c(infected,0)
    uninfected<-c(uninfected,1)
  }
}

#Find the number of infected individuals to uninfected
sum_inf<-sum(infected)
sum_non<-sum(uninfected)
total<-sum_inf + sum_non
per_inf<-(sum_inf/total)*100

#Percent infected in Country X
Xinfected<-c()
Xuninfected<-c()
fileX<-subset(files, country == "X")
for (i in 1:nrow(fileX)){
  if (fileX$marker01[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker02[i]==1 ){
    Xinfected<-c(Xinfected,1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker03[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker04[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker05[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker06[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker07[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker08[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker09[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker10[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else{
    Xinfected<-c(Xinfected,0)
    Xuninfected<-c(Xuninfected,1)
  }
}

#Find the number of infected individuals to uninfected
Xsum_inf<-sum(Xinfected)
Xsum_non<-sum(Xuninfected)
Xtotal<-Xsum_inf + Xsum_non
Xper_inf<-(Xsum_inf/Xtotal)*100

#Percent in Country Y
Yinfected<-c()
Yuninfected<-c()
fileY<-subset(files, country == "Y")
for (i in 1:nrow(fileY)){
  if (fileY$marker01[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker02[i]==1 ){
    Yinfected<-c(Yinfected,1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker03[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker04[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker05[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker06[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker07[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker08[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker09[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker10[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else{
    Yinfected<-c(Yinfected,0)
    Yuninfected<-c(Yuninfected,1)
  }
}

#Find the number of infected individuals to uninfected
Ysum_inf<-sum(Yinfected)
Ysum_non<-sum(Yuninfected)
Ytotal<-Ysum_inf + Ysum_non
Yper_inf<-(Ysum_inf/Ytotal)*100

#Make a data frame of the date with infected and uninfected

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

#Males to Females for Country X
Xfemale<-c()
Xmale<-c()
for (i in 1:nrow(fileX)){
  if (fileX$gender[i]=="female"){
    Xfemale<-c(Xfemale,1)
  }else{
    Xmale<-c(Xmale,1)
  }
}
Xtot_fem<-length(Xfemale)
Xtot_mal<-length(Xmale)

#Males to Females for Country Y
Yfemale<-c()
Ymale<-c()
for (i in 1:nrow(fileY)){
  if (fileY$gender[i]=="female"){
    Yfemale<-c(Yfemale,1)
  }else{
    Ymale<-c(Ymale,1)
  }
}
Ytot_fem<-length(Yfemale)
Ytot_mal<-length(Ymale)

#Determine the age distribution 


#Output all data to a data frame


}

##Function 4
#This function will determine the markers that are present in each country for the infections

hetergenity<-function(files){
  ##First load the data from the combined file
  files<-read.csv("allData.csv", header = TRUE, sep = ",")
  #Subset files by country
  fileX<-subset(files, country == "X")
  fileY<-subset(files, country == "Y")
  
  m1x<-sum(fileX$marker01)
  m2x<-sum(fileX$marker02)
  m3x<-sum(fileX$marker03)
  m4x<-sum(fileX$marker04)
  m5x<-sum(fileX$marker05)
  m6x<-sum(fileX$marker06)
  m7x<-sum(fileX$marker07)
  m8x<-sum(fileX$marker08)
  m9x<-sum(fileX$marker09)
  m10x<-sum(fileX$marker10)
  markerX<-c(m1x, m2x, m3x, m4x, m5x, m6x, m7x, m8x, m9x, m10x)
  
  m1y<-sum(fileY$marker01)
  m2y<-sum(fileY$marker02)
  m3y<-sum(fileY$marker03)
  m4y<-sum(fileY$marker04)
  m5y<-sum(fileY$marker05)
  m6y<-sum(fileY$marker06)
  m7y<-sum(fileY$marker07)
  m8y<-sum(fileY$marker08)
  m9y<-sum(fileY$marker09)
  m10y<-sum(fileY$marker10)
  markerY<-c(m1y, m2y, m3y, m4y, m5y, m6y, m7y, m8y, m9y, m10y)
  
  Xdays<-fileX$dayofYear
  Ydays<-fileY$dayofYear
  
  markerdf<-data.frame (markerX, markerY)
}



    
