##This script has the functions needed to accomplish various data handling or summary tasks
##Set working directory
dir<-setwd("~/Desktop/Rproject2021")

##Function 1 
#Country X and Y have different traditions for the delimiter in their data files. 
#This function converts all files in a directory with a space or tab-delimited data
#into .csv files

CSV.converter <-function(dir){
  setwd(dir)
  filelist = list.files(path = dir,pattern = ".txt")
  for (i in 1:length(filelist)){
    input<-filelist[i]
    output<-paste0(gsub("\\.txt$", "", input), ".csv")
    data = read.table(input, sep = "", header = TRUE)   
    write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
    file.remove(input)
  }
}
CSV.converter(dir)

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
    Yuninfected<-c(Yuninfected,1)
  }
}

#Find the number of infected individuals to uninfected
Ysum_inf<-sum(Yinfected)
Ysum_non<-sum(Yuninfected)
Ytotal<-Ysum_inf + Ysum_non
Yper_inf<-(Ysum_inf/Ytotal)*100


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

}




    
