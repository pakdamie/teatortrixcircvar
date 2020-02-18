library(here)
library(ggplot2)
library(reshape2)
###CLEANING UP DATA HERE###
main_dat <- read.csv(here("Data","COMBINED.csv"),stringsAsFactors = FALSE)
main_dat$Date <- as.Date(main_dat$Date, format = '%m/%d/%Y')

###RENAMING THE COLUMNS TO MAKE IT EASIER

colnames(main_dat)[2:6] <- c("Instar1","Instar2","Instar3","Instar4","Instar5")
colnames(main_dat)[14] <- 'Eggs'
###COMBINING THE female.moth and male.moth

main_dat$combinedadult <- main_dat$Female.moth + main_dat$Male.Moth

###COMBINING THE NUMBERS OF EGGS (assume that we're going to get all the eggs insead
### of individual egg sac)

Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}


main_dat$Egg
for (r in seq(1,nrow(main_dat))){
  if(main_dat$Egg[r]==""){
    main_dat$Egg[r]=NA}}



LIST_EGGS<- NULL
for (r in seq(nrow(main_dat))){
  if(is.na(main_dat$Egg[r])==FALSE){
    LIST_EGGS[[r]]<-sum(as.numeric(Numextract(main_dat$Egg[r])))}
  else{
    LIST_EGGS[[r]] = as.numeric(main_dat$Egg[r])
  }
  
}
temp <- unlist(LIST_EGGS)

main_dat$Eggs <- temp
###The Dates, the life-stages, the reps, and the temperature
main_dat_f <- main_dat[,c(14,1:7,17,15,16)]


save(main_dat_f, file = "main_dat_f.Rdata")
