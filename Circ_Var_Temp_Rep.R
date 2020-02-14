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


###Going to split data by the temperature 
main_dat_split  <- split(main_dat_f ,main_dat_f$Temp )

dat<- main_dat_split[[5]]

###PROOF OF CONCEPT###

dat_2 <- melt(dat, id.vars = c("Date","Rep","Temp"))

ggplot(dat_2, aes(x = Date, y = log(value+1), color = variable))+geom_line(size = 1)+facet_wrap(~Rep)+theme_bw()+
  scale_color_viridis(discrete=TRUE)


######################
###Make a circ-var ###
######################




###(This gets rid of the columns that you don't need-specifically the time columns )
tempo <-split(main_dat_split[[5]],main_dat_split[[5]]$Rep)

tempo_cutter <- subset(tempo[[3]],select = -c(Date, Rep, Temp))
##############################################################
###I'm assuming egg, larvae, pupae, adults ###################
##############################################################
#########################################################################################################
cvar=function(y,theta.start,theta.end){
  
  ###The theta.start and theta.end is provided by the user and is determined by the number of stages
  ### or subcompartments in my cases
  p=(theta.end-theta.start)/sum(theta.end-theta.start);
  theta.start=c(0,2*pi*cumsum(p[-length(p)]));
  theta.end=2*pi*cumsum(p) #normalize theta for missing stages
  
  if(length(dim(y))==0){
    y=matrix(y,nrow=1,ncol=length(y))}
  
  diff.theta=matrix(theta.end-theta.start,
                    nrow=dim(y)[1],
                    ncol=dim(y)[2],
                    byrow=T)
  
  cos.theta1=matrix(cos(theta.start),nrow=dim(y)[1],ncol=dim(y)[2],byrow=T);
  cos.theta2=matrix(cos(theta.end),nrow=dim(y)[1],ncol=dim(y)[2],byrow=T)
  
  sin.theta1=matrix(sin(theta.start),nrow=dim(y)[1],ncol=dim(y)[2],byrow=T);              
  sin.theta2=matrix(sin(theta.end),nrow=dim(y)[1],ncol=dim(y)[2],byrow=T)
  
  ###THIS IS HOW YOU CALCULATE THE Resultant Vector 
  R=sqrt(rowSums((y/diff.theta)*(sin.theta2-sin.theta1))^2+rowSums((y/diff.theta)*(cos.theta1-cos.theta2))^2)/rowSums(y)
  
  ###You then return the Circular Variance 
  return(1-R)
  
}






circ_var <- function(dat){

  ###You calculate the number of theta based on the column length
  theta = seq(0, 2*pi, length = 9) #Egg, Five instar, Pupae, combined adult + 1
  ###calculate the starting theta for each stage
  theta.start=theta[-9]
  theta.end=theta[-1];

  y <- as.matrix(dat)


  if(length(dim(y))==0){
  y=matrix(y,nrow=1,ncol=length(y))}

diff.theta=matrix(theta.end-theta.start,
                  nrow=dim(y)[1],
                  ncol=dim(y)[2],
                  byrow=T)

y=dat/matrix(colMeans(dat,na.rm=TRUE),
                          nrow=dim(dat)[1],
                          ncol=dim(dat)[2],byrow=T) #n

var.loc.NORM= na.omit(cvar(y,theta.start,theta.end))


###For simplicty, this function calculate the year and julian


return(var.loc.NORM)
}
a<- circ_var(tempo_cutter)
plot(a,type='l')
