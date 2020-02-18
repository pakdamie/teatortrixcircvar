###Packages needed
library(WaveletComp)
library(ggplot2)
library(dplyr)
###Data file needed
load(here("Data","main_dat_f.RData"))

main_dat_split <- split(main_dat_f,main_dat_f$Temp)

for(i in seq(main_dat_split)){
temp = main_dat_split[[i]] 
 
temp$Date <- seq(1,nrow(temp))
temp2 <- melt(temp, id.vars =list("Date","Rep","Temp"))
ggplot_temp2 <- ggplot(temp2, aes( x= Date, y = value, color=variable))+geom_line(size = 1.3)+
  facet_grid(.~Rep) +
  ggtitle(temp2$Temp)+theme_bw()

print(ggplot_temp2)
}

melter <- function(dat){
  return(melt(dat, id.vars = list('Date',"Rep",'Temp')))
  
  
  
}

no =7
nv = 32
a = 2^seq(1, no + 1- 1/nv, by = 1/nv)

wavelet_maker <- function(dat){
  
  
  life_stages <- unique(dat$variable)
  par(mfrow=c(2,4))
  for (i in seq(1,length(life_stages))){
    
    tmp = subset(dat,dat$variable==life_stages[i])
    tmp$value[is.na(tmp$value)==TRUE] <- 0
    wfit=cwt(tmp$value,
             no, nv,plot=FALSE)
    wspec = Mod(wfit)
    image(x = seq(1,nrow(tmp)), wspec, y = a * 2)
    contour(x=seq(1,nrow(tmp)), wspec, y=a*2,
            zlim=c(mean(wspec), max(wspec)), add=TRUE)
    title(life_stages[i])
    
    
    
  }  
  title(unique(tmp$Temp), line = -1, outer = TRUE)
  
}

###Temperature 12
###I like Rep 1 though Rep 2 could work- 

Temp_12_ALL <- melter(subset(main_dat_f, main_dat_f$Temp == 12 & main_dat_f$Rep ==1))
ggplot(Temp_12_ALL, aes(x = Date, y = value, color = variable ))+geom_line(size =1)+
  facet_wrap(~Rep,scales = 'free')+theme_bw() + ggtitle("Temp-12")


ggplot(Temp_12_ALL, aes(x = Date, y = value, color = variable ))+geom_line(size =1)+
  facet_wrap(variable~Rep,scales = 'free')+theme_bw() + ggtitle("Temp-12")

Temp_12_1 <- melter (subset(main_dat_f, main_dat_f$Temp == 12 & main_dat_f$Rep==1))

ggplot(Temp_12_1, aes(x = Date, y = value, color = variable ))+geom_line(size =1)+
  facet_wrap(~variable,scales='free')+theme_bw()+ggtitle("Temp-12, Rep-1")


wavelet_maker(Temp_12_1)





###Temperature 12
###I like Rep 1 though Rep 2 could work- 


Temp_14_ALL <- melter(subset(main_dat_f, main_dat_f$Temp == 14))

ggplot(Temp_14_ALL, aes(x = Date, y = value, color = variable ))+geom_line(size =1)+
  facet_wrap(~Rep,scales = 'free')+theme_bw() + ggtitle("Temp-14")


ggplot(Temp_14_ALL, aes(x = Date, y = value, color = variable ))+geom_line(size =1)+
  facet_wrap(variable~Rep,scales = 'free')+theme_bw() + ggtitle("Temp-14")

Temp_14_1 <- melter (subset(main_dat_f, main_dat_f$Temp == 14 & main_dat_f$Rep==1))

ggplot(Temp_14_1, aes(x = Date, y = value, color = variable ))+geom_line(size =1)+
  facet_wrap(~variable,scales='free')+theme_bw()+ggtitle("Temp-14, Rep-1")


wavelet_maker(Temp_14_1)

###FOR EACH LIFE_STAGES


analyze.wavelet(T)
