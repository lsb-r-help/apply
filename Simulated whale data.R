rm(list=ls())
library(data.table)
library(truncnorm)
Year<-seq(1900, 1985)

Species<-c("Blue", "Sperm", "Minke")

Individual<-c(1:50)

whale.dat<-expand.grid(Year, Species, Individual)
names(whale.dat)<-c("Year", "Species", "Individual")
whale.dat$Length.code<-sample(c(1,2,3), nrow(whale.dat), replace=T)
whale.dat$Length<-NA

whale.sizes<-data.frame(Species=Species, 
                        mean.size=c(33, 12, 8.5))
#k<-split(whale.dat, list(whale.dat$Species, whale.dat$Length.code))[[3]]
fin.dd<-rbindlist(lapply(split(whale.dat, list(whale.dat$Species, whale.dat$Length.code)), function(k){
  ##if length.code==1, then make it cm
  ##if length.code==2 then leave it as it is (meters)
  ##if legnth.code==3 then make it feet
  y <- rtruncnorm(length(k$Year), 
                  a=2,
                  b=Inf,
                  mean = seq(from=subset(whale.sizes, Species==k$Species[1])$mean.size, 
                          to=subset(whale.sizes, Species==k$Species[1])$mean.size/runif(1, min=1.1, max=1.5),
                          length.out = length(k$Year)),
               sd = 1)
  k<-k[order(k$Year),]
  if(k$Length.code[1]==1)k$Length<-y*100
  if(k$Length.code[1]==2)k$Length<-y
  if(k$Length.code[1]==3)k$Length<-round(y*3, 0)
  return(k)
}))

fin.dd$Individual<-NULL
save(fin.dd, file='~/Desktop/Work/Teaching/R/Apply functions/whale data.RData')

ggplot(fin.dd, aes(x=Year, y=Length))+geom_point()+facet_wrap(Species~Length.code, scales="free")+geom_smooth()
