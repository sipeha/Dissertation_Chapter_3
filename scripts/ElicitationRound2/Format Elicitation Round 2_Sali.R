library(gsheet);library(mc2d);library(reshape2);library(dplyr);library(stringi);library(ggplot2);library(tidyverse)
library(here); library(nimble)
source(here("scripts", "Functions for Elicitation Round2.R"))
Round2<-read.csv(here("data/Round 2 output/Round2Sali.csv"))

#need to change output to 99%
alphas<-betas<-dist<-numeric(length(Round2$X))
Round2<-cbind(Round2, alphas,betas,dist)

params<-c("Sali Adult Survival", 
          "Sali Juvenile Survival", 
          "Sali Fledgling Survival", "Sali Nest Success", 
          "Sali Nest Attempts",
          "Sali Fledglings per Nest")

# scenarios<-c("Scenario 1a", "Scenario 1b", "Scenario 1c",
#              "Scenario 2a", "Scenario 2b", "Scenario 2c",
#              "Scenario 3")
# sites<-c("HMU", "Refuge")

Round2$dist[Round2$Parameter==params[1]]<-"beta"
Round2$dist[Round2$Parameter==params[2]]<-"beta"
Round2$dist[Round2$Parameter==params[3]]<-"beta"
Round2$dist[Round2$Parameter==params[4]]<-"beta"
Round2$dist[Round2$Parameter==params[5]]<-"gamma"
Round2$dist[Round2$Parameter==params[6]]<-"gamma"

for(i in 1:length(Round2$dist)){
  if(Round2$dist[i]=="beta"){
    temp<-getparams(prob=T, paramvect=as.numeric(Round2[i,6:9]))
    Round2$alphas[i]<-temp$alpha
    Round2$betas[i]<-temp$beta
   }else if (Round2$dist[i]=="gamma"){
     temp<-getparams(prob=F, paramvect=as.numeric(Round2[i,6:9]))
     Round2$alphas[i]<-temp$alpha
     Round2$betas[i]<-temp$beta
  }
}

write.csv(Round2, here("data","Simulation","Saliparams.csv"))



