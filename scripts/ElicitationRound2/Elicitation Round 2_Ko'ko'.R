library(gsheet);library(mc2d);library(reshape2);library(dplyr);library(stringi);library(ggplot2);library(tidyverse)
library(here);library(readxl) #call in locally
source(here("scripts", "Functions for Elicitation Round2.R"))


n.experts<-5
n.params<-5
n.alts<-7

#Guam X/Amy
E1list<-list(idE1=as.data.frame(read_excel(here("data/Elicitation sheets/Amy Round 1.xlsx"), "Instructions")[15,-1])[1,],
             P1E1=as.data.frame(read_excel(here("data/Elicitation sheets/Amy Round 2.xlsx"), "Koko Adult Survival")[,2:5]),
             P2E1=as.data.frame(read_excel(here("data/Elicitation sheets/Amy Round 2.xlsx"), " Koko Juvenile Survival")[,2:5]),
             P3E1=as.data.frame(read_excel(here("data/Elicitation sheets/Amy Round 2.xlsx"), "Koko Nest Success")[,2:5]),
             P4E1=as.data.frame(read_excel(here("data/Elicitation sheets/Amy Round 1.xlsx"), "Koko Nest Attempts")[1:14,2:5]),
             P5E1=as.data.frame(read_excel(here("data/Elicitation sheets/Amy Round 1.xlsx"), "Koko Hatchlings per Nest")[1:14,2:5]))

#Expert 1/Eben
E2list<-list(idE2=as.data.frame(read_excel(here("data/Elicitation sheets/Eben Round 1.xlsx"), "Instructions")[15,-1])[1,],
             P1E2=as.data.frame(read_excel(here("data/Elicitation sheets/Eben Round 2.xlsx"), "Koko Adult Survival")[,2:5]),
             P2E2=as.data.frame(read_excel(here("data/Elicitation sheets/Eben Round 2.xlsx"), " Koko Juvenile Survival")[,2:5]),
             P3E2=as.data.frame(read_excel(here("data/Elicitation sheets/Eben Round 2.xlsx"), "Koko Nest Success")[,2:5]),
             P4E2=as.data.frame(read_excel(here("data/Elicitation sheets/Eben Round 2.xlsx"), "Koko Nest Attempts")[,2:5]),
             P5E2=as.data.frame(read_excel(here("data/Elicitation sheets/Eben Round 2.xlsx"), "Koko Hatchlings per Nest")[,2:5])
)

#aircon/ Martin
E3list<-list(idE3=as.data.frame(read_excel(here("data/Elicitation sheets/Martin Round 1.xlsx"), "Instructions")[15,-1])[1,],
             P1E3=as.data.frame(read_excel(here("data/Elicitation sheets/Martin Round 2.xlsx"), "Koko Adult Survival")[,2:5]),
             P2E3=as.data.frame(read_excel(here("data/Elicitation sheets/Martin Round 2.xlsx"), " Koko Juvenile Survival")[,2:5]),
             P3E3=as.data.frame(read_excel(here("data/Elicitation sheets/Martin Round 2.xlsx"), "Koko Nest Success")[,2:5]),
             P4E3=as.data.frame(read_excel(here("data/Elicitation sheets/Martin Round 2.xlsx"), "Koko Nest Attempts")[,2:5]),
             P5E3=as.data.frame(read_excel(here("data/Elicitation sheets/Martin Round 2.xlsx"), "Koko Hatchlings per Nest")[,2:5])
)

#CDEDBDiis/Will
E4list<-list(idE4=as.data.frame(read_excel(here("data/Elicitation sheets/Will Round 1.xlsx"), "Instructions")[15,-1])[1,],
             P1E4=as.data.frame(read_excel(here("data/Elicitation sheets/Will Round 2.xlsx"), "Koko Adult Survival")[,2:5]),
             P2E4=as.data.frame(read_excel(here("data/Elicitation sheets/Will Round 2.xlsx"), " Koko Juvenile Survival")[,2:5]),
             P3E4=as.data.frame(read_excel(here("data/Elicitation sheets/Will Round 2.xlsx"), "Koko Nest Success")[,2:5]),
             P4E4=as.data.frame(read_excel(here("data/Elicitation sheets/Will Round 2.xlsx"), "Koko Nest Attempts")[,2:5]),
             P5E4=as.data.frame(read_excel(here("data/Elicitation sheets/Will Round 2.xlsx"), "Koko Hatchlings per Nest")[,2:5])
)

#96929/Haldre 
E5list<-list(idE5=as.data.frame(read_excel(here("data/Elicitation sheets/Haldre Round 1.xlsx"), "Instructions")[15,-1])[1,],
             P1E5=as.data.frame(read_excel(here("data/Elicitation sheets/Haldre Round 2.xlsx"), "Koko Adult Survival")[,2:5]),
             P2E5=as.data.frame(read_excel(here("data/Elicitation sheets/Haldre Round 2.xlsx"), " Koko Juvenile Survival")[,2:5]),
             P3E5=as.data.frame(read_excel(here("data/Elicitation sheets/Haldre Round 2.xlsx"), "Koko Nest Success")[,2:5]),
             P4E5=as.data.frame(read_excel(here("data/Elicitation sheets/Haldre Round 2.xlsx"), "Koko Nest Attempts")[,2:5]),
             P5E5=as.data.frame(read_excel(here("data/Elicitation sheets/Haldre Round 2.xlsx"), "Koko Hatchlings per Nest")[,2:5])
)

P1list<-list(P1E1=E1list$P1E1, P1E2=E2list$P1E2, P1E3=E3list$P1E3, 
             P1E4=E4list$P1E4, 
             P1E5=E5list$P1E5)
P2list<-list(P2E1=E1list$P2E1, P2E2=E2list$P2E2, P2E3=E3list$P2E3,
             P2E4=E4list$P2E4,
             P2E5=E5list$P2E5)
P3list<-list(P3E1=E1list$P3E1, P3E2=E2list$P3E2, P3E3=E3list$P3E3, 
             P3E4=E4list$P3E4,
             P3E5=E5list$P3E5)
P4list<-list(P4E1=E1list$P4E1, P4E2=E2list$P4E2, P4E3=E3list$P4E3,
             P4E4=E4list$P4E4,
             P4E5=E5list$P4E5)
P5list<-list(P5E1=E1list$P5E1, P5E2=E2list$P5E2, P5E3=E3list$P5E3, 
             P5E4=E4list$P5E4,
             P5E5=E5list$P5E5)

for(i in 1:n.experts){
  colnames(P1list[[i]])<-c("best","min","max","conf")
  colnames(P2list[[i]])<-c("best","min","max","conf")
  colnames(P3list[[i]])<-c("best","min","max","conf")
  colnames(P4list[[i]])<-c("best","min","max","conf")
  colnames(P5list[[i]])<-c("best","min","max","conf")
}


params<-c("Ko'ko' Adult Survival", 
          "Ko'ko' Juvenile Survival", 
          "Ko'ko' Nest Success", 
          "Ko'ko' Nest Attempts",
          "Ko'ko' Hatchlings per Nest")

scenarios<-c("Scenario 1a", "Scenario 1b", "Scenario 1c",
             "Scenario 2a", "Scenario 2b", "Scenario 2c",
             "Scenario 3")
sites<-c("HMU", "Refuge")


P1listC<-datacleanfn(P1list, num_ex=n.experts, nqs=length(P1list[[1]][,1]), prob=T)
P2listC<-datacleanfn(P2list, num_ex=n.experts, nqs=length(P2list[[1]][,1]), prob=T)
P3listC<-datacleanfn(P3list, num_ex=n.experts, nqs=length(P3list[[1]][,1]), prob=T)
P4listC<-datacleanfn(P4list, num_ex=n.experts, nqs=length(P4list[[1]][,1]), prob=F)
P5listC<-datacleanfn(P5list, num_ex=n.experts, nqs=length(P5list[[1]][,1]), prob=F)


R2dat<-as.data.frame(cbind(expert=c(rep(c(E1list$idE1, E2list$idE2, E3list$idE3, 
                                          E4list$idE4, E5list$idE5), each=14)),
                           parameter=c(rep(c("Ko'ko' Adult Survival"), 14*n.experts)),
                           sceanrio=rep(c(rep(scenarios, times=2)), n.experts),
                           site = rep(c(rep(sites, each=7)),n.experts),
                           rbind(P1listC$P1E1, P1listC$P1E2, P1listC$P1E3,
                                 P1listC$P1E4, P1listC$P1E5)))

for(i in 2:5){
  temp<-as.data.frame(cbind(expert=c(rep(c(E1list$idE1, E2list$idE2, E3list$idE3,
                                           E4list$idE4, E5list$idE5), each=14)),
                            parameter=c(rep(c(params[i]), 14*n.experts)),
                            sceanrio=rep(c(rep(scenarios, times=2)), n.experts),
                            site = rep(c(rep(sites, each=7)),n.experts),
                            rbind(eval(parse(text=paste0("P",i,"listC$P",i,"E1"))),
                                  eval(parse(text=paste0("P",i,"listC$P",i,"E2"))),
                                  eval(parse(text=paste0("P",i,"listC$P",i,"E3"))),
                                  eval(parse(text=paste0("P",i,"listC$P",i,"E4"))),
                                  eval(parse(text=paste0("P",i,"listC$P",i,"E5"))))))
  R2dat<-rbind(R2dat, temp)
  
}
round2<-rep("Round 2", length(R2dat[,1]))
R2datout<-cbind(R2dat, round2)
colnames(R2datout)<-c("Expert", "Parameter", "Scenario","Site", "Min","Best","Max","Conf","Round")
R2datout<-as.data.frame(R2datout)

write.csv(R2datout, file=here("data/Round 2 output/Round2Ko'ko'.csv"))

