library(gsheet);library(mc2d);library(reshape2);library(dplyr);library(stringi);library(ggplot2);library(tidyverse)
library(here); library(nimble)
source(here("scripts", "Functions for Elicitation Round2.R"))

Round2<-read.csv(here("data/Round 2 output/Round2Ko'ko'.csv"))

#need to change output to 99%
alphas<-betas<-dist<-numeric(length(Round2$X))
Round2<-cbind(Round2, alphas,betas,dist)

params<-c("Ko'ko' Adult Survival", 
          "Ko'ko' Juvenile Survival", 
          "Ko'ko' Nest Success", 
          "Ko'ko' Nest Attempts",
          "Ko'ko' Hatchlings per Nest")

# scenarios<-c("Scenario 1a", "Scenario 1b", "Scenario 1c",
#              "Scenario 2a", "Scenario 2b", "Scenario 2c",
#              "Scenario 3")
# sites<-c("HMU", "Refuge")

Round2$dist[Round2$Parameter==params[1]]<-"beta"
Round2$dist[Round2$Parameter==params[2]]<-"beta"
Round2$dist[Round2$Parameter==params[3]]<-"beta"
Round2$dist[Round2$Parameter==params[4]]<-"gamma"
Round2$dist[Round2$Parameter==params[5]]<-"gamma"

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

write.csv(Round2, here("data","Simulation","Ko'ko'params.csv"))

#####make plots
#make a table of standardized values

#by parameter and alternative across experts - facet grid for alternatives
# byexpertparamalt<-function(parameter, prob){
#   
#   if(prob==T){
#     P11list<-list(E1=Round1 %>% filter(Round1$Expert=="GuamX" & Round1$Parameter==parameter),
#                   E2=Round1 %>% filter(Round1$Expert=="Expert 1" & Round1$Parameter==parameter),
#                   E3=Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter==parameter),
#                   E4=Round1 %>% filter(Round1$Expert=="CDEDBDiis" & Round1$Parameter==parameter),
#                   E5=Round1 %>% filter(Round1$Expert=="96929" & Round1$Parameter==parameter ))
#     
#     DispAR1<-makerands12(P11list,nalts=length(P11list$E1[,1]), num_ex=5)
#     #sites<-c("HMU", "Refuge")
#     sites<-c("HMU", "Refuge", "Anao", "North Finegayan")
#     for(i in 1:length(sites)){
#       
#       XX<-ggplot((DispAR1[[i]]), aes(x=ID, y=value, fill=ID))+geom_violin(trim=T, scale="width")+
#         facet_wrap(~factor(scenario,levels=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
#                                              "Scenario 2a", "Scenario 2b", "Scenario 2c",
#                                              "Scenario 3")), scale="fixed")+
#         xlab("")+ylab("Probability")+labs(title=paste0(parameter, " at ",
#                                                        sites[i]),
#                                           subtitle="Elicited values standardized to 90%",fill="",
#                                           caption="Round 1")+
#         theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="right")+
#         expand_limits(y=c(0,1))
#       ggsave(XX, filename = here("results","Round 1 Figures",
#                                  paste0(parameter,"_",
#                                         sites[i],"_facet.png")),
#              width = 8, height = 6, dpi = 300, units = "in", device='png')
#     }
#     # for(i in 1:length(P11list$E1[,1])){
#     #   
#     #   XX<-ggplot((DispAR1[[i]]), aes(x=ID, y=value, fill=ID))+geom_violin(trim=T, scale="width")+
#     #                                       "Scenario 2a", "Scenario 2b", "Scenario 2c",
#     #                                       "Scenario 3")), scale="free")+
#     #     xlab("")+ylab("Probability")+labs(title=paste0(parameter,", ", P11list$E1$Scenario[i], " at ",
#     #                                                    P11list$E1$Site[i]),
#     #                                       subtitle="Elicited values standardized to 90%",fill="",
#     #                                       caption="Round 1")+
#     #     theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="right")+
#     #     expand_limits(y=c(0,1))
#     #   ggsave(XX, filename = here("results","Round 1 Figures",
#     #                              paste0(parameter, "_",  P11list$E1$Scenario[i],"_",
#     #                                     P11list$E1$Site[i],".png")),
#     #          width = 8, height = 6, dpi = 300, units = "in", device='png')
#     # }
#   }else{
#     P11list<-list(E1=Round1 %>% filter(Round1$Expert=="GuamX" & Round1$Parameter==parameter),
#                   E2=Round1 %>% filter(Round1$Expert=="Expert 1" & Round1$Parameter==parameter),
#                   E3=Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter==parameter),
#                   E4=Round1 %>% filter(Round1$Expert=="CDEDBDiis" & Round1$Parameter==parameter),
#                   E5=Round1 %>% filter(Round1$Expert=="96929" & Round1$Parameter==parameter))
#     
#     #xxfpn<-Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter=="Sali Fledglings per Nest")
#     #xx<-Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter=="Sali Nest Attempts")
#     #P11list$E3[,6:8]<-xxfpn[,6:8]/xx[,6:8]
#     #Change number of experts below:
#     #TODO
#     DispAR1<-makerands12_gamma(P11list,nalts=length(P11list$E1[,1]), num_ex=5)
#     sites<-c("HMU", "Refuge")
#     #sites<-c("HMU", "Refuge", "Anao", "North Finegayan")
#     maxs<-numeric(length(sites))
#     for(i in 1:length(maxs)){
#       maxs[i]<-max(DispAR1[[i]]$value)
#     }
#     maxy<-max(maxs)
#     
#     for(i in 1:length(sites)){
#       
#       XX<-ggplot((DispAR1[[i]]), aes(x=ID, y=value, fill=ID))+geom_violin(trim=T, scale="width")+
#         facet_wrap(~factor(scenario,levels=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
#                                              "Scenario 2a", "Scenario 2b", "Scenario 2c",
#                                              "Scenario 3")), scales="free")+ #???
#         coord_cartesian(ylim = c(0,min(maxy,10)))+
#         xlab("")+ylab("Probability")+labs(title=paste0(parameter, " at ",
#                                                        sites[i]),
#                                           subtitle="Elicited values standardized to 90%",fill="",
#                                           caption="Round 1")+
#         theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="right")#+
#       #expand_limits(y=c(0,1))
#       ggsave(XX, filename = here("results","Round 1 Figures",
#                                  paste0(parameter,"_",
#                                         sites[i],"_facet.png")),
#              width = 8, height = 6, dpi = 300, units = "in", device='png')
#     }
#     # for(i in 1:length(P11list$E1[,1])){
#     #   
#     #   XX<-ggplot((DispAR1[[i]]), aes(x=ID, y=value, fill=ID))+geom_violin(trim=T, scale="width")+
#     #     xlab("")+ylab("Number")+labs(title=paste0(parameter,", ", P11list$E1$Scenario[i], " at ",
#     #                                                    P11list$E1$Site[i]),
#     #                                       subtitle="Elicited values standardized to 90%",fill="",
#     #                                       caption="Round 1")+
#     #     theme(axis.text.x=element_blank(), 
#     #           axis.ticks.x=element_blank(), legend.position="right")+ylim(c(0,min(maxy,15)))
#     #   ggsave(XX, filename = here("results","Round 1 Figures",
#     #                              paste0(parameter, "_",  P11list$E1$Scenario[i],"_",
#     #                                     P11list$E1$Site[i],"MartinAdjusted.png")),
#     #          width = 8, height = 6, dpi = 300, units = "in", device='png')
#     # }
#   }
#   
# }
# byexpertparamalt(parameter="Sali Adult Survival", prob=T)
# #byexpertparamalt(parameter="Ko'ko' Adult Survival", prob=T)
# byexpertparamalt(parameter="Sali Juvenile Survival", prob=T)
# #byexpertparamalt(parameter="Ko'ko' Juvenile Survival", prob=T)
# byexpertparamalt(parameter="Sali Fledgling Survival", prob=T)
# byexpertparamalt(parameter="Sali Nest Success", prob=T)
# #byexpertparamalt(parameter="Ko'ko' Nest Success", prob=T)
# 
# byexpertparamalt(parameter="Sali Nest Attempts", prob=F)
# #byexpertparamalt(parameter="Ko'ko' Nest Attempts", prob=F)
# byexpertparamalt(parameter="Sali Fledglings per Nest", prob=F)
# #byexpertparamalt(parameter="Ko'ko' Hatchlings per Nest", prob=F)
# 
# ##########################
# #by parameter, across alternative
# #########################
# paramall<-function(parameter, site, prob){
#   
#   if(prob==T){
#     P12listCom<-list(E1=Round1 %>% filter(Round1$Expert=="GuamX" & Round1$Parameter==parameter,
#                                           Round1$Site==site),
#                      E2=Round1 %>% filter(Round1$Expert=="Expert 1" & Round1$Parameter==parameter,
#                                           Round1$Site==site),
#                      E3=Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter==parameter,
#                                           Round1$Site==site),
#                      E4=Round1 %>% filter(Round1$Expert=="CDEDBDiis" & Round1$Parameter==parameter  &
#                                             Round1$Site==site),
#                      E5=Round1 %>% filter(Round1$Expert=="96929" & Round1$Parameter==parameter  &
#                                             Round1$Site==site))
#     
#     parmout<-makerandsCom(P12listCom, nalts=length(P12listCom$E1[,1]), num_ex=5)
#     
#     
#     XX2<-ggplot(parmout, aes(x=ID, y=value, fill=ID))+geom_violin(trim=T, scale="width")+
#       xlab("")+ylab("Probability")+labs(title=paste0(parameter, " at ", site),
#                                         subtitle="Elicited values standardized to 90%",fill="",
#                                         caption="Round 1")+
#       theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#             legend.position="right")+ylim(c(0,1))
#     ggsave(XX2, filename = here("results","Round 1 Figures",
#                                 paste0(parameter, "_All Scenarios_",site, ".png")),
#            width = 8, height = 6, dpi = 300, units = "in", device='png')
#   }else{
#     
#     P12listCom<-list(E1=Round1 %>% filter(Round1$Expert=="GuamX" & Round1$Parameter==parameter,
#                                           Round1$Site==site),
#                      E2=Round1 %>% filter(Round1$Expert=="Expert 1" & Round1$Parameter==parameter,
#                                           Round1$Site==site),
#                      E3=Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter==parameter,
#                                           Round1$Site==site),
#                      E4=Round1 %>% filter(Round1$Expert=="CDEDBDiis" & Round1$Parameter==parameter  &
#                                             Round1$Site==site),
#                      E5=Round1 %>% filter(Round1$Expert=="96929" & Round1$Parameter==parameter  &
#                                             Round1$Site==site))
#     #xxfpn<-Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter=="Sali Fledglings per Nest"
#     #                         & Round1$Site==site)
#     #xx<-Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter=="Sali Nest Attempts" &
#     #                        Round1$Site==site)
#     #P12listCom$E3[,6:8]<-xxfpn[,6:8]/xx[,6:8]
#     parmout<-makerandsCom_gamma(P12listCom, nalts=length(P12listCom$E1[,1]), num_ex=5)
#     
#     
#     XX2<-ggplot(parmout, aes(x=ID, y=value, fill=ID))+geom_violin(trim=T, scale="width")+
#       xlab("")+ylab("Probability")+labs(title=paste0(parameter, " at ", site),
#                                         subtitle="Elicited values standardized to 90%",fill="",
#                                         caption="Round 1")+
#       theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#             legend.position="right")+ylim(c(0,10))
#     ggsave(XX2, filename = here("results","Round 1 Figures",
#                                 paste0(parameter, "_All Scenarios_",site, ".png")),
#            width = 8, height = 6, dpi = 300, units = "in", device='png')
#     
#   }
# }
# 
# sites<-c("HMU", "Refuge", "Anao", "North Finegayan")
# for(i in 1:4){
#   #paramall(parameter="Sali Adult Survival", site=sites[i], prob=T)
#   #paramall(parameter="Ko'ko' Adult Survival", site=sites[i], prob=T)
#   #paramall(parameter="Sali Juvenile Survival", site=sites[i], prob=T)
#   #paramall(parameter="Ko'ko' Juvenile Survival", site=sites[i], prob=T)
#   #paramall(parameter="Sali Fledgling Survival", site=sites[i], prob=T)
#   #paramall(parameter="Sali Nest Success", site=sites[i], prob=T)
#   #paramall(parameter="Ko'ko' Nest Success", site=sites[i], prob=T)
#   #paramall(parameter="Sali Nest Attempts", site=sites[i], prob=F)
#   #paramall(parameter="Ko'ko' Nest Attempts", site=sites[i], prob=F)
#   paramall(parameter="Sali Fledglings per Nest", site=sites[i], prob=F)
#   #paramall(parameter="Ko'ko' Hatchlings per Nest", site=sites[i], prob=F)
# }
# 
# 
# 
# paramallSite<-function(parameter, prob){
#   
#   if(prob==T){
#     P12listCom<-list(E1=Round1 %>% filter(Round1$Expert=="GuamX" & Round1$Parameter==parameter),
#                      #Round1$Scenario==scenario),
#                      E2=Round1 %>% filter(Round1$Expert=="Expert 1" & Round1$Parameter==parameter),
#                      #Round1$Scenario==scenario),
#                      E3=Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter==parameter),
#                      #Round1$Scenario==scenario),
#                      E4=Round1 %>% filter(Round1$Expert=="CDEDBDiis" & Round1$Parameter==parameter),  #&
#                      #Round1$Scenario==scenario),
#                      E5=Round1 %>% filter(Round1$Expert=="96929" & Round1$Parameter==parameter))  #&
#     #Round1$Scenario==scenario))
#     
#     parmout<-makerandsComSite(P12listCom, nalts=length(P12listCom$E1[,1]), num_ex=5)
#     
#     parmout$ID[which(parmout$ID=="HMU")]<-"1-HMU"
#     parmout$ID[which(parmout$ID=="Refuge")]<-"2-Refuge"
#     # parmout$ID[which(parmout$ID=="Anao")]<-"3-Anao"
#     # parmout$ID[which(parmout$ID=="North Finegayan")]<-"4-North Finegayan"
#     XX2<-ggplot(parmout, aes(x=ID, y=value, fill=ID))+geom_violin(trim=T, scale="width")+
#       facet_wrap(~factor(scenario,levels=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
#                                            "Scenario 2a", "Scenario 2b", "Scenario 2c",
#                                            "Scenario 3")), scale="fixed")+
#       xlab("")+ylab("Probability")+labs(title=paste0(parameter),
#                                         subtitle="Elicited values standardized to 90%",fill="",
#                                         caption="Round 1")+
#       theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#             legend.position="right")+ylim(c(0,1))
#     ggsave(XX2, filename = here("results","Round 1 Figures",
#                                 paste0(parameter, "_2 Sites_facet.png")),
#            width = 8, height = 6, dpi = 300, units = "in", device='png')
#   }else{
#     
#     P12listCom<-list(E1=Round1 %>% filter(Round1$Expert=="GuamX" & Round1$Parameter==parameter),
#                      E2=Round1 %>% filter(Round1$Expert=="Expert 1" & Round1$Parameter==parameter),
#                      E3=Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter==parameter),
#                      E4=Round1 %>% filter(Round1$Expert=="CDEDBDiis" & Round1$Parameter==parameter),
#                      E5=Round1 %>% filter(Round1$Expert=="96929" & Round1$Parameter==parameter ))
#     #xxfpn<-Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter=="Sali Fledglings per Nest")
#     #xx<-Round1 %>% filter(Round1$Expert=="aircon" & Round1$Parameter=="Sali Nest Attempts")
#     #P12listCom$E3[,6:8]<-xxfpn[,6:8]/xx[,6:8]
#     parmout<-makerandsComSite_gamma(P12listCom, nalts=length(P12listCom$E1[,1]), num_ex=5)
#     # maxs<-numeric(length(sites))
#     # for(i in 1:length(maxs)){
#     #   maxs[i]<-max(parmout$value)
#     # }
#     # maxy<-max(maxs)
#     parmout$ID[which(parmout$ID=="HMU")]<-"1-HMU"
#     parmout$ID[which(parmout$ID=="Refuge")]<-"2-Refuge"
#     # parmout$ID[which(parmout$ID=="Anao")]<-"3-Anao"
#     # parmout$ID[which(parmout$ID=="North Finegayan")]<-"4-North Finegayan"
#     XX2<-ggplot(parmout, aes(x=ID, y=value, fill=ID))+geom_violin(trim=T, scale="width")+
#       facet_wrap(~factor(scenario,levels=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
#                                            "Scenario 2a", "Scenario 2b", "Scenario 2c",
#                                            "Scenario 3")), scale="fixed")+
#       coord_cartesian(ylim = c(0,15))+
#       xlab("")+ylab("Probability")+labs(title=paste0(parameter),
#                                         subtitle="Elicited values standardized to 90%",fill="",
#                                         caption="Round 1")+
#       theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#             legend.position="right")
#     ggsave(XX2, filename = here("results","Round 1 Figures",
#                                 paste0(parameter, "_2 Sites_facet.png")),
#            width = 8, height = 6, dpi = 300, units = "in", device='png')
#     
#   }
# }
# 
# paramallSite("Sali Adult Survival", prob=T)
# paramallSite("Sali Juvenile Survival", prob=T)
# paramallSite("Sali Fledgling Survival", prob=T)
# paramallSite("Sali Nest Success",  prob=T)
# paramallSite("Sali Nest Attempts",  prob=F)
# paramallSite("Sali Fledglings per Nest", prob=F)

