
#Summary output from sims
library(here);library(ggplot2);library(dplyr)
#TODO organize and save figures
AllKoko_HMU_K_outs<-readRDS("results/Simulation output/Simulations/AllKoko_HMU_K_outs_2.RDS")
AllKoko_HMU_noK_outs<-readRDS("results/Simulation output/Simulations/AllKoko_HMU_noK_outs_2.RDS")

AllKoko_Refuge_K_outs<-readRDS("results/Simulation output/Simulations/AllKoko_Refuge_K_outs_2.RDS")
AllKoko_Refuge_noK_outs<-readRDS("results/Simulation output/Simulations/AllKoko_Refuge_noK_outs_2.RDS")

AllSali_HMU_K_outs<-readRDS("results/Simulation output/Simulations/AllSali_HMU_K_outs_2.RDS")
AllSali_HMU_noK_outs<-readRDS("results/Simulation output/Simulations/AllSali_HMU_noK_outs_2.RDS")

AllSali_Refuge_K_outs<-readRDS("results/Simulation output/Simulations/AllSali_Refuge_K_outs_2.RDS")
AllSali_Refuge_noK_outs<-readRDS("results/Simulation output/Simulations/AllSali_Refuge_noK_outs_2.RDS")



#sensitivity of extinction probability to whether K is included or not
sensitivityKoko_HMU<-sensitivityKoko_Refuge<-matrix(nrow=4, ncol=7)
sensitivitySali_HMU<-sensitivitySali_Refuge<-matrix(nrow=4, ncol=7)

for(i in 1:4){
  for(j in 1:7){
    sensitivityKoko_HMU[i,j]<-abs(sum(AllKoko_HMU_K_outs[[i]]$ExtOut[[j]][1,]-AllKoko_HMU_noK_outs[[i]]$ExtOut[[j]][1,]))
    sensitivityKoko_Refuge[i,j]<-abs(sum(AllKoko_Refuge_K_outs[[i]]$ExtOut[[j]][1,]-AllKoko_Refuge_noK_outs[[i]]$ExtOut[[j]][1,]))
    sensitivitySali_HMU[i,j]<-abs(sum(AllSali_HMU_K_outs[[i]]$ExtOut[[j]][1,]-AllSali_HMU_noK_outs[[i]]$ExtOut[[j]][1,]))
    sensitivitySali_Refuge[i,j]<-abs(sum(AllSali_Refuge_K_outs[[i]]$ExtOut[[j]][1,]-AllSali_Refuge_noK_outs[[i]]$ExtOut[[j]][1,]))
  }
}

plot(c(sensitivityKoko_HMU), ylab="difference between K and noK",
     main="Probability of extinction", ylim=c(0,1))
points(c(sensitivityKoko_Refuge))
points(c(sensitivitySali_HMU), col="red")
points(c(sensitivitySali_Refuge), col="red")

#find the maximum difference between with and without across all simulations
max(sensitivityKoko_HMU, sensitivityKoko_Refuge, sensitivitySali_HMU, sensitivitySali_Refuge)
#0.037

#############
#compare extinction probability with and without K
#############
#plots for Sali koko with and without K extinction probability

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#DC267F")


SaliextinctionHMU<-array(dim=c(2,21,7,4)) #k/noK
for(i in 1:4){
  for(j in 1:7){
    SaliextinctionHMU[1,,j,i]<-AllSali_HMU_K_outs[[i]]$ExtOut[[j]][1,]
    SaliextinctionHMU[2,,j,i]<-AllSali_HMU_noK_outs[[i]]$ExtOut[[j]][1,]
  }
}
Sali_extinction_HMU<-data.frame(NumRel=(c(rep(c(5,10,15,20), each=7*21))),
                                year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                                scenario=rep((rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                                                    "Scenario 2a", "Scenario 2b", "Scenario 2c",
                                                    "Scenario 3"), each=21)), times=4),
                                K=c(rep("with K", length(c(SaliextinctionHMU[1,,,]))), 
                                    rep("without K", length(c(SaliextinctionHMU[1,,,])))),
                                value=c(SaliextinctionHMU[1,,,], SaliextinctionHMU[2,,,]))


#for extinction comparison
p1<-ggplot(Sali_extinction_HMU, aes(x=year, y=value, color=scenario))+geom_line()+
  facet_grid(NumRel~K)+
  theme(legend.position="right")+ 
  scale_color_manual(limits=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                              "Scenario 2a", "Scenario 2b", "Scenario 2c",
                              "Scenario 3"), values=cbbPalette)+
  labs(y="Extinction Probability",x="Year", color="Scenario", title="Sali HMU")
p1

ggsave(p1, filename = "results/Simulation output/Sali_HMU_ex_kandnok.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')

SaliextinctionRefuge<-array(dim=c(2,21,7,4)) #k/noK
for(i in 1:4){
  for(j in 1:7){
    SaliextinctionRefuge[1,,j,i]<-AllSali_Refuge_K_outs[[i]]$ExtOut[[j]][1,]
    SaliextinctionRefuge[2,,j,i]<-AllSali_Refuge_noK_outs[[i]]$ExtOut[[j]][1,]
  }
}
Sali_extinction_Refuge<-data.frame(NumRel=(c(rep(c(5,10,15,20), each=7*21))),
                                   year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                                   scenario=rep((rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                                                       "Scenario 2a", "Scenario 2b", "Scenario 2c",
                                                       "Scenario 3"), each=21)), times=4),
                                   K=c(rep("with K", length(c(SaliextinctionRefuge[1,,,]))), 
                                       rep("without K", length(c(SaliextinctionRefuge[1,,,])))),
                                   value=c(SaliextinctionRefuge[1,,,], SaliextinctionRefuge[2,,,]))
p2<-ggplot(Sali_extinction_Refuge, aes(x=year, y=value, color=scenario))+geom_line()+
  facet_grid(NumRel~K)+
  theme(legend.position="right")+ 
  scale_color_manual(limits=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                              "Scenario 2a", "Scenario 2b", "Scenario 2c",
                              "Scenario 3"), values=cbbPalette)+
  labs(y="Extinction Probability",x="Year", color="Scenario", title="Sali Refuge")
p2
ggsave(p2, filename = "results/Simulation output/Sali_Refuge_ex_kandnok.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')

KokoextinctionHMU<-array(dim=c(2,21,7,4)) #k/noK
for(i in 1:4){
  for(j in 1:7){
    KokoextinctionHMU[1,,j,i]<-AllKoko_HMU_K_outs[[i]]$ExtOut[[j]][1,]
    KokoextinctionHMU[2,,j,i]<-AllKoko_HMU_noK_outs[[i]]$ExtOut[[j]][1,]
  }
}
Koko_extinction_HMU<-data.frame(NumRel=(c(rep(c(5,10,15,20), each=7*21))),
                                year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                                scenario=rep((rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                                                    "Scenario 2a", "Scenario 2b", "Scenario 2c",
                                                    "Scenario 3"), each=21)), times=4),
                                K=c(rep("with K", length(c(KokoextinctionHMU[1,,,]))), 
                                    rep("without K", length(c(KokoextinctionHMU[1,,,])))),
                                value=c(KokoextinctionHMU[1,,,], KokoextinctionHMU[2,,,]))

p3<-ggplot(Koko_extinction_HMU, aes(x=year, y=value, color=scenario))+geom_line()+
  facet_grid(NumRel~K)+
  theme(legend.position="right")+ 
  scale_color_manual(limits=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                              "Scenario 2a", "Scenario 2b", "Scenario 2c",
                              "Scenario 3"), values=cbbPalette)+
  labs(y="Extinction Probability",x="Year", color="Scenario", title="Koko HMU")
p3
ggsave(p3, filename = "results/Simulation output/Koko_HMU_ex_kandnok.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')

KokoextinctionRefuge<-array(dim=c(2,21,7,4)) #k/noK
for(i in 1:4){
  for(j in 1:7){
    KokoextinctionRefuge[1,,j,i]<-AllKoko_Refuge_K_outs[[i]]$ExtOut[[j]][1,]
    KokoextinctionRefuge[2,,j,i]<-AllKoko_Refuge_noK_outs[[i]]$ExtOut[[j]][1,]
  }
}
Koko_extinction_Refuge<-data.frame(NumRel=(c(rep(c(5,10,15,20), each=7*21))),
                                   year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                                   scenario=rep((rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                                                       "Scenario 2a", "Scenario 2b", "Scenario 2c",
                                                       "Scenario 3"), each=21)), times=4),
                                   K=c(rep("with K", length(c(KokoextinctionRefuge[1,,,]))), 
                                       rep("without K", length(c(KokoextinctionRefuge[1,,,])))),
                                   value=c(KokoextinctionRefuge[1,,,], KokoextinctionRefuge[2,,,]))

p4<-ggplot(Koko_extinction_Refuge, aes(x=year, y=value, color=scenario))+geom_line()+
  facet_grid(NumRel~K)+
  theme(legend.position="right")+ 
  scale_color_manual(limits=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                              "Scenario 2a", "Scenario 2b", "Scenario 2c",
                              "Scenario 3"), values=cbbPalette)+
  labs(y="Extinction Probability",x="Year", color="Scenario", title="Koko Refuge")
p4
ggsave(p4, filename = "results/Simulation output/Koko_Refuge_ex_kandnok.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')



######################
#plots for paper/report
#plot Sali extinction prob with K at HMU and refuge
####################

SaliextinctionBothSites<-array(dim=c(2,21,7,4)) #HMU/Refuge
for(i in 1:4){
  for(j in 1:7){
    SaliextinctionBothSites[1,,j,i]<-1-AllSali_HMU_K_outs[[i]]$ExtOut[[j]][1,]
    SaliextinctionBothSites[2,,j,i]<-1-AllSali_Refuge_K_outs[[i]]$ExtOut[[j]][1,]
  }
}
Sali_extinction_BothSites<-data.frame(NumRel=(c(rep(c(5,10,15,20), each=7*21))),
                                      year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                                      scenario=rep((rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                                                          "Scenario 2a", "Scenario 2b", "Scenario 2c",
                                                          "Scenario 3"), each=21)), times=4),
                                      site=c(rep("HMU", length(c(SaliextinctionBothSites[1,,,]))), 
                                             rep("Refuge", length(c(SaliextinctionBothSites[1,,,])))),
                                      value=c(SaliextinctionBothSites[1,,,], SaliextinctionBothSites[2,,,]))

p5<-ggplot(Sali_extinction_BothSites, aes(x=year, y=value, color=scenario))+geom_line()+
  facet_grid(NumRel~site)+
  theme(legend.position="right")+ 
  scale_color_manual(limits=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                              "Scenario 2a", "Scenario 2b", "Scenario 2c",
                              "Scenario 3"), values=cbbPalette)+
  labs(y="Probability of persistence",x="Year", color="Scenario", 
       title="Sali persistence probability at both sites", subtitle="with carrying capacity")
p5
ggsave(p5, filename = "results/Simulation output/Sali_persistence_bothsites.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')
####################
#plot Koko extinction prob with K at HMU and refuge
###################
KokoextinctionBothSites<-array(dim=c(2,21,7,4)) #HMU/Refuge
for(i in 1:4){
  for(j in 1:7){
    KokoextinctionBothSites[1,,j,i]<-1-AllKoko_HMU_K_outs[[i]]$ExtOut[[j]][1,]
    KokoextinctionBothSites[2,,j,i]<-1-AllKoko_Refuge_K_outs[[i]]$ExtOut[[j]][1,]
  }
}
Koko_extinction_BothSites<-data.frame(NumRel=(c(rep(c(5,10,15,20), each=7*21))),
                                      year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                                      scenario=rep((rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                                                          "Scenario 2a", "Scenario 2b", "Scenario 2c",
                                                          "Scenario 3"), each=21)), times=4),
                                      site=c(rep("HMU", length(c(KokoextinctionBothSites[1,,,]))), 
                                             rep("Refuge", length(c(KokoextinctionBothSites[1,,,])))),
                                      value=c(KokoextinctionBothSites[1,,,], KokoextinctionBothSites[2,,,]))

p6<-ggplot(Koko_extinction_BothSites, aes(x=year, y=value, color=scenario))+geom_line()+
  facet_grid(NumRel~site)+ylim(c(0,1))+
  theme(legend.position="right")+ 
  scale_color_manual(limits=c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                              "Scenario 2a", "Scenario 2b", "Scenario 2c",
                              "Scenario 3"), values=cbbPalette)+
  labs(y="Probability of persistence",x="Year", color="Scenario", 
       title="Koko persistence probability at both sites", subtitle="with carrying capacity")
p6
ggsave(p6, filename = "results/Simulation output/Koko_persistence_bothsites.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')

###############
#plot Sali and Koko extinction at HMU
##############
BothextinctionHMU<-array(dim=c(2,21,7,4)) #Sali/Koko
for(i in 1:4){
  for(j in 1:7){
    BothextinctionHMU[1,,j,i]<-1-AllSali_HMU_K_outs[[i]]$ExtOut[[j]][1,]
    BothextinctionHMU[2,,j,i]<-1-AllKoko_HMU_K_outs[[i]]$ExtOut[[j]][1,]
  }
}
Both_extinction_HMU<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                                year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                                scenario=rep((rep(c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                                                    "Strategy 2a", "Strategy 2b", "Strategy 2c",
                                                    "Strategy 3"), each=21)), times=4),
                                species=c(rep("Sali", length(c(BothextinctionHMU[1,,,]))), 
                                          rep("Koko", length(c(BothextinctionHMU[1,,,])))),
                                value=c(BothextinctionHMU[1,,,], BothextinctionHMU[2,,,]))

p7<-ggplot(Both_extinction_HMU, aes(x=year, y=value, color=scenario))+geom_line()+
  facet_grid(NumRel~species)+
  theme(legend.position="right")+ 
  scale_color_manual(limits=c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                              "Strategy 2a", "Strategy 2b", "Strategy 2c",
                              "Strategy 3"), values=cbbPalette)+
  labs(y="Probability of persistence",x="Year", color="Scenario", 
       title="Sali and Koko persistence probability at HMU", subtitle="with carrying capacity")
p7

ggsave(p7, filename = "results/Simulation output/HMU_persistence_bothsp.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')
#############
#plot Sali and Koko extinction at Refuge
############
BothextinctionRefuge<-array(dim=c(2,21,7,4)) #Sali/Koko
for(i in 1:4){
  for(j in 1:7){
    BothextinctionRefuge[1,,j,i]<-1-AllSali_Refuge_K_outs[[i]]$ExtOut[[j]][1,]
    BothextinctionRefuge[2,,j,i]<-1-AllKoko_Refuge_K_outs[[i]]$ExtOut[[j]][1,]
  }
}
Both_extinction_Refuge<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                                   year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                                   scenario=rep((rep(c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                                                       "Strategy 2a", "Strategy 2b", "Strategy 2c",
                                                       "Strategy 3"), each=21)), times=4),
                                   species=c(rep("Sali", length(c(BothextinctionRefuge[1,,,]))), 
                                             rep("Koko", length(c(BothextinctionRefuge[1,,,])))),
                                   value=c(BothextinctionRefuge[1,,,], BothextinctionRefuge[2,,,]))

p8<-ggplot(Both_extinction_Refuge, aes(x=year, y=value, color=scenario))+geom_line()+
  facet_grid(NumRel~species)+
  theme(legend.position="right")+ 
  scale_color_manual(limits=c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                              "Strategy 2a", "Strategy 2b", "Strategy 2c",
                              "Strategy 3"), values=cbbPalette)+
  labs(y="Probability of persistence",x="Year", color="Scenario", 
       title="Sali and Koko persistence probability at Refuge", subtitle="with carrying capacity")
p8
ggsave(p8, filename = "results/Simulation output/Refuge_persistence_bothsp.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')

####make a figure with both species at both sites

AllSpAllSiteDF<-rbind(Both_extinction_HMU, Both_extinction_Refuge)
AllSpAllSiteDF$site<-c(rep("HMU", length(Both_extinction_HMU[,1])), rep("Refuge", length(Both_extinction_Refuge[,1])))

p9<-ggplot(AllSpAllSiteDF, aes(x=year, y=value, color=scenario))+geom_line()+
  facet_grid(NumRel~species+site)+
  theme(legend.position="right")+ 
  scale_color_manual(limits=c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                              "Strategy 2a", "Strategy 2b", "Strategy 2c",
                              "Strategy 3"), values=cbbPalette)+
  labs(y="Probability of persistence",x="Year", color="Strategy")+theme_bw()
p9
#TODO: remove title - will be one of the figures
ggsave(p9, filename = "results/Simulation output/Figure 2.png",  bg = "white",
       width = 8, height = 6, dpi = 300, units = "in", device='png')

#############################
#Species at sites by scenario
############################
#Sali HMU
SaliMeansHMU<-array(dim=c(21, 7, 4,4)) #years, scenarios, numRel, summstats
for(j in 1:4){
  for(i in 1:7){
    SaliMeansHMU[,i,j,1]<-AllSali_HMU_K_outs[[j]]$SaliOut[[i]][1,]
    SaliMeansHMU[,i,j,2]<-AllSali_HMU_K_outs[[j]]$SaliOut[[i]][2,]
    SaliMeansHMU[,i,j,3]<-AllSali_HMU_K_outs[[j]]$SaliOut[[i]][5,]
    SaliMeansHMU[,i,j,4]<-AllSali_HMU_K_outs[[j]]$SaliOut[[i]][6,]
  }
}
Sali_HMU<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                     year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                     scenario=rep((rep(c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                                         "Strategy 2a", "Strategy 2b", "Strategy 2c",
                                         "Strategy 3"), each=21)), times=4),
                     mean=c(SaliMeansHMU[,,,1]),
                     median=c(SaliMeansHMU[,,,2]),
                     low=c(SaliMeansHMU[,,,3]),
                     high=c(SaliMeansHMU[,,,4]))

ps1<-ggplot(Sali_HMU, aes(x=year, y=mean, color=NumRel))+
  facet_wrap(~scenario, scales="free")+
  geom_ribbon(data=Sali_HMU, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Adults Released",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "horizontal",
                       title.position = "top"
                     ))+
  scale_fill_manual(name="Adults Released",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top"
                    ))+
  scale_linetype(name="")+
  labs(title="Sali HMU projections")+
  geom_line(linewidth=0.8)+ylab(label="Adult Sali")+xlab(label="Year")+
  theme_bw()+
  theme(
    legend.position = c(1, 0.15),
    legend.justification = c("right"),
    legend.box.just = "center",
    legend.margin = margin(3, 3, 3,3),
    legend.direction = "horizontal"
  )

ps1

ggsave(ps1, filename = "results/Simulation output/Sali_HMU_proj.png",  bg = "white",
       width = 8, height = 6, dpi = 300, units = "in", device='png')

#Sali at refuge
SaliMeansRefuge<-array(dim=c(21, 7, 4,4)) #years, scenarios, numRel, summstats
for(j in 1:4){
  for(i in 1:7){
    SaliMeansRefuge[,i,j,1]<-AllSali_Refuge_K_outs[[j]]$SaliOut[[i]][1,]
    SaliMeansRefuge[,i,j,2]<-AllSali_Refuge_K_outs[[j]]$SaliOut[[i]][2,]
    SaliMeansRefuge[,i,j,3]<-AllSali_Refuge_K_outs[[j]]$SaliOut[[i]][5,]
    SaliMeansRefuge[,i,j,4]<-AllSali_Refuge_K_outs[[j]]$SaliOut[[i]][6,]
  }
}
#make a data frame for data
Sali_Refuge<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                     year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                     scenario=rep((rep(c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                                         "Strategy 2a", "Strategy 2b", "Strategy 2c",
                                         "Strategy 3"), each=21)), times=4),
                     mean=c(SaliMeansRefuge[,,,1]),
                     median=c(SaliMeansRefuge[,,,2]),
                     low=c(SaliMeansRefuge[,,,3]),
                     high=c(SaliMeansRefuge[,,,4]))

ps2<-ggplot(Sali_Refuge, aes(x=year, y=mean, color=NumRel))+
  facet_wrap(~scenario, scales="free")+
  geom_ribbon(data=Sali_Refuge, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Adults Released",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "horizontal",
                       title.position = "top"
                     ))+
  scale_fill_manual(name="Adults Released",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top"
                    ))+
  scale_linetype(name="")+
  labs(title="Sali Refuge projections")+
  geom_line(linewidth=0.8)+ylab(label="Adult Sali")+xlab(label="Year")+
  theme_bw()+
  theme(
    legend.position = c(1, 0.15),
    legend.justification = c("right"),
    legend.box.just = "center",
    legend.margin = margin(3, 3, 3,3),
    legend.direction = "horizontal"
  )

ps2
ggsave(ps2, filename = "results/Simulation output/Sali_Refuge_proj.png",  bg = "white",
       width = 8, height = 6, dpi = 300, units = "in", device='png')

#Koko HMU
KokoMeansHMU<-array(dim=c(21, 7, 4,4)) #years, scenarios, numRel, summstats
for(j in 1:4){
  for(i in 1:7){
    KokoMeansHMU[,i,j,1]<-AllKoko_HMU_K_outs[[j]]$KokoOut[[i]][1,]
    KokoMeansHMU[,i,j,2]<-AllKoko_HMU_K_outs[[j]]$KokoOut[[i]][2,]
    KokoMeansHMU[,i,j,3]<-AllKoko_HMU_K_outs[[j]]$KokoOut[[i]][5,]
    KokoMeansHMU[,i,j,4]<-AllKoko_HMU_K_outs[[j]]$KokoOut[[i]][6,]
  }
}
Koko_HMU<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                     year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                     scenario=rep((rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                                         "Scenario 2a", "Scenario 2b", "Scenario 2c",
                                         "Scenario 3"), each=21)), times=4),
                     mean=c(KokoMeansHMU[,,,1]),
                     median=c(KokoMeansHMU[,,,2]),
                     low=c(KokoMeansHMU[,,,3]),
                     high=c(KokoMeansHMU[,,,4]))

ps3<-ggplot(Koko_HMU, aes(x=year, y=mean, color=NumRel))+
  facet_wrap(~scenario, scales="free")+
  geom_ribbon(data=Koko_HMU, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Adults Released",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "horizontal",
                       title.position = "top"
                     ))+
  scale_fill_manual(name="Adults Released",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top"
                    ))+
  scale_linetype(name="")+
  labs(title="Koko HMU projections")+
  geom_line(linewidth=0.8)+ylab(label="Adult Koko")+xlab(label="Year")+
  theme_bw()+
  theme(
    legend.position = c(1, 0.15),
    legend.justification = c("right"),
    legend.box.just = "center",
    legend.margin = margin(3, 3, 3,3),
    legend.direction = "horizontal"
  )

ps3
ggsave(ps3, filename = "results/Simulation output/Koko_HMU_proj.png",  bg = "white",
       width = 8, height = 6, dpi = 300, units = "in", device='png')
#Koko at refuge
KokoMeansRefuge<-array(dim=c(21, 7, 4,4)) #years, scenarios, numRel, summstats
for(j in 1:4){
  for(i in 1:7){
    KokoMeansRefuge[,i,j,1]<-AllKoko_Refuge_K_outs[[j]]$KokoOut[[i]][1,]
    KokoMeansRefuge[,i,j,2]<-AllKoko_Refuge_K_outs[[j]]$KokoOut[[i]][2,]
    KokoMeansRefuge[,i,j,3]<-AllKoko_Refuge_K_outs[[j]]$KokoOut[[i]][5,]
    KokoMeansRefuge[,i,j,4]<-AllKoko_Refuge_K_outs[[j]]$KokoOut[[i]][6,]
  }
}
#make a data frame for data
Koko_Refuge<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                        year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                        scenario=rep((rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                                            "Scenario 2a", "Scenario 2b", "Scenario 2c",
                                            "Scenario 3"), each=21)), times=4),
                        mean=c(KokoMeansRefuge[,,,1]),
                        median=c(KokoMeansRefuge[,,,2]),
                        low=c(KokoMeansRefuge[,,,3]),
                        high=c(KokoMeansRefuge[,,,4]))

ps4<-ggplot(Koko_Refuge, aes(x=year, y=mean, color=NumRel))+
  facet_wrap(~scenario, scales="free")+
  geom_ribbon(data=Koko_Refuge, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Adults Released",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "horizontal",
                       title.position = "top"
                     ))+
  scale_fill_manual(name="Adults Released",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top"
                    ))+
  scale_linetype(name="")+
  labs(title="Koko Refuge projections")+
  geom_line(linewidth=0.8)+ylab(label="Adult Koko")+xlab(label="Year")+
  theme_bw()+
  theme(
    legend.position = c(1, 0.15),
    legend.justification = c("right"),
    legend.box.just = "center",
    legend.margin = margin(3, 3, 3,3),
    legend.direction = "horizontal"
  )

ps4
ggsave(ps4, filename = "results/Simulation output/Koko_Refuge_proj.png",  bg = "white",
       width = 8, height = 6, dpi = 300, units = "in", device='png')



##############
#probability of  persistence with 95%
##############
Saliext_HMU<-array(dim=c(3,21,7,4)) #Sali/Koko
for(i in 1:4){
  for(j in 1:7){
    Saliext_HMU[1,,j,i]<-1-AllSali_HMU_K_outs[[i]]$ExtOut[[j]][1,]
    Saliext_HMU[2,,j,i]<-1-AllSali_HMU_K_outs[[i]]$ExtOut[[j]][5,]
    Saliext_HMU[3,,j,i]<-1-AllSali_HMU_K_outs[[i]]$ExtOut[[j]][6,]
  }
}

Saliext_HMU_DF<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                           year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                           scenario=rep((rep(c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                                               "Strategy 2a", "Strategy 2b", "Strategy 2c",
                                               "Strategy 3"), each=21)), times=4),
                           mean=c(Saliext_HMU[1,,,]),
                           low=c(Saliext_HMU[2,,,]),
                           high=c(Saliext_HMU[3,,,]))
EX_Sali_hmu<-ggplot(Saliext_HMU_DF, aes(x=year, y=mean, color=NumRel))+
         facet_wrap(~scenario)+
  geom_ribbon(data=Saliext_HMU_DF, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Initial Adults",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "horizontal",
                       title.position = "right"
                     ))+
  scale_fill_manual(name="Initial Adults",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "right"
                    ))+
  scale_linetype(name="")+
  labs(title="Sali Persistence probability, HMU")+
  geom_line(linewidth=0.8)+ylab(label="Probability of persistence")+xlab(label="Year")+
  theme_bw()+
  theme(
    legend.position = c(1, 0.15),
    legend.justification = c("right"),
    legend.box.just = "center",
    legend.margin = margin(3, 3, 3,3),
    legend.direction = "horizontal"
  )

EX_Sali_hmu

ggsave(EX_Sali_hmu, filename = "results/Simulation output/Persistence_Sali_HMU.png",  bg = "white",
       width = 8, height = 8, dpi = 300, units = "in", device='png')
###Sali Refuge
Saliext_Refuge<-array(dim=c(3,21,7,4)) #Sali/Koko
for(i in 1:4){
  for(j in 1:7){
    Saliext_Refuge[1,,j,i]<-1-AllSali_Refuge_K_outs[[i]]$ExtOut[[j]][1,]
    Saliext_Refuge[2,,j,i]<-1-AllSali_Refuge_K_outs[[i]]$ExtOut[[j]][5,]
    Saliext_Refuge[3,,j,i]<-1-AllSali_Refuge_K_outs[[i]]$ExtOut[[j]][6,]
  }
}

Saliext_Refuge_DF<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                           year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                           scenario=rep((rep(c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                                               "Strategy 2a", "Strategy 2b", "Strategy 2c",
                                               "Strategy 3"), each=21)), times=4),
                           mean=c(Saliext_Refuge[1,,,]),
                           low=c(Saliext_Refuge[2,,,]),
                           high=c(Saliext_Refuge[3,,,]))
EX_Sali_ref<-ggplot(Saliext_Refuge_DF, aes(x=year, y=mean, color=NumRel))+
  facet_wrap(~scenario)+
  geom_ribbon(data=Saliext_Refuge_DF, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Initial Adults",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "horizontal",
                       title.position = "right"
                     ))+
  scale_fill_manual(name="Initial Adults",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "right"
                    ))+
  scale_linetype(name="")+
  labs(title="Sali Persistence probability, Refuge")+
  geom_line(linewidth=0.8)+ylab(label="Probability of persistence")+xlab(label="Year")+
  theme_bw()+
  theme(
    legend.position = c(1, 0.15),
    legend.justification = c("right"),
    legend.box.just = "center",
    legend.margin = margin(3, 3, 3,3),
    legend.direction = "horizontal"
  )

EX_Sali_ref

ggsave(EX_Sali_ref, filename = "results/Simulation output/Persistence_Sali_Refuge.png",  bg = "white",
       width = 8, height = 8, dpi = 300, units = "in", device='png')


#for koko at HMu
Kokoext_HMU<-array(dim=c(3,21,7,4)) #Sali/Koko
for(i in 1:4){
  for(j in 1:7){
    Kokoext_HMU[1,,j,i]<-1-AllKoko_HMU_K_outs[[i]]$ExtOut[[j]][1,]
    Kokoext_HMU[2,,j,i]<-1-AllKoko_HMU_K_outs[[i]]$ExtOut[[j]][5,]
    Kokoext_HMU[3,,j,i]<-1-AllKoko_HMU_K_outs[[i]]$ExtOut[[j]][6,]
  }
}

Kokoext_HMU_DF<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                           year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                           scenario=rep((rep(c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                                               "Strategy 2a", "Strategy 2b", "Strategy 2c",
                                               "Strategy 3"), each=21)), times=4),
                           mean=c(Kokoext_HMU[1,,,]),
                           low=c(Kokoext_HMU[2,,,]),
                           high=c(Kokoext_HMU[3,,,]))
EX_Koko_hmu<-ggplot(Kokoext_HMU_DF, aes(x=year, y=mean, color=NumRel))+
  facet_wrap(~scenario)+
  geom_ribbon(data=Kokoext_HMU_DF, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Initial Adults",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "horizontal",
                       title.position = "right"
                     ))+
  scale_fill_manual(name="Initial Adults",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "right"
                    ))+
  scale_linetype(name="")+
  labs(title="Koko Persistence probability, HMU")+
  geom_line(linewidth=0.8)+ylab(label="Probability of persistence")+xlab(label="Year")+
  theme_bw()+
  theme(
    legend.position = c(1, 0.15),
    legend.justification = c("right"),
    legend.box.just = "center",
    legend.margin = margin(3, 3, 3,3),
    legend.direction = "horizontal"
  )

EX_Koko_hmu

ggsave(EX_Sali_hmu, filename = "results/Simulation output/Persistence_Koko_HMU.png",  bg = "white",
       width = 8, height = 8, dpi = 300, units = "in", device='png')
###Sali Refuge
Kokoext_Refuge<-array(dim=c(3,21,7,4)) #Sali/Koko
for(i in 1:4){
  for(j in 1:7){
    Kokoext_Refuge[1,,j,i]<-1-AllKoko_Refuge_K_outs[[i]]$ExtOut[[j]][1,]
    Kokoext_Refuge[2,,j,i]<-1-AllKoko_Refuge_K_outs[[i]]$ExtOut[[j]][5,]
    Kokoext_Refuge[3,,j,i]<-1-AllKoko_Refuge_K_outs[[i]]$ExtOut[[j]][6,]
  }
}

Kokoext_Refuge_DF<-data.frame(NumRel=(c(rep(c("10","20","30","40"), each=7*21))),
                              year=rep(c(rep(seq(1,21,by=1), times=7)), times=4),
                              scenario=rep((rep(c("Strategy 1a", "Strategy 1b", "Strategy 1c",
                                                  "Strategy 2a", "Strategy 2b", "Strategy 2c",
                                                  "Strategy 3"), each=21)), times=4),
                              mean=c(Kokoext_Refuge[1,,,]),
                              low=c(Kokoext_Refuge[2,,,]),
                              high=c(Kokoext_Refuge[3,,,]))
EX_Koko_ref<-ggplot(Kokoext_Refuge_DF, aes(x=year, y=mean, color=NumRel))+
  facet_wrap(~scenario)+
  geom_ribbon(data=Kokoext_Refuge_DF, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Initial Adults",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "horizontal",
                       title.position = "right"
                     ))+
  scale_fill_manual(name="Initial Adults",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "right"
                    ))+
  scale_linetype(name="")+
  labs(title="Koko Persistence probability, Refuge")+
  geom_line(linewidth=0.8)+ylab(label="Probability of persistence")+xlab(label="Year")+
  theme_bw()+
  theme(
    legend.position = c(1, 0.15),
    legend.justification = c("right"),
    legend.box.just = "center",
    legend.margin = margin(3, 3, 3,3),
    legend.direction = "horizontal"
  )

EX_Koko_ref

ggsave(EX_Koko_ref, filename = "results/Simulation output/Persistence_Koko_Refuge.png",  bg = "white",
       width = 8, height = 8, dpi = 300, units = "in", device='png')



SaliBothSites<-rbind(Saliext_HMU_DF, Saliext_Refuge_DF)
SaliBothSites$site<-c(rep("HMU", length(Saliext_HMU_DF[,1])), 
                      rep("Refuge", length(Saliext_Refuge_DF[,1])))

SaliBoth<-ggplot(SaliBothSites, aes(x=year, y=mean, color=NumRel))+geom_line()+
  facet_grid(scenario~site)+
  geom_ribbon(data=SaliBothSites, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Initial adult abundance",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "vertical",
                       title.position = "top"
                     ))+
  scale_fill_manual(name="Initial adult abundance",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "vertical",
                      title.position = "top"
                    ))+
  scale_linetype(name="")+
  geom_line(linewidth=0.8)+ylab(label="Probability of persistence")+xlab(label="Year")+
  theme_bw()

SaliBoth
ggsave(SaliBoth, filename = "results/Simulation output/Figure 3.png",  bg = "white",
       width = 10, height = 8, dpi = 300, units = "in", device='png')


#both sites Koko
KokoBothSites<-rbind(Kokoext_HMU_DF, Kokoext_Refuge_DF)
KokoBothSites$site<-c(rep("HMU", length(Kokoext_HMU_DF[,1])), 
                      rep("Refuge", length(Kokoext_Refuge_DF[,1])))
KokoBoth<-ggplot(KokoBothSites, aes(x=year, y=mean, color=NumRel))+geom_line()+
  facet_grid(scenario~site)+
  geom_ribbon(data=KokoBothSites, aes(ymin=low, ymax=high, fill=NumRel), alpha=0.05, 
              show.legend = T, linetype=2, linewidth=0.5)+
  scale_color_manual(name="Initial adult abundance",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                     limits=c("10","20","30","40"),
                     guide = guide_legend(
                       direction = "vertical",
                       title.position = "top"
                     ))+
  scale_fill_manual(name="Initial adult abundance",values=c("#648FFF","#DC267F","#FE6100","#785EF0"), 
                    limits=c("10","20","30","40"),
                    guide = guide_legend(
                      direction = "vertical",
                      title.position = "top"
                    ))+
  scale_linetype(name="")+
  geom_line(linewidth=0.8)+ylab(label="Probability of persistence")+xlab(label="Year")+
  theme_bw()

KokoBoth
ggsave(KokoBoth, filename = "results/Simulation output/Figure 4.png",  bg = "white",
       width = 10, height = 8, dpi = 300, units = "in", device='png')





