#TODO: clean this up
#figures: both SD at both sites
#TODO: make one that looks at the number released and compares the sites

#simple example of CDF from extinction probabilities

#here are some extinction probabilties 
#ext<-sort(runif(21,0,0.5))
library(here);library(ggplot2)
AllSali_HMU_K_outs<-readRDS("results/Simulation output/Simulations/AllSali_HMU_K_outs_2.RDS")
AllSali_Refuge_K_outs<-readRDS("results/Simulation output/Simulations/AllSali_Refuge_K_outs_2.RDS")

AllKoko_HMU_K_outs<-readRDS("results/Simulation output/Simulations/AllKoko_HMU_K_outs_2.RDS")
AllKoko_Refuge_K_outs<-readRDS("results/Simulation output/Simulations/AllKoko_Refuge_K_outs_2.RDS")



CDF_ext<-function(extprobs,x){
  temps<-0
  for(i in extprobs){
    if(i<=x){
      temps<-temps+1
    }
  }
  return(temps/length(extprobs))
}

# trial<-rbeta(10000, 0.4,0.2)
# hist(trial)
# cdftrial<-numeric(length(seqx))
# for(i in 1:length(seqx)){
#   cdftrial[i]<-CDF_ext(trial, seqx[i])  
# }
# 
# hist(trial)
# plot(seqx, cdftrial)

seqx<-seq(-0.000000001,1,length.out=50)
xx<-matrix(nrow=length(seqx), ncol=7)

CDFout<-NULL
for(k in 1:4){
  for(j in 1:7){
    temp<-(1-AllSali_HMU_K_outs[[k]]$outextprobs[[j]])
    for(i in 1:length(xx[,1])){
      xx[i,j]<-CDF_ext(temp,seqx[i])
    }
  }
  CDFout[[k]]<-xx
}


cdfs<-data.frame(ID=rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                            "Strategy 2a", "Strategy 2b","Strategy 2c",
                            "Strategy 3"), each=length(seqx)), times=4),
                 numrel=c(rep(c("10","20","30","40"), each=(length(seqx)*7))),
                 xaxis=c(CDFout[[1]], CDFout[[2]], CDFout[[3]], CDFout[[4]]),
                 yaxis=c(rep(rep(seqx, times=7), times=4)))
ggplot(cdfs, aes(x=yaxis, y=xaxis, color=ID))+geom_line()+facet_wrap(~numrel, ncol=1)+
  xlim(c(-0.01,1))+ylim(c(0,1))+
  xlab("Probability of Persistence")+ylab("CDF")+theme_bw()+labs(title=c("Sali HMU"))


seqx2<-seqx#seq(0,1,length.out=50)
xx2<-matrix(nrow=length(seqx2), ncol=7)

CDFout2<-NULL
for(k in 1:4){
  for(j in 1:7){
    temp<-(1-AllSali_Refuge_K_outs[[k]]$outextprobs[[j]])
    for(i in 1:length(xx2[,1])){
      xx2[i,j]<-CDF_ext(temp,seqx2[i])
    }
  }
  CDFout2[[k]]<-xx2
}

cdfs2<-data.frame(ID=rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                               "Strategy 2a", "Strategy 2b","Strategy 2c",
                               "Strategy 3"), each=length(seqx2)), times=4),
                 numrel=c(rep(c("10","20","30","40"), each=(length(seqx2)*7))),
                 yaxis=c(CDFout2[[1]], CDFout2[[2]], CDFout2[[3]], CDFout2[[4]]),
                 xaxis=c(rep(rep(seqx2, times=7), times=4)))
ggplot(cdfs2, aes(x=xaxis, y=yaxis, color=ID))+geom_line()+facet_wrap(~numrel, ncol=1)+
  xlim(c(-0.001,1))+ylim(c(0,1))+
  xlab("Probability of Persistence")+ylab("CDF")+theme_bw()+labs(title=c("Sali Refuge"))


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#DC267F")


#put HMU and Refuge side by side
bothCDFs<-data.frame(Strategy=rep(rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                                            "Strategy 2a", "Strategy 2b","Strategy 2c",
                                            "Strategy 3"), each=length(seqx2)), times=4),times=2),
                            numrel=rep(c(rep(c("10","20","30","40"), each=(length(seqx2)*7))),times=2),
                            site=c(rep(c("HMU","Refuge"), each=(7*4*length(seqx)))),
                            yaxis=c(CDFout[[1]], CDFout[[2]], CDFout[[3]], CDFout[[4]],
                                    CDFout2[[1]], CDFout2[[2]], CDFout2[[3]], CDFout2[[4]]),
                            xaxis=c(rep(rep(seqx2, times=7), times=4)))
SalibothSD<-ggplot(bothCDFs, aes(x=xaxis, y=yaxis, color=Strategy))+geom_line()+
  facet_grid(numrel~site)+
  xlab("Probability of persistence")+ylab("Cumulative probability")+
  xlim(c(-0.0001,1))+ylim(c(0,1))+labs(caption="Sali both sites")+theme_bw()+
  scale_color_manual(limits=c("Strategy 1a","Strategy 1b","Strategy 1c",
                              "Strategy 2a", "Strategy 2b","Strategy 2c",
                              "Strategy 3"), values=cbbPalette)

SalibothSD
ggsave(SalibothSD, filename = "results/Simulation output/Sali_SD_bothsites.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')
#Koko

seqxK<-seqx#seq(0,1,length.out=50)
xxK<-matrix(nrow=length(seqxK), ncol=7)

CDFoutK<-NULL
for(k in 1:4){
  for(j in 1:7){
    temp<-(1-AllKoko_HMU_K_outs[[k]]$outextprobs[[j]])
    for(i in 1:length(xx[,1])){
      xxK[i,j]<-CDF_ext(temp,seqxK[i])
    }
  }
  CDFoutK[[k]]<-xxK
}


cdfsK<-data.frame(ID=rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                               "Strategy 2a", "Strategy 2b","Strategy 2c",
                               "Strategy 3"), each=length(seqxK)), times=4),
                 numrel=c(rep(c("10","20","30","40"), each=(length(seqx)*7))),
                 yaxis=c(CDFoutK[[1]], CDFoutK[[2]], CDFoutK[[3]], CDFoutK[[4]]),
                 xaxis=c(rep(rep(seqxK, times=7), times=4)))
ggplot(cdfsK, aes(x=xaxis, y=yaxis, color=ID))+geom_line()+facet_wrap(~numrel, ncol=1)+
  xlim(c(-0.0001,1))+ylim(c(0,1))+
  xlab("Probability of Persistence")+ylab("CDF")+theme_bw()+labs(title=c("Koko HMU"))


seqx2K<-seqx#seq(0,1,length.out=50)
xx2K<-matrix(nrow=length(seqx2K), ncol=7)

CDFout2K<-NULL
for(k in 1:4){
  for(j in 1:7){
    temp<-(1-AllKoko_Refuge_K_outs[[k]]$outextprobs[[j]])
    for(i in 1:length(xx2[,1])){
      xx2K[i,j]<-CDF_ext(temp,seqx2K[i])
    }
  }
  CDFout2K[[k]]<-xx2K
}

cdfs2K<-data.frame(ID=rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                                "Strategy 2a", "Strategy 2b","Strategy 2c",
                                "Strategy 3"), each=length(seqx2K)), times=4),
                  numrel=c(rep(c("10","20","30","40"), each=(length(seqx2K)*7))),
                  yaxis=c(CDFout2K[[1]], CDFout2K[[2]], CDFout2K[[3]], CDFout2K[[4]]),
                  xaxis=c(rep(rep(seqx2K, times=7), times=4)))
ggplot(cdfs2K, aes(x=xaxis, y=yaxis, color=ID))+geom_line()+facet_wrap(~numrel, ncol=1)+
  xlim(c(-0.0001,1))+ylim(c(0,1))+
  xlab("Probability of Persistence")+ylab("CDF")+theme_bw()+labs(title=c("Koko Refuge"))


#put HMU and Refuge side by side

bothCDFsK<-data.frame(Strategy=rep(rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                                             "Strategy 2a", "Strategy 2b","Strategy 2c",
                                             "Strategy 3"), each=length(seqx2K)), times=4),times=2),
                     numrel=rep(c(rep(c("10","20","30","40"), each=(length(seqx2K)*7))),times=2),
                     site=c(rep(c("HMU","Refuge"), each=(7*4*length(seqxK)))),
                     yaxis=c(CDFoutK[[1]], CDFoutK[[2]], CDFoutK[[3]], CDFoutK[[4]],
                             CDFout2K[[1]], CDFout2K[[2]], CDFout2K[[3]], CDFout2K[[4]]),
                     xaxis=c(rep(rep(seqx2K, times=7), times=4)))
KokobothSD<-ggplot(bothCDFsK, aes(x=xaxis, y=yaxis, color=Strategy))+
  geom_line()+#, aes(linetype=Scenario))+
  facet_grid(numrel~site)+
  xlab("Probability of persistence")+ylab("Cumulative probability")+
  xlim(c(-0.001,1))+ylim(c(0,1))+labs(caption="Koko both sites")+theme_bw()+
  scale_color_manual(limits=c("Strategy 1a","Strategy 1b","Strategy 1c",
                              "Strategy 2a", "Strategy 2b","Strategy 2c",
                              "Strategy 3"), values=cbbPalette)


KokobothSD
ggsave(KokobothSD, filename = "results/Simulation output/Koko_SD_bothsites.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')


#both species SD at HMU

SDboth_HMU<-data.frame(Strategy=rep(rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                                              "Strategy 2a", "Strategy 2b","Strategy 2c",
                                              "Strategy 3"), each=length(seqx2K)), times=4),times=2),
                       numrel=rep(c(rep(c("10","20","30","40"), each=(length(seqx2K)*7))),times=2),
                       species=c(rep(c("Sali","Koko"), each=(7*4*length(seqxK)))),
                       yaxis=c(CDFout[[1]], CDFout[[2]], CDFout[[3]], CDFout[[4]],
                               CDFoutK[[1]], CDFoutK[[2]], CDFoutK[[3]], CDFoutK[[4]]),
                       xaxis=c(rep(rep(seqx2K, times=7), times=4)))

HMUbothSD<-ggplot(SDboth_HMU, aes(x=xaxis, y=yaxis, color=Strategy))+
  geom_line()+#, aes(linetype=Scenario))+
  facet_grid(numrel~species)+
  xlab("Probability of persistence")+ylab("Cumulative probability")+
  #xlim(c(0,1))+
  ylim(c(0,1))+labs(caption="HMU, both species")+theme_bw()+
  scale_color_manual(limits=c("Strategy 1a","Strategy 1b","Strategy 1c",
                              "Strategy 2a", "Strategy 2b","Strategy 2c",
                              "Strategy 3"), values=cbbPalette)


HMUbothSD
ggsave(HMUbothSD, filename = "results/Simulation output/BothSpeciesSD_HMU.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')


#both species SD at Refuge

SDboth_Ref<-data.frame(Strategy=rep(rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                                              "Strategy 2a", "Strategy 2b","Strategy 2c",
                                              "Strategy 3"), each=length(seqx2K)), times=4),times=2),
                       numrel=rep(c(rep(c("10","20","30","40"), each=(length(seqx2K)*7))),times=2),
                       species=c(rep(c("Sali","Koko"), each=(7*4*length(seqxK)))),
                       yaxis=c(CDFout2[[1]], CDFout2[[2]], CDFout2[[3]], CDFout2[[4]],
                               CDFout2K[[1]], CDFout2K[[2]], CDFout2K[[3]], CDFout2K[[4]]),
                       xaxis=c(rep(rep(seqx2K, times=7), times=4)))

RefbothSD<-ggplot(SDboth_Ref, aes(x=xaxis, y=yaxis, color=Strategy))+
  geom_line()+#, aes(linetype=Scenario))+
  facet_grid(numrel~species)+
  xlab("Probability of persistence")+ylab("Cumulative probability")+
  #xlim(c(0,1))+
  ylim(c(0,1))+labs(caption="Refuge, both species")+theme_bw()+
  scale_color_manual(limits=c("Strategy 1a","Strategy 1b","Strategy 1c",
                              "Strategy 2a", "Strategy 2b","Strategy 2c",
                              "Strategy 3"), values=cbbPalette)


RefbothSD
ggsave(RefbothSD, filename = "results/Simulation output/BothSpeciesSD_Refuge.png",  bg = "white",
       width = 6, height = 8, dpi = 300, units = "in", device='png')

#both species at both sites
#TODO: this will be a figure

SD_BothBoth<-rbind(SDboth_HMU, SDboth_Ref)
SD_BothBoth$site<-c(rep("HMU", length(SDboth_HMU[,1])),
                    rep("Refuge", length(SDboth_Ref[,1])))

BothBoth<-ggplot(SD_BothBoth, aes(x=xaxis, y=yaxis, color=Strategy))+
  geom_line(linewidth=0.5)+
  facet_grid(numrel~species+site)+
  xlab("Probability of persistence")+ylab("Cumulative probability")+
  #xlim(c(0,1))+
  ylim(c(0,1))+theme_bw()+
  scale_color_manual(limits=c("Strategy 1a","Strategy 1b","Strategy 1c",
                              "Strategy 2a", "Strategy 2b","Strategy 2c",
                              "Strategy 3"), values=cbbPalette)
BothBoth

ggsave(BothBoth, filename = "results/Simulation output/Figure 7.png",  bg = "white",
       width = 10, height = 8, dpi = 300, units = "in", device='png')



SDboth_Ref<-data.frame(Scenario=rep(rep(rep(c("Scenario 1a","Scenario 1b","Scenario 1c",
                                              "Scenario 2a", "Scenario 2b","Scenario 2c",
                                              "Scenario 3"), each=length(seqx2K)), times=4),times=2),
                       numrel=rep(c(rep(c("10","20","30","40"), each=(length(seqx2K)*7))),times=2),
                       species=c(rep(c("Sali","Koko"), each=(7*4*length(seqxK)))),
                       xaxis=c(CDFout2[[1]], CDFout2[[2]], CDFout2[[3]], CDFout2[[4]],
                               CDFout2K[[1]], CDFout2K[[2]], CDFout2K[[3]], CDFout2K[[4]]),
                       yaxis=c(rep(rep(seqx2K, times=7), times=4)))

###make violin plots of all the probs
probs<-array(dim=c(2,400000, 4, 7))
for(i in 1:4){
  for(j in 1:7){
    probs[1,,i,j]<-(1-AllSali_HMU_K_outs[[i]]$outextprobs[[j]])
    probs[2,,i,j]<-(1-AllSali_Refuge_K_outs[[i]]$outextprobs[[j]])
  }
}
Meanprobs<-array(dim=c(2,4,7))
for(i in 1:4){
  for(j in 1:7){
    Meanprobs[1,i,j]<-mean(1-AllSali_HMU_K_outs[[i]]$outextprobs[[j]])
    Meanprobs[2,i,j]<-mean(1-AllSali_Refuge_K_outs[[i]]$outextprobs[[j]])
  }
}

violinDF<-data.frame(Strategy=c(rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                                          "Strategy 2a", "Strategy 2b","Strategy 2c",
                                          "Strategy 3"), each=400000), times=1)), 
                     numrel=c(rep(c("10","20","30","40"), each=(400000*7))),
                     Site=c(rep("HMU", length(c(probs[1,,,]))), 
                            rep("Refuge", length(c(probs[2,,,])))),
                     vals=c(probs[1,,1,], probs[1,,2,], probs[1,,3,], probs[1,,4,],
                            probs[2,,1,], probs[2,,2,], probs[2,,3,], probs[2,,4,]))
pointsDF<-data.frame(Strategy=c(rep((c("Strategy 1a","Strategy 1b","Strategy 1c",
                                          "Strategy 2a", "Strategy 2b","Strategy 2c",
                                          "Strategy 3")), times=8)),
                     numrel=rep(c(rep(c("10","20","30","40"), each=(7))), times=2),
                     Site=c(rep("HMU", length(c(Meanprobs[1,,]))), 
                            rep("Refuge", length(c(Meanprobs[2,,])))),
                     means=c(Meanprobs[1,1,],Meanprobs[1,2,],Meanprobs[1,3,],Meanprobs[1,4,],
                             Meanprobs[2,1,],Meanprobs[2,2,],Meanprobs[2,3,],Meanprobs[2,4,])
                     )
ggplot(violinDF, aes(x=numrel, y=vals, fill=Site))+geom_violin(scale="width")+
  facet_wrap(Strategy~.)+
  xlab("Initial number of adults")+ylab("Probability of persistence")

violinsSali<-ggplot(violinDF, aes(x=Strategy, y=vals, fill=Strategy, color=Strategy))+
  geom_violin(scale="width", linewidth=0)+
  stat_summary(fun=mean, geom="point", shape=16, size=3, color="dark grey")+
  facet_wrap(numrel~Site, nrow=4)+
  ylab("Probability of persistence")+
  theme_bw()+
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank())+
  scale_fill_manual(limits=c("Strategy 1a","Strategy 1b","Strategy 1c",
                             "Strategy 2a", "Strategy 2b","Strategy 2c",
                             "Strategy 3"), values=cbbPalette)+
  scale_color_manual(limits=c("Strategy 1a","Strategy 1b","Strategy 1c",
                              "Strategy 2a", "Strategy 2b","Strategy 2c",
                              "Strategy 3"), values=cbbPalette)+
  guides(fill = guide_legend(override.aes = list(shape = NA)))
  
violinsSali
ggsave(violinsSali, filename = "results/Simulation output/Figure 5.png",  bg = "white",
       width = 10, height = 8, dpi = 300, units = "in", device='png')


###make violin plots of all the probs
#Koko
probsK<-array(dim=c(2,400000, 4, 7))
for(i in 1:4){
  for(j in 1:7){
    probsK[1,,i,j]<-(1-AllKoko_HMU_K_outs[[i]]$outextprobs[[j]])
    probsK[2,,i,j]<-(1-AllKoko_Refuge_K_outs[[i]]$outextprobs[[j]])
  }
}

violinDFK<-data.frame(Strategy=c(rep(rep(c("Strategy 1a","Strategy 1b","Strategy 1c",
                                           "Strategy 2a", "Strategy 2b","Strategy 2c",
                                           "Strategy 3"), each=400000), times=1)), 
                     numrel=c(rep(c("10","20","30","40"), each=(400000*7))),
                     Site=c(rep("HMU", length(c(probs[1,,,]))), 
                            rep("Refuge", length(c(probs[2,,,])))),
                     vals=c(probsK[1,,1,], probsK[1,,2,], probsK[1,,3,], probsK[1,,4,],
                            probsK[2,,1,], probsK[2,,2,], probsK[2,,3,], probsK[2,,4,]))
ggplot(violinDFK, aes(x=numrel, y=vals, fill=Site))+geom_violin(scale="width")+
  facet_wrap(Scenario~.)+
  xlab("Initial number of adults")+ylab("Probability of persistence")

violinsKoko<-ggplot(violinDFK, aes(x=Strategy, y=vals, fill=Strategy, color=Strategy))+
  geom_violin(scale="width", linewidth=0)+
  stat_summary(fun=mean, geom="point", shape=16, size=3, color="dark grey")+
  facet_wrap(numrel~Site, nrow=4)+
  ylab("Probability of persistence")+
  theme_bw()+
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank())+
  scale_fill_manual(limits=c("Strategy 1a","Strategy 1b","Strategy 1c",
                             "Strategy 2a", "Strategy 2b","Strategy 2c",
                             "Strategy 3"), values=cbbPalette)+
  scale_color_manual(limits=c("Strategy 1a","Strategy 1b","Strategy 1c",
                              "Strategy 2a", "Strategy 2b","Strategy 2c",
                              "Strategy 3"), values=cbbPalette)+
  guides(fill = guide_legend(override.aes = list(shape = NA)))
violinsKoko

ggsave(violinsKoko, filename = "results/Simulation output/Figure 6.png",  bg = "white",
       width = 10, height = 8, dpi = 300, units = "in", device='png')
