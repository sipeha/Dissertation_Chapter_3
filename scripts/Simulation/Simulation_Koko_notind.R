#Ko'ko' simulation code
library(here); library(dplyr); library(nimble)

#read in ee values
KokoParams<-read.csv(here("data","Simulation","Ko'ko'params.csv"))
#read in mixture functions
source(here("scripts", "Functions for sim.R"))



set.seed(3222)
simKoko_FN<-function(nyears, nmales, nfemales, nsims, alternative, site, num_ex, 
                     includeK, K, samps){
  #get parameters
  phiad_temp<-KokoParams %>% filter(KokoParams$Parameter=="Ko'ko' Adult Survival", 
                                    KokoParams$Scenario==alternative, KokoParams$Site==site)
  phiad<-replicate(nsims, rbetamix(alpha=phiad_temp$alphas, beta=phiad_temp$betas, num_ex=5))
  
  phij_temp<-KokoParams %>% filter(KokoParams$Parameter=="Ko'ko' Juvenile Survival", 
                                   KokoParams$Scenario==alternative, KokoParams$Site==site)
  phij<-replicate(nsims, rbetamix(alpha=phij_temp$alphas, beta=phij_temp$betas, num_ex=5))
  
  nestsuccess_temp<-KokoParams %>% filter(KokoParams$Parameter=="Ko'ko' Nest Success", 
                                          KokoParams$Scenario==alternative, KokoParams$Site==site)
  nestsuccess<-replicate(nsims, rbetamix(alpha=nestsuccess_temp$alphas, 
                                         beta=nestsuccess_temp$betas, num_ex=5))
  nestattempts_temp<-KokoParams %>% filter(KokoParams$Parameter=="Ko'ko' Nest Attempts", 
                                           KokoParams$Scenario==alternative, KokoParams$Site==site)
  nestattempts<-replicate(nsims, rgammamix(alpha=nestattempts_temp$alphas, 
                                           beta=nestattempts_temp$betas, num_ex=5))
  hatchlingspnest_temp<-KokoParams %>% filter(KokoParams$Parameter=="Ko'ko' Hatchlings per Nest", 
                                              KokoParams$Scenario==alternative, KokoParams$Site==site)
  hatchlingspnest<-replicate(nsims, rgammamix(alpha=hatchlingspnest_temp$alphas, 
                                              beta=hatchlingspnest_temp$betas, num_ex=5))
  fec<-(nestsuccess*nestattempts*hatchlingspnest)/2
  fecmat<-array(dim=c((nyears+1),nsims, samps ))
  for(i in 1:nsims){
    fecmat[,i,]<-rep(fec[i],samps)
  }
  
  #run simulations
  sims<-array(dim=c(nsims, (nyears+1),samps))
  extinct<-array(dim=c(nsims, (nyears+1),samps))
  for(s in 1:nsims){
    Find<-array(dim=c(2, nyears+1,samps))
    #2 in array: 
    #1: no. F adults,  2:  F Juvs
    Mind<-array(dim=c(2, nyears+1,samps))
    #2 in array: 
    #1: no. M adults,2: M Juvs
    
    Find[1,1,]<-nfemales
    Mind[1,1,]<-nmales
    if(includeK==T){
      for(t in 1:(nyears)){
        fecmat[t,s,]<-ifelse((Find[1,t,]+Mind[1,t,])>=K, 0, fecmat[t,s,])

        Find[2,t,]<-rbinom(samps,rpois(samps,(Find[1,t,]*fecmat[t,s,])), phij[s])
        Mind[2,t,]<-rbinom(samps, rpois(samps,(Mind[1,t,]*fecmat[t,s,])), phij[s])
        
        Find[1,t+1,]<-rbinom(samps, Find[1,t,],phiad[s])+rbinom(samps,Find[2,t,],phiad[s])
        Mind[1,t+1,]<-rbinom(samps, Mind[1,t,], phiad[s])+rbinom(samps, Mind[2,t,], phiad[s])

      }
    }else if (includeK==F){
      for(t in 1:(nyears)){
        fecmat[t,s,]<-ifelse((Find[1,t,]+Mind[1,t,])>=9000, 0, fecmat[t,s,])
        Find[2,t,]<-rbinom(samps,rpois(samps,Find[1,t,]*fecmat[t,s,]), phij[s])
        Mind[2,t,]<-rbinom(samps, rpois(samps,Mind[1,t,]*fecmat[t,s,]), phij[s])
        
        Find[1,t+1,]<-rbinom(samps, Find[1,t,],phiad[s])+rbinom(samps,Find[2,t,],phiad[s])
        Mind[1,t+1,]<-rbinom(samps, Mind[1,t,], phiad[s])+rbinom(samps, Mind[2,t,], phiad[s])
        
      }
    }
    #save adult totals and the instances that went extinct
    for(i in 1:samps){
      extinct[s,,i]<-ifelse((Find[1,,i]+Mind[1,,i])<1, 1, 0)
    }
    sims[s,,]<-Find[1,,]+Mind[1,,]
  
    
  } #sim loop
  out<-list(sims=sims, extinct=extinct)
  return(out)
}



#function to do multiple sims
Koko_sim_byaltFN<-function(nyears, nmales, nfemales, nsims,
                           site, num_ex=5, K, includeK, samps){
  simsbyalt<-NULL
  alternatives<-c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                  "Scenario 2a", "Scenario 2b", "Scenario 2c",
                  "Scenario 3")
  for(i in 1:length(alternatives)){
    simsbyalt[[i]]<-simKoko_FN(nyears=nyears, nmales=nmales, nfemales=nmales, nsims=nsims,
                               alternative=alternatives[i], site=site, num_ex=5, 
                               K=K, includeK=includeK, samps=samps)
    print(alternatives[i])
  }
  return(simsbyalt)
}

nmales<-nfemales<-c(5,10,15,20)
#run from 1 to 40 in different instances (lazy parallel)
simnum<-seq(31,40,by=1)
for(j in simnum[1]:simnum[10]){
  for(i in 1:length(nmales)){
    XH<-Koko_sim_byaltFN(nyears = 20, nmales=nmales[i], nfemales=nfemales[i],nsims=10000,
                         site="HMU",num_ex=5, K=110, includeK=F, samps=50)
    saveRDS(XH, here("SimFiles",paste0("Koko_HMU_noK_24 ",j,"_",nmales[i],".RDS")))
    
    XHK<-Koko_sim_byaltFN(nyears = 20, nmales=nmales[i], nfemales=nfemales[i],nsims=10000,
                          site="HMU",num_ex=5, K=110, includeK=T, samps=50)
    saveRDS(XHK, here("SimFiles",paste0("Koko_HMU_K_24 ",j,"_",nmales[i],".RDS")))
    
    XR<-Koko_sim_byaltFN(nyears = 20, nmales=nmales[i], nfemales=nfemales[i],nsims=10000,
                         site="Refuge",num_ex=5, K=312, includeK=F,samps=50)
    saveRDS(XR, here("SimFiles",paste0("Koko_Refuge_noK_24 ",j,"_",nmales[i],".RDS")))
    
    XRK<-Koko_sim_byaltFN(nyears = 20, nmales=nmales[i], nfemales=nfemales[i],nsims=10000,
                          site="Refuge",num_ex=5, K=312, includeK=T,samps=50)
    saveRDS(XRK, here("SimFiles",paste0("Koko_Refuge_K_24 ",j,"_",nmales[i],".RDS")))
    
    print(j)
  }
}




# 
# extinctarray<-array(dim=c(2,7,21))
# for(i in 1:7){
#   extinctarray[1,i,]<-colSums(Koko_HMU[[i]]$extinct)/100000
#   extinctarray[2,i,]<-colSums(Koko_Refuge[[i]]$extinct)/100000
# }
# 
# 
# 
# extinctDF<-data.frame(scenario=c(rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
#                                        "Scenario 2a", "Scenario 2b", "Scenario 2c",
#                                        "Scenario 3"), times=7*21*2)),
#                       site=c(rep(c("HMU"),7*21), rep(c("Refuge"), 7*21)),
#                       year=c(rep(rep(c(1:21), each=7),2)),
#                       values=c(extinctarray[1,,], extinctarray[2,,])
# )
# 
# 
# 
# extinction11<-ggplot(extinctDF, aes(x=year, y=values, color=scenario))+geom_line()+ylim(c(0,1))+
#   facet_wrap(~site)
# extinction11
# ggsave(extinction11, filename = here("results","Simulation output","Koko_10_extinction_facet sites.png"),
#        width = 8, height = 6, dpi = 300, units = "in", device='png')
# extinction12<-ggplot(extinctDF, aes(x=year, y=values, color=site))+geom_line()+ylim(c(0,1))+
#   facet_wrap(~scenario)
# extinction12
# ggsave(extinction12, filename = here("results","Simulation output","Koko_10_extinction_facet scenario.png"),
#        width = 8, height = 6, dpi = 300, units = "in", device='png')
# 
# 
# HMUoutput<-array(dim=c(4,7,21))
# for(i in 1:7){
#   HMUoutput[1,i,]<-apply(Koko_HMU[[i]]$sims, 2, median)
#   HMUoutput[2,i,]<-apply(Koko_HMU[[i]]$sims, 2, mean)
#   HMUoutput[3,i,]<-apply(Koko_HMU[[i]]$sims, 2, quantile, probs=c(0.05))
#   HMUoutput[4,i,]<-apply(Koko_HMU[[i]]$sims, 2, quantile, probs=c(0.95))
# }
# 
# projHMU<-data.frame(scenario=c(rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
#                                      "Scenario 2a", "Scenario 2b", "Scenario 2c",
#                                      "Scenario 3"), times=21)),
#                     years=c(rep(1:21, each=7)),
#                     site=c(rep("HMU", time=21*7)),
#                     median=c(HMUoutput[1,,]),
#                     mean=c(HMUoutput[2,,]),
#                     lower=c(HMUoutput[3,,]),
#                     upper=c(HMUoutput[4,,])
#                     )
# 
# #need to add in CI and facet
# ggplot(projHMU, aes(x=years, y=median, color=scenario))+geom_line()
# 
# Refoutput<-array(dim=c(4,7,21))
# for(i in 1:7){
#   Refoutput[1,i,]<-apply(Koko_Refuge[[i]]$sims, 2, median)
#   Refoutput[2,i,]<-apply(Koko_Refuge[[i]]$sims, 2, mean)
#   Refoutput[3,i,]<-apply(Koko_Refuge[[i]]$sims, 2, quantile, probs=c(0.05))
#   Refoutput[4,i,]<-apply(Koko_Refuge[[i]]$sims, 2, quantile, probs=c(0.95))
# }
# 
# projRef<-data.frame(scenario=c(rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
#                                      "Scenario 2a", "Scenario 2b", "Scenario 2c",
#                                      "Scenario 3"), times=21)),
#                     years=c(rep(1:21, each=7)),
#                     site=c(rep("Refuge", time=21*7)),
#                     median=c(Refoutput[1,,]),
#                     mean=c(Refoutput[2,,]),
#                     lower=c(Refoutput[3,,]),
#                     upper=c(Refoutput[4,,])
# )
# 
# #need to add in CI and facet
# ggplot(projRef, aes(x=years, y=median, color=scenario))+geom_line()
# 
# 
# # 
# # 
# # extinct<-colSums(sims1$extinct)/nsims
# # plot(extinct)
# # medianS1<-apply(sims1$sims, 2, median)
# # meanS1<-apply(sims1$sims, 2, mean)
# # lS1<-apply(sims1$sims, 2, quantile, probs=c(0.05))
# # uS1<-apply(sims1$sims, 2, quantile, probs=c(0.95))
# # plot(medianS1, ylim=c(0,350))
# # points(meanS1, ylim=c(0,350), pch=2)
# # lines(lS1)
# # lines(uS1)
# # 
# # femaleAdDF1<-data.frame(value=c(sims1$sims),
# #                         year=rep(1:(nyears+1), each=nsims),
# #                         simnum=rep(1:nsims,nyears+1))
# # 
# # 
# # library(ggplot2)
# # #will likely be better to show mean and error because will run 
# # # 100000 to capture expert dists
# # ggplot(femaleAdDF1, aes(x=year, y=value, group=simnum, color=simnum))+
# #   geom_line()+theme(legend.position="none")
# # 
# # 
# # 
# # 
# # ggplot(femaleAdDF2, aes(x=year, y=value, group=simnum, color=simnum))+
# #   geom_line()+theme(legend.position="none")
# # 
# # ggplot(maleAdDF, aes(x=year, y=value, group=simnum, color=(simnum)))+
# #   geom_line()+
# #   # scale_colour_continuous("gradient")+
# #   theme(legend.position="none")#+
# # #scale_color_gradientn(low="purple", high="blue")
# # #scale_color_viridis_b(option = "H")
# # #scale_color_distiller(palette = 'GnBu')
# # 
