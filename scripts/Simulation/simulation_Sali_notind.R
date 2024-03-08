#Sali simulation code
library(here); library(dplyr); library(nimble)

#read in ee values
SaliParams<-read.csv(here("data","Simulation","Saliparams.csv"))
#read in mixture functions
source(here("scripts", "Functions for sim.R"))



set.seed(4888)

simSali_FN<-function(nyears, nmales, nfemales, nsims, alternative, site, num_ex, includeK, 
                     K, samps){
  #get parameters
  phiad_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Adult Survival", 
                                    SaliParams$Scenario==alternative, SaliParams$Site==site)
  phiad<-replicate(nsims, rbetamix(alpha=phiad_temp$alphas, beta=phiad_temp$betas, num_ex=5))
  
  phifl_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Fledgling Survival", 
                                    SaliParams$Scenario==alternative, SaliParams$Site==site)
  phifl<-replicate(nsims, rbetamix(alpha=phifl_temp$alphas, beta=phifl_temp$betas, num_ex=5))
  
  phij_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Juvenile Survival", 
                                   SaliParams$Scenario==alternative, SaliParams$Site==site)
  phij<-replicate(nsims, rbetamix(alpha=phij_temp$alphas, beta=phij_temp$betas, num_ex=5))
  
  nestsuccess_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Nest Success", 
                                          SaliParams$Scenario==alternative, SaliParams$Site==site)
  nestsuccess<-replicate(nsims, rbetamix(alpha=nestsuccess_temp$alphas, 
                                         beta=nestsuccess_temp$betas, num_ex=5))
  nestattempts_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Nest Attempts", 
                                           SaliParams$Scenario==alternative, SaliParams$Site==site)
  nestattempts<-replicate(nsims, rgammamix(alpha=nestattempts_temp$alphas, 
                                           beta=nestattempts_temp$betas, num_ex=5))
  fledglingspnest_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Fledglings per Nest", 
                                              SaliParams$Scenario==alternative, SaliParams$Site==site)
  fledglingspnest<-replicate(nsims, rgammamix(alpha=fledglingspnest_temp$alphas, 
                                              beta=fledglingspnest_temp$betas, num_ex=5))
  fec<-(nestsuccess*nestattempts*fledglingspnest)/2
  fecmat<-matrix(nrow=nsims, ncol=samps)
  for(i in 1:nsims){
    fecmat[i,]<-rep(fec[i],samps)
  }
  #run simulations
  #sims<-NULL
  sims<-array(dim=c(nsims, (nyears+1),samps))
  extinct<-array(dim=c(nsims, (nyears+1),samps))
  for(s in 1:nsims){
    Find<-array(dim=c(3, nyears+1,samps))
    #3 in array: 
    #1: no. F adults,  2: F Fl, 3: F Juvs
    Mind<-array(dim=c(3, nyears+1,samps))
    #3 in array: 
    #1: no. M adults,2: M Fl, 3:  M Juvs
    
    Find[1,1,]<-nfemales
    Mind[1,1,]<-nmales
    if(includeK==T){
      for(t in 1:(nyears)){
        #for(i in 1:samps){
        fecmat[s,]<-ifelse((Find[1,t,]+Mind[1,t,])>=K, 0, fecmat[s,])
        #if((Find[1,t,i]+Mind[1,t,i])>=K){
        Find[2,t,]<-rbinom(samps,rpois(samps,(Find[1,t,]*fecmat[s,])), phifl[s])
        Mind[2,t,]<-rbinom(samps, rpois(samps,(Mind[1,t,]*fecmat[s,])), phifl[s])
        
        Find[3,t,]<-rbinom(samps,Find[2,t,],phij[s])
        Mind[3,t,]<-rbinom(samps,Mind[2,t,],phij[s])
        Find[1,t+1,]<-rbinom(samps, Find[1,t,],phiad[s])+rbinom(samps,Find[3,t,],phiad[s])
        Mind[1,t+1,]<-rbinom(samps, Mind[1,t,], phiad[s])+rbinom(samps, Mind[3,t,], phiad[s])
        #}else{
        # Find[2,t,i]<-rbinom(1,rpois(1,Find[1,t,i]*fec[s]), phifl[s])
        # Mind[2,t,i]<-rbinom(1, rpois(1,Mind[1,t,i]*fec[s]), phifl[s])
        # 
        # Find[3,t,i]<-rbinom(1,Find[2,t,i],phij[s])
        # Mind[3,t,i]<-rbinom(1,Mind[2,t,i],phij[s])
        # Find[1,t+1,i]<-rbinom(1, Find[1,t,i],phiad[s])+rbinom(1,Find[3,t,i],phiad[s])
        # Mind[1,t+1,i]<-rbinom(1, Mind[1,t,i], phiad[s])+rbinom(1, Mind[3,t,i], phiad[s])
        #}
        
      }
      #}
    }else if (includeK==F){
      for(t in 1:(nyears)){
        #for(i in 1:samps){
        fecmat[s,]<-ifelse((Find[1,t,]+Mind[1,t,])>=9000, 0, fecmat[s,])
        #if((Find[1,t,i]+Mind[1,t,i])>=3000){fec[s]=0}
        Find[2,t,]<-rbinom(samps,rpois(samps,Find[1,t,]*fecmat[s,]), phifl[s])
        Mind[2,t,]<-rbinom(samps, rpois(samps,Mind[1,t,]*fecmat[s,]), phifl[s])
        
        Find[3,t,]<-rbinom(samps,Find[2,t,],phij[s])
        Mind[3,t,]<-rbinom(samps,Mind[2,t,],phij[s])
        Find[1,t+1,]<-rbinom(samps, Find[1,t,],phiad[s])+rbinom(samps,Find[3,t,],phiad[s])
        Mind[1,t+1,]<-rbinom(samps, Mind[1,t,], phiad[s])+rbinom(samps, Mind[3,t,], phiad[s])
        
      }
    }
    #}
    #save adult totals and the instances that went extinct
    for(i in 1:samps){
      extinct[s,,i]<-ifelse((Find[1,,i]+Mind[1,,i])<1, 1, 0)
    }
    sims[s,,]<-Find[1,,]+Mind[1,,]
    
    #calculate population growth rate, include that too?
    
  } #sim loop
  out<-list(sims=sims, extinct=extinct)
  return(out)
}

#to test it
# simSali_FN(nyears=10, nmales=10,nfemales=10,alternative="Scenario 1a",
#            site="HMU", num_ex=5, includeK = F, K=289, nsims=100, samps=100)$sims

#function to do multiple sims
Sali_sim_byaltFN<-function(nyears, nmales, nfemales, nsims,
                           site, num_ex=5, K, includeK, samps){
  simsbyalt<-NULL
  alternatives<-c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                  "Scenario 2a", "Scenario 2b", "Scenario 2c",
                  "Scenario 3")
  for(i in 1:length(alternatives)){
    simsbyalt[[i]]<-simSali_FN(nyears=nyears, nmales=nmales, nfemales=nmales, nsims=nsims,
                               alternative=alternatives[i], site=site, num_ex=5, 
                               K=K, includeK=includeK, samps=samps)
    print(alternatives[i])
  }
  return(simsbyalt)
}

nmales<-nfemales<-c(5,10,15,20)
#loop over sims from 1 to 4
simnum<-seq(31,40,by=1)
for(j in simnum[1]:simnum[10]){
  for(i in 1:length(nmales)){
    XH<-Sali_sim_byaltFN(nyears = 20, nmales=nmales[i], nfemales=nfemales[i],nsims=10000,
                         site="HMU",num_ex=5, K=327, includeK=F, samps=50)
    saveRDS(XH, here("SimFiles",paste0("Sali_HMU_noK_24 ",j,"_",nmales[i],".RDS")))
    XHK<-Sali_sim_byaltFN(nyears = 20, nmales=nmales[i], nfemales=nfemales[i],nsims=10000,
                          site="HMU",num_ex=5, K=327, includeK=T, samps=50)
    saveRDS(XHK, here("SimFiles",paste0("Sali_HMU_K_24 ",j,"_",nmales[i],".RDS")))
    
    XR<-Sali_sim_byaltFN(nyears = 20, nmales=nmales[i], nfemales=nfemales[i],nsims=10000,
                         site="Refuge",num_ex=5, K=927, includeK=F,samps=50)
    saveRDS(XR, here("SimFiles",paste0("Sali_Refuge_noK_24 ",j,"_",nmales[i],".RDS")))
    
    XRK<-Sali_sim_byaltFN(nyears = 20, nmales=nmales[i], nfemales=nfemales[i],nsims=10000,
                          site="Refuge",num_ex=5, K=927, includeK=T,samps=50)
    saveRDS(XRK, here("SimFiles",paste0("Sali_Refuge_K_24 ",j,"_",nmales[i],".RDS")))
    
    print(j)
  }
}

# Sali_sim_byrelnumFN<-function(nyears, nmales, nfemales, nsims, 
#                               site, num_ex=5, K, includeK){
#   SimsbyNum<-NULL
#   for(i in 1:length(nmales)){
#     SimsbyNum[[i]]<-Sali_sim_byaltFN(nyears=nyears, nmales=nmales[i],nfemales=nfemales[i],
#                                      nsims=nsims, site=site, num_ex=5, K=K, includeK=includeK)
#   }
#   return(SimsbyNum)
# }

library(beepr)
#~ K for HMU is 285
#~K for Refuge is 811
# Sali_HMU_noK<-Sali_sim_byrelnumFN(nyears=20, nmales=c(5,10,15,20), nfemales=c(5,10,15,20), 
#                                   nsims=300000, 
#                        site="HMU",num_ex=5, K=286, includeK=F)
# Sali_HMU_K<-Sali_sim_byrelnumFN(nyears=20, nmales=c(5,10,15,20), nfemales=c(5,10,15,20), 
#                                   nsims=300000, 
#                                   site="HMU",num_ex=5, K=286, includeK=T)
# 
# Sali_Refuge_noK<-Sali_sim_byrelnumFN(nyears=20, nmales=c(5,10,15,20), nfemales=c(5,10,15,20), 
#                                   nsims=300000, 
#                                   site="Refuge",num_ex=5, K=811, includeK=F)
# Sali_Refuge_K<-Sali_sim_byrelnumFN(nyears=20, nmales=c(5,10,15,20), nfemales=c(5,10,15,20), 
#                                 nsims=300000, 
#                                 site="Refuge",num_ex=5, K=811, includeK=T)

beep(sound=6)


extinctarrayK<-array(dim=c(2,7,21))
extinctarraynoK<-array(dim=c(2,7,21))
for(i in 1:7){
  extinctarrayK[1,i,]<-colSums(Sali_HMU_K[[1]][[i]]$extinct)/300000
  extinctarrayK[2,i,]<-colSums(Sali_Refuge_noK[[1]][[i]]$extinct)/300000
  extinctarraynoK[1,i,]<-colSums(Sali_HMU_noK[[1]][[i]]$extinct)/300000
  extinctarraynoK[2,i,]<-colSums(Sali_Refuge_K[[1]][[i]]$extinct)/300000
}



extinctDF<-data.frame(scenario=c(rep(c("Scenario 1a", "Scenario 1b", "Scenario 1c",
                                       "Scenario 2a", "Scenario 2b", "Scenario 2c",
                                       "Scenario 3"), times=7*21*2)),
                      site=c(rep(c("HMU"),7*21), rep(c("Refuge"), 7*21)),
                      year=c(rep(rep(c(1:21), each=7),2)),
                      values=c(extinctarrayK[1,,], extinctarrayK[2,,])
)


library(ggplot2)
extinction11<-ggplot(extinctDF, aes(x=year, y=values, color=site))+geom_line()+ylim(c(0,1))+
  facet_wrap(~scenario)
extinction11
ggsave(extinction11, filename = here("results","Simulation output","Sali_10_extinction_facet scenario.png"),
       width = 8, height = 6, dpi = 300, units = "in", device='png')

extinction12<-ggplot(extinctDF, aes(x=year, y=values, color=scenario))+geom_line()+ylim(c(0,1))+
  facet_wrap(~site)
extinction12
ggsave(extinction12, filename = here("results","Simulation output","Sali_10_extinction_facet site.png"),
       width = 8, height = 6, dpi = 300, units = "in", device='png')




plot(((1-extinctarrayK[1,1,21])-seq(0,1,0.01))/(1-extinctarrayK[1,1,21]), seq(0,1,0.01), xlim=c(0,1), 
     pch=16,col="lightblue")
points(((1-extinctarrayK[1,2,21])-seq(0,1,0.01))/(1-extinctarrayK[1,2,21]), seq(0,1,0.01), xlim=c(0,1), 
       pch=16,col="pink")
points(((1-extinctarrayK[1,3,21])-seq(0,1,0.01))/(1-extinctarrayK[1,3,21]), seq(0,1,0.01), xlim=c(0,1), 
       pch=16,col="lightgreen")
points(((1-extinctarrayK[1,4,21])-seq(0,1,0.01))/(1-extinctarrayK[1,4,21]), seq(0,1,0.01), xlim=c(0,1), 
       pch=16,col="plum")
points(((1-extinctarrayK[1,5,21])-seq(0,1,0.01))/(1-extinctarrayK[1,5,21]), seq(0,1,0.01), xlim=c(0,1), 
       pch=16,col="orange")
points(((1-extinctarrayK[1,6,21])-seq(0,1,0.01))/(1-extinctarrayK[1,6,21]), seq(0,1,0.01), xlim=c(0,1), 
       pch=16,col="yellow")
points(((1-extinctarrayK[1,7,21])-seq(0,1,0.01))/(1-extinctarrayK[1,7,21]), seq(0,1,0.01), xlim=c(0,1), 
       pch=16,col="purple")
abline(h=0.5)


# 
# 
# 
# extinct<-colSums(sims1$extinct)/nsims
# plot(extinct)
# medianS1<-apply(sims1$sims, 2, median)
# meanS1<-apply(sims1$sims, 2, mean)
# lS1<-apply(sims1$sims, 2, quantile, probs=c(0.05))
# uS1<-apply(sims1$sims, 2, quantile, probs=c(0.95))
# plot(medianS1, ylim=c(0,350))
# points(meanS1, ylim=c(0,350), pch=2)
# lines(lS1)
# lines(uS1)
# 
# femaleAdDF1<-data.frame(value=c(sims1$sims),
#                         year=rep(1:(nyears+1), each=nsims),
#                         simnum=rep(1:nsims,nyears+1))
# 
# 
# library(ggplot2)
# #will likely be better to show mean and error because will run 
# # 100000 to capture expert dists
# ggplot(femaleAdDF1, aes(x=year, y=value, group=simnum, color=simnum))+
#   geom_line()+theme(legend.position="none")
# 
# 
# 
# 
# ggplot(femaleAdDF2, aes(x=year, y=value, group=simnum, color=simnum))+
#   geom_line()+theme(legend.position="none")
# 
# ggplot(maleAdDF, aes(x=year, y=value, group=simnum, color=(simnum)))+
#   geom_line()+
#   # scale_colour_continuous("gradient")+
#   theme(legend.position="none")#+
# #scale_color_gradientn(low="purple", high="blue")
# #scale_color_viridis_b(option = "H")
# #scale_color_distiller(palette = 'GnBu')
# 
