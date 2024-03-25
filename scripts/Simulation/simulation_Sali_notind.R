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
        fecmat[s,]<-ifelse((Find[1,t,]+Mind[1,t,])>=K, 0, fecmat[s,])
        Find[2,t,]<-rbinom(samps,rpois(samps,(Find[1,t,]*fecmat[s,])), phifl[s])
        Mind[2,t,]<-rbinom(samps, rpois(samps,(Mind[1,t,]*fecmat[s,])), phifl[s])
        
        Find[3,t,]<-rbinom(samps,Find[2,t,],phij[s])
        Mind[3,t,]<-rbinom(samps,Mind[2,t,],phij[s])
        Find[1,t+1,]<-rbinom(samps, Find[1,t,],phiad[s])+rbinom(samps,Find[3,t,],phiad[s])
        Mind[1,t+1,]<-rbinom(samps, Mind[1,t,], phiad[s])+rbinom(samps, Mind[3,t,], phiad[s])
        
      }
    
    }else if (includeK==F){
      for(t in 1:(nyears)){
        fecmat[s,]<-ifelse((Find[1,t,]+Mind[1,t,])>=9000, 0, fecmat[s,])
        Find[2,t,]<-rbinom(samps,rpois(samps,Find[1,t,]*fecmat[s,]), phifl[s])
        Mind[2,t,]<-rbinom(samps, rpois(samps,Mind[1,t,]*fecmat[s,]), phifl[s])
        
        Find[3,t,]<-rbinom(samps,Find[2,t,],phij[s])
        Mind[3,t,]<-rbinom(samps,Mind[2,t,],phij[s])
        Find[1,t+1,]<-rbinom(samps, Find[1,t,],phiad[s])+rbinom(samps,Find[3,t,],phiad[s])
        Mind[1,t+1,]<-rbinom(samps, Mind[1,t,], phiad[s])+rbinom(samps, Mind[3,t,], phiad[s])
        
      }
    }
    for(i in 1:samps){
      extinct[s,,i]<-ifelse((Find[1,,i]+Mind[1,,i])<1, 1, 0)
    }
    sims[s,,]<-Find[1,,]+Mind[1,,]
    
    
  } #sim loop
  out<-list(sims=sims, extinct=extinct)
  return(out)
}


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
#run from 1 to 40 in different instances (lazy parallel)
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
