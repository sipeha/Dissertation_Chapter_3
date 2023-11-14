#Sali

#process results
#from sims want the mean, median,sd, 95% CI,97.5% CI, low, high, out from the runs
#keep extinction the same
library(here)


#Sali output
Sali_HMU_noK_5<-readRDS("Sali_HMU_noK 5.RDS")
Sali_HMU_noK_10<-readRDS("Sali_HMU_noK 10.RDS")
Sali_HMU_noK_15<-readRDS("Sali_HMU_noK 15.RDS")
Sali_HMU_noK_20<-readRDS("Sali_HMU_noK 20.RDS")

Sali_HMU_K_5<-readRDS("Sali_HMU_K 5.RDS")
Sali_HMU_K_10<-readRDS("Sali_HMU_K 10.RDS")
Sali_HMU_K_15<-readRDS("Sali_HMU_K 15.RDS")
Sali_HMU_K_20<-readRDS("Sali_HMU_K 20.RDS")

Sali_Refuge_noK_5<-readRDS("Sali_Refuge_noK 5.RDS")
Sali_Refuge_noK_10<-readRDS("Sali_Refuge_noK 10.RDS")
Sali_Refuge_noK_15<-readRDS("Sali_Refuge_noK 15.RDS")
Sali_Refuge_noK_20<-readRDS("Sali_Refuge_noK 20.RDS")

Sali_Refuge_K_5<-readRDS("Sali_Refuge_K 5.RDS")
Sali_Refuge_K_10<-readRDS("Sali_Refuge_K 10.RDS")
Sali_Refuge_K_15<-readRDS("Sali_Refuge_K 15.RDS")
Sali_Refuge_K_20<-readRDS("Sali_Refuge_K 20.RDS")



SaliOutputFN<-function(site, K, num){
  if(K==T){
    SaliOutput<-NULL
    Extinction<-NULL
    for(i in 1:7){
      SaliOutput[[i]]<-rbind(apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$sims,2,mean),
                             apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$sims,2,median),
                             apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$sims,2,sd),
                             apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$sims,2,quantile,c(0.025)),
                             apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$sims,2,quantile,c(0.05)),
                             apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$sims,2,quantile,c(0.95)),
                             apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$sims,2,quantile,c(0.975)),
                             apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$sims,2,min),
                             apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$sims,2,max))
      Extinction[[i]]<-apply(get((paste0("Sali_", site, "_K_",num)))[[i]]$extinct, 2, sum)/300000
    }
  }else if (K==F){
    SaliOutput<-NULL
    Extinction<-NULL
    for(i in 1:7){
      SaliOutput[[i]]<-rbind(apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$sims,2,mean),
                             apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$sims,2,median),
                             apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$sims,2,sd),
                             apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$sims,2,quantile,c(0.025)),
                             apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$sims,2,quantile,c(0.05)),
                             apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$sims,2,quantile,c(0.95)),
                             apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$sims,2,quantile,c(0.975)),
                             apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$sims,2,min),
                             apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$sims,2,max))
      Extinction[[i]]<-apply(get((paste0("Sali_", site, "_noK_",num)))[[i]]$extinct, 2, sum)/300000
    }
  }
  out<-list(SaliOutput=SaliOutput, Extinction=Extinction)
  return(out)
}


combineNumsRelFNSali<-function(site, K){
  nums<-c(5,10,15,20)
  fullSaliOut<-NULL
  for(i in 1:length(nums)){
    fullSaliOut[[i]]<-SaliOutputFN(site=site, K=K, num=nums[i])
  }
  return(fullSaliOut)
}


AllSali_HMU_K_outs<-combineNumsRelFNSali(site="HMU", K=T)
saveRDS(AllSali_HMU_K_outs, file="AllSali_HMU_K_outs.RDS")

AllSali_HMU_noK_outs<-combineNumsRelFNSali(site="HMU", K=F)
saveRDS(AllSali_HMU_noK_outs, file="AllSali_HMU_noK_outs.RDS")

AllSali_Refuge_K_outs<-combineNumsRelFNSali(site="Refuge", K=T)
saveRDS(AllSali_Refuge_K_outs, file="AllSali_Refuge_K_outs.RDS")

AllSali_Refuge_noK_outs<-combineNumsRelFNSali(site="Refuge", K=F)
saveRDS(AllSali_Refuge_noK_outs, file="AllSali_Refuge_noK_outs.RDS")






rm(list=ls())

#Koko

#process results
#from sims want the mean, median, sd,95% CI,97.5% CI, low, high, out from the runs
#keep extinction the same


#Sali output
Koko_HMU_noK_5<-readRDS("Koko_HMU_noK 5.RDS")
Koko_HMU_noK_10<-readRDS("Koko_HMU_noK 10.RDS")
Koko_HMU_noK_15<-readRDS("Koko_HMU_noK 15.RDS")
Koko_HMU_noK_20<-readRDS("Koko_HMU_noK 20.RDS")

Koko_HMU_K_5<-readRDS("Koko_HMU_K 5.RDS")
Koko_HMU_K_10<-readRDS("Koko_HMU_K 10.RDS")
Koko_HMU_K_15<-readRDS("Koko_HMU_K 15.RDS")
Koko_HMU_K_20<-readRDS("Koko_HMU_K 20.RDS")

Koko_Refuge_noK_5<-readRDS("Koko_Refuge_noK 5.RDS")
Koko_Refuge_noK_10<-readRDS("Koko_Refuge_noK 10.RDS")
Koko_Refuge_noK_15<-readRDS("Koko_Refuge_noK 15.RDS")
Koko_Refuge_noK_20<-readRDS("Koko_Refuge_noK 20.RDS")

Koko_Refuge_K_5<-readRDS("Koko_Refuge_K 5.RDS")
Koko_Refuge_K_10<-readRDS("Koko_Refuge_K 10.RDS")
Koko_Refuge_K_15<-readRDS("Koko_Refuge_K 15.RDS")
Koko_Refuge_K_20<-readRDS("Koko_Refuge_K 20.RDS")



KokoOutputFN<-function(site, K, num){
  if(K==T){
    KokoOutput<-NULL
    Extinction<-NULL
    for(i in 1:7){
      KokoOutput[[i]]<-rbind(apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$sims,2,mean),
                             apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$sims,2,median),
                             apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$sims,2,sd),
                             apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$sims,2,quantile,c(0.025)),
                             apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$sims,2,quantile,c(0.05)),
                             apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$sims,2,quantile,c(0.95)),
                             apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$sims,2,quantile,c(0.975)),
                             apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$sims,2,min),
                             apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$sims,2,max))
      Extinction[[i]]<-apply(get((paste0("Koko_", site, "_K_",num)))[[i]]$extinct, 2, sum)/300000
    }
  }else if (K==F){
    KokoOutput<-NULL
    Extinction<-NULL
    for(i in 1:7){
      KokoOutput[[i]]<-rbind(apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$sims,2,mean),
                             apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$sims,2,median),
                             apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$sims,2,sd),
                             apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$sims,2,quantile,c(0.025)),
                             apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$sims,2,quantile,c(0.05)),
                             apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$sims,2,quantile,c(0.95)),
                             apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$sims,2,quantile,c(0.975)),
                             apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$sims,2,min),
                             apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$sims,2,max))
      Extinction[[i]]<-apply(get((paste0("Koko_", site, "_noK_",num)))[[i]]$extinct, 2, sum)/300000
    }
  }
  out<-list(KokoOutput=KokoOutput, Extinction=Extinction)
  return(out)
}


combineNumsRelFNKoko<-function(site, K){
  nums<-c(5,10,15,20)
  fullKokoOut<-NULL
  for(i in 1:length(nums)){
    fullKokoOut[[i]]<-KokoOutputFN(site=site, K=K, num=nums[i])
  }
  return(fullKokoOut)
}


AllKoko_HMU_K_outs<-combineNumsRelFNKoko(site="HMU", K=T)
saveRDS(AllKoko_HMU_K_outs, file="AllKoko_HMU_K_outs.RDS")

AllKoko_HMU_noK_outs<-combineNumsRelFNKoko(site="HMU", K=F)
saveRDS(AllKoko_HMU_noK_outs, file="AllKoko_HMU_noK_outs.RDS")

AllKoko_Refuge_K_outs<-combineNumsRelFNKoko(site="Refuge", K=T)
saveRDS(AllKoko_Refuge_K_outs, file="AllKoko_Refuge_K_outs.RDS")

AllKoko_Refuge_noK_outs<-combineNumsRelFNKoko(site="Refuge", K=F)
saveRDS(AllKoko_Refuge_noK_outs, file="AllKoko_Refuge_noK_outs.RDS")



#sensitivity of extinction probability to whether K is included or not
sensitivityKoko_HMU<-sensitivityKoko_Refuge<-matrix(nrow=4, ncol=7)
for(i in 1:4){
  for(j in 1:7){
    sensitivityKoko_HMU[i,j]<-abs(sum(AllKoko_HMU_K_outs[[i]]$Extinction[[j]]-AllKoko_HMU_noK_outs[[i]]$Extinction[[j]]))
    sensitivityKoko_Refuge[i,j]<-abs(sum(AllKoko_Refuge_K_outs[[i]]$Extinction[[j]]-AllKoko_Refuge_noK_outs[[i]]$Extinction[[j]]))
    
  }
}


