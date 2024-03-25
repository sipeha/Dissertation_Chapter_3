library(here)


KokoOutputFN<-function(site, K, num){
  
  if(K==T){
    extmeans<-NULL
    extprobs<-NULL
    KokoOutput<-NULL
    allextmeans<-NULL
    allextprobs<-NULL
    allKokoOutput<-NULL
    for(j in 1:40){
      if(j <=10){
        temp<-readRDS(here(paste0("SimFiles/Koko_",site,"_K_2 ", j,"_",num,".RDS")))
      }else if (j>=11 & j<=20){
        temp<-readRDS(here(paste0("SimFiles/Koko_",site,"_K_22 ", j,"_",num,".RDS")))
      }else if(j>=21 & j<=30){
        temp<-readRDS(here(paste0("SimFiles/Koko_",site,"_K_23 ", j,"_",num,".RDS")))
      }else if(j>=31 & j<=40){
        temp<-readRDS(here(paste0("SimFiles/Koko_",site,"_K_24 ", j,"_",num,".RDS")))
      }
      extinction<-array(dim=c(7,10000,21))
      exprobs<-array(dim=c(7,10000))
      
      for(k in 1:7){
        for(i in 1:10000){
          extinction[k,i,]<-rowSums(temp[[k]]$extinct[i,,])/50
          exprobs[k,i]<-length(which(temp[[k]]$sims[i,21,]==0))/50 #probability of persistence
        }
        extmeans[[k]]<-extinction[k,,]
        extprobs[[k]]<-c(exprobs[k,])
        KokoOutput[[k]]<-((apply(temp[[k]]$sims,2,c)))
      }
      allextprobs[[j]]<-extprobs
      allextmeans[[j]]<-extmeans
      allKokoOutput[[j]]<-KokoOutput
    }
    outextprobs<-NULL
    outextmeans<-NULL
    outKokoOutput<-NULL
    for(k in 1:7){
      outextprobs[[k]]<-c(allextprobs[[1]][[k]])
      outextmeans[[k]]<-allextmeans[[1]][[k]]
      outKokoOutput[[k]]<-allKokoOutput[[1]][[k]]
      for(j in 2:40){
        outextprobs[[k]]<-c(allextprobs[[j]][[k]],outextprobs[[k]])
        outextmeans[[k]]<-rbind(outextmeans[[k]], allextmeans[[j]][[k]])
        outKokoOutput[[k]]<-rbind(outKokoOutput[[k]],allKokoOutput[[j]][[k]])
      }
    }
    KokoOut<-NULL
    ExtOut<-NULL
    for(k in 1:7){
      KokoOut[[k]]<-rbind(apply(outKokoOutput[[k]],2,mean),
                          apply(outKokoOutput[[k]],2,median),
                          apply(outKokoOutput[[k]],2,sd),
                          apply(outKokoOutput[[k]],2,quantile,c(0.025)),
                          apply(outKokoOutput[[k]],2,quantile,c(0.05)),
                          apply(outKokoOutput[[k]],2,quantile,c(0.95)),
                          apply(outKokoOutput[[k]],2,quantile,c(0.975)),
                          apply(outKokoOutput[[k]],2,min),
                          apply(outKokoOutput[[k]],2,max))
      ExtOut[[k]]<-rbind(apply(outextmeans[[k]],2,mean),
                         apply(outextmeans[[k]],2,median),
                         apply(outextmeans[[k]],2,sd),
                         apply(outextmeans[[k]],2,quantile,c(0.025)),
                         apply(outextmeans[[k]],2,quantile,c(0.05)),
                         apply(outextmeans[[k]],2,quantile,c(0.95)),
                         apply(outextmeans[[k]],2,quantile,c(0.975)),
                         apply(outextmeans[[k]],2,min),
                         apply(outextmeans[[k]],2,max))
    }
    
  }else if(K==F){
    extmeans<-NULL
    extprobs<-NULL
    KokoOutput<-NULL
    allextmeans<-NULL
    allextprobs<-NULL
    allKokoOutput<-NULL
    for(j in 1:40){
      if(j <=10){
        temp<-readRDS(here(paste0("SimFiles/Koko_",site,"_noK_2 ", j,"_",num,".RDS")))
      }else if (j>=11 & j<=20){
        temp<-readRDS(here(paste0("SimFiles/Koko_",site,"_noK_22 ", j,"_",num,".RDS")))
      }else if(j>=21 & j<=30){
        temp<-readRDS(here(paste0("SimFiles/Koko_",site,"_noK_23 ", j,"_",num,".RDS")))
      }else if(j>=31 & j<=40){
        temp<-readRDS(here(paste0("SimFiles/Koko_",site,"_noK_24 ", j,"_",num,".RDS")))
      }
      extinction<-array(dim=c(7,10000,21))
      exprobs<-array(dim=c(7,10000))
      
      for(k in 1:7){
        for(i in 1:10000){
          extinction[k,i,]<-rowSums(temp[[k]]$extinct[i,,])/50
          exprobs[k,i]<-length(which(temp[[k]]$sims[i,21,]==0))/50
        }
        extmeans[[k]]<-extinction[k,,]
        extprobs[[k]]<-c(exprobs[k,])
        KokoOutput[[k]]<-((apply(temp[[k]]$sims,2,c)))
      }
      allextprobs[[j]]<-extprobs
      allextmeans[[j]]<-extmeans
      allKokoOutput[[j]]<-KokoOutput
    }
    outextprobs<-NULL
    outextmeans<-NULL
    outKokoOutput<-NULL
    for(k in 1:7){
      outextprobs[[k]]<-c(allextprobs[[1]][[k]])
      outextmeans[[k]]<-allextmeans[[1]][[k]]
      outKokoOutput[[k]]<-allKokoOutput[[1]][[k]]
      for(j in 2:40){
        outextprobs[[k]]<-c(allextprobs[[j]][[k]],outextprobs[[k]])
        outextmeans[[k]]<-rbind(outextmeans[[k]], allextmeans[[j]][[k]])
        outKokoOutput[[k]]<-rbind(outKokoOutput[[k]],allKokoOutput[[j]][[k]])
      }
    }
    KokoOut<-NULL
    ExtOut<-NULL
    for(k in 1:7){
      KokoOut[[k]]<-rbind(apply(outKokoOutput[[k]],2,mean),
                          apply(outKokoOutput[[k]],2,median),
                          apply(outKokoOutput[[k]],2,sd),
                          apply(outKokoOutput[[k]],2,quantile,c(0.025)),
                          apply(outKokoOutput[[k]],2,quantile,c(0.05)),
                          apply(outKokoOutput[[k]],2,quantile,c(0.95)),
                          apply(outKokoOutput[[k]],2,quantile,c(0.975)),
                          apply(outKokoOutput[[k]],2,min),
                          apply(outKokoOutput[[k]],2,max))
      ExtOut[[k]]<-rbind(apply(outextmeans[[k]],2,mean),
                         apply(outextmeans[[k]],2,median),
                         apply(outextmeans[[k]],2,sd),
                         apply(outextmeans[[k]],2,quantile,c(0.025)),
                         apply(outextmeans[[k]],2,quantile,c(0.05)),
                         apply(outextmeans[[k]],2,quantile,c(0.95)),
                         apply(outextmeans[[k]],2,quantile,c(0.975)),
                         apply(outextmeans[[k]],2,min),
                         apply(outextmeans[[k]],2,max))
    }
  }
  out<-list(KokoOut=KokoOut, ExtOut=ExtOut, outextprobs=outextprobs)
  return(out)
}


combineNumsRelFNKoko<-function(site, K){
  nums<-c(5,10,15,20)
  fullKokoOut<-NULL
  for(i in 1:length(nums)){
    fullKokoOut[[i]]<-KokoOutputFN(site=site, K=K,num=nums[i])
  }
  return(fullKokoOut)
}

AllKoko_HMU_K_outs<-combineNumsRelFNKoko(site="HMU", K=T)
saveRDS(AllKoko_HMU_K_outs, file="AllKoko_HMU_K_outs_2.RDS")


AllKoko_HMU_noK_outs<-combineNumsRelFNKoko(site="HMU", K=F)
saveRDS(AllKoko_HMU_noK_outs, file="AllKoko_HMU_noK_outs_2.RDS")


AllKoko_Refuge_K_outs<-combineNumsRelFNKoko(site="Refuge", K=T)
saveRDS(AllKoko_Refuge_K_outs, file="AllKoko_Refuge_K_outs_2.RDS")


AllKoko_Refuge_noK_outs<-combineNumsRelFNKoko(site="Refuge", K=F)
saveRDS(AllKoko_Refuge_noK_outs, file="AllKoko_Refuge_noK_outs_2.RDS")






