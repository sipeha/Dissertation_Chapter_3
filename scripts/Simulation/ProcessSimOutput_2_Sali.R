library(here)


SaliOutputFN<-function(site, K, num){
  
  if(K==T){
    extmeans<-NULL
    extprobs<-NULL
    SaliOutput<-NULL
    allextmeans<-NULL
    allextprobs<-NULL
    allSaliOutput<-NULL
    for(j in 1:40){
      if(j <=10){
        temp<-readRDS(here(paste0("SimFiles/Sali_",site,"_K_2 ", j,"_",num,".RDS")))
      }else if (j>=11 & j<=20){
        temp<-readRDS(here(paste0("SimFiles/Sali_",site,"_K_22 ", j,"_",num,".RDS")))
      }else if(j>=21 & j<=30){
        temp<-readRDS(here(paste0("SimFiles/Sali_",site,"_K_23 ", j,"_",num,".RDS")))
      }else if(j>=31 & j<=40){
        temp<-readRDS(here(paste0("SimFiles/Sali_",site,"_K_24 ", j,"_",num,".RDS")))
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
        SaliOutput[[k]]<-((apply(temp[[k]]$sims,2,c)))
      }
      allextprobs[[j]]<-extprobs
      allextmeans[[j]]<-extmeans
      allSaliOutput[[j]]<-SaliOutput
    }
    outextprobs<-NULL
    outextmeans<-NULL
    outSaliOutput<-NULL
    for(k in 1:7){
      outextprobs[[k]]<-c(allextprobs[[1]][[k]])
      outextmeans[[k]]<-allextmeans[[1]][[k]]
      outSaliOutput[[k]]<-allSaliOutput[[1]][[k]]
      for(j in 2:40){
        outextprobs[[k]]<-c(allextprobs[[j]][[k]],outextprobs[[k]])
        outextmeans[[k]]<-rbind(outextmeans[[k]], allextmeans[[j]][[k]])
        outSaliOutput[[k]]<-rbind(outSaliOutput[[k]],allSaliOutput[[j]][[k]])
      }
    }
    SaliOut<-NULL
    ExtOut<-NULL
    for(k in 1:7){
      SaliOut[[k]]<-rbind(apply(outSaliOutput[[k]],2,mean),
                          apply(outSaliOutput[[k]],2,median),
                          apply(outSaliOutput[[k]],2,sd),
                          apply(outSaliOutput[[k]],2,quantile,c(0.025)),
                          apply(outSaliOutput[[k]],2,quantile,c(0.05)),
                          apply(outSaliOutput[[k]],2,quantile,c(0.95)),
                          apply(outSaliOutput[[k]],2,quantile,c(0.975)),
                          apply(outSaliOutput[[k]],2,min),
                          apply(outSaliOutput[[k]],2,max))
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
    SaliOutput<-NULL
    allextmeans<-NULL
    allextprobs<-NULL
    allSaliOutput<-NULL
    for(j in 1:40){
      if(j <=10){
        temp<-readRDS(here(paste0("SimFiles/Sali_",site,"_noK_2 ", j,"_",num,".RDS")))
      }else if (j>=11 & j<=20){
        temp<-readRDS(here(paste0("SimFiles/Sali_",site,"_noK_22 ", j,"_",num,".RDS")))
      }else if(j>=21 & j<=30){
        temp<-readRDS(here(paste0("SimFiles/Sali_",site,"_noK_23 ", j,"_",num,".RDS")))
      }else if(j>=31 & j<=40){
        temp<-readRDS(here(paste0("SimFiles/Sali_",site,"_noK_24 ", j,"_",num,".RDS")))
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
        SaliOutput[[k]]<-((apply(temp[[k]]$sims,2,c)))
      }
      allextprobs[[j]]<-extprobs
      allextmeans[[j]]<-extmeans
      allSaliOutput[[j]]<-SaliOutput
    }
    outextprobs<-NULL
    outextmeans<-NULL
    outSaliOutput<-NULL
    for(k in 1:7){
      outextprobs[[k]]<-c(allextprobs[[1]][[k]])
      outextmeans[[k]]<-allextmeans[[1]][[k]]
      outSaliOutput[[k]]<-allSaliOutput[[1]][[k]]
      for(j in 2:40){
        outextprobs[[k]]<-c(allextprobs[[j]][[k]],outextprobs[[k]])
        outextmeans[[k]]<-rbind(outextmeans[[k]], allextmeans[[j]][[k]])
        outSaliOutput[[k]]<-rbind(outSaliOutput[[k]],allSaliOutput[[j]][[k]])
      }
    }
    SaliOut<-NULL
    ExtOut<-NULL
    for(k in 1:7){
      SaliOut[[k]]<-rbind(apply(outSaliOutput[[k]],2,mean),
                          apply(outSaliOutput[[k]],2,median),
                          apply(outSaliOutput[[k]],2,sd),
                          apply(outSaliOutput[[k]],2,quantile,c(0.025)),
                          apply(outSaliOutput[[k]],2,quantile,c(0.05)),
                          apply(outSaliOutput[[k]],2,quantile,c(0.95)),
                          apply(outSaliOutput[[k]],2,quantile,c(0.975)),
                          apply(outSaliOutput[[k]],2,min),
                          apply(outSaliOutput[[k]],2,max))
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
  out<-list(SaliOut=SaliOut, ExtOut=ExtOut, outextprobs=outextprobs)
  return(out)
}


combineNumsRelFNSali<-function(site, K){
  nums<-c(5,10,15,20)
  fullSaliOut<-NULL
  for(i in 1:length(nums)){
    fullSaliOut[[i]]<-SaliOutputFN(site=site, K=K,num=nums[i])
  }
  return(fullSaliOut)
}

AllSali_HMU_K_outs<-combineNumsRelFNSali(site="HMU", K=T)
saveRDS(AllSali_HMU_K_outs, file="AllSali_HMU_K_outs_2.RDS")
rm(AllSali_HMU_K_outs)

AllSali_HMU_noK_outs<-combineNumsRelFNSali(site="HMU", K=F)
saveRDS(AllSali_HMU_noK_outs, file="AllSali_HMU_noK_outs_2.RDS")
rm(AllSali_HMU_noK_outs)

AllSali_Refuge_K_outs<-combineNumsRelFNSali(site="Refuge", K=T)
saveRDS(AllSali_Refuge_K_outs, file="AllSali_Refuge_K_outs_2.RDS")

AllSali_Refuge_noK_outs<-combineNumsRelFNSali(site="Refuge", K=F)
saveRDS(AllSali_Refuge_noK_outs, file="AllSali_Refuge_noK_outs_2.RDS")

