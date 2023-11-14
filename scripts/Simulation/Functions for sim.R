
rbetamix<-function(alpha, beta, num_ex){
  
  ex<-rcat(1, rep(1/num_ex, num_ex))
  x=rbeta(1, alpha[ex], beta[ex])
  
  return(x)
  
}


rgammamix<-function(alpha, beta, num_ex){
  
  ex<-rcat(1, rep(1/num_ex, num_ex))
  x=rgamma(1, alpha[ex], rate=beta[ex])
  
  return(x)
  
}

#TODO: add in final function here

# #put Sali function sim in here too
# simSali_FN<-function(nyears, nmales, nfemales, nsims, alternative, site, num_ex){
#   #get parameters
#   phiad_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Adult Survival", 
#                                     SaliParams$Scenario==alternative, SaliParams$Site==site)
#   phiad<-replicate(nsims, rbetamix(alpha=phiad_temp$alphas, beta=phiad_temp$betas, num_ex=5))
#   
#   phifl_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Fledgling Survival", 
#                                     SaliParams$Scenario==alternative, SaliParams$Site==site)
#   phifl<-replicate(nsims, rbetamix(alpha=phifl_temp$alphas, beta=phifl_temp$betas, num_ex=5))
#   
#   phij_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Juvenile Survival", 
#                                    SaliParams$Scenario==alternative, SaliParams$Site==site)
#   phij<-replicate(nsims, rbetamix(alpha=phij_temp$alphas, beta=phij_temp$betas, num_ex=5))
#   
#   nestsuccess_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Nest Success", 
#                                           SaliParams$Scenario==alternative, SaliParams$Site==site)
#   nestsuccess<-replicate(nsims, rbetamix(alpha=nestsuccess_temp$alphas, 
#                                          beta=nestsuccess_temp$betas, num_ex=5))
#   nestattempts_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Nest Attempts", 
#                                            SaliParams$Scenario==alternative, SaliParams$Site==site)
#   nestattempts<-replicate(nsims, rgammamix(alpha=nestattempts_temp$alphas, 
#                                            beta=nestattempts_temp$betas, num_ex=5))
#   fledglingspnest_temp<-SaliParams %>% filter(SaliParams$Parameter=="Sali Fledglings per Nest", 
#                                               SaliParams$Scenario==alternative, SaliParams$Site==site)
#   fledglingspnest<-replicate(nsims, rgammamix(alpha=fledglingspnest_temp$alphas, 
#                                               beta=fledglingspnest_temp$betas, num_ex=5))
#   fec<-(nestsuccess*nestattempts*fledglingspnest)/2
#   
#   #run simulations
#   sims<-NULL
#   for(s in 1:nsims){
#     maxind<-nfemales*nmales*20
#     Find<-array(dim=c(5, nyears+1, maxind))
#     #5 in array: 
#     #1: no. F adults, 2: no. female chicks produced, 3: F Fl, 4: F Juvs
#     Mind<-array(dim=c(5, nyears+1, maxind))
#     #5 in array: 
#     #1: no. M adults, 2: no. M chicks produced, 3: M Fl, 4:  M Juvs
#     
#     Find[1,1,1:nfemales]<-1
#     Mind[1,1,1:nmales]<-1
#     tempind<-array(dim=c(2,nyears, maxind)) #2: 1=females, 2=males
#     inpop<-array(dim=c(2,nyears+1)) #2: 1=females, 2=males
#     inpopsub<-array(dim=c(2,nyears+1))
#     inpop[1,1]<-nfemales
#     inpop[2,1]<-nmales
#     for(t in 1:nyears){
#       for(i in 1:inpop[1,t]){
#         tempind[1,t,i]<-which(Find[c(1,5),t,i]==1)
#         if(tempind[1,t,i]==1){ #if adult female
#           Find[2,t,i]<-rpois(1,fec)
#           survF<-rbinom(1,1,phiad)
#           Find[1,t+1,i]<-ifelse(survF==1, 1, 0)
#           Find[3:4,t+1,i]<-NA
#           Find[5,t+1,i]<-ifelse(survF==0, 1, NA)
#         } else if(tempind[1,t,i]==2){ #they are dead
#           Find[1:4,t+1,i]<-NA
#           Find[5,t+1,i]<-1
#         }
#       }
#       for(i in 1:inpop[2,t]){
#         tempind[2,t,i]<-which(Mind[c(1,5),t,i]==1)
#         if(tempind[2,t,i]==1){ #if adult male
#           Mind[2,t,i]<-rpois(1,fec)
#           survM<-rbinom(1,1,phiad)
#           Mind[1,t+1,i]<-ifelse(survM==1, 1, 0)
#           Mind[3:4,t+1,i]<-NA
#           Mind[5,t+1,i]<-ifelse(survM==0, 1, NA)
#         } else if(tempind[2,t,i]==2){ #they are dead
#           Mind[1:4,t+1,i]<-NA
#           Mind[5,t+1,i]<-1
#         }
#       }#i loop for adult fate
#       #add in the offspring from current year, see if they survive to 
#       # be recruited to adults
#       inpopsub[1,t]<-sum(Find[2,t,1:inpop[1,t]], na.rm=T)
#       inpopsub[2,t]<-sum(Mind[2,t,1:inpop[2,t]],na.rm=T)
#       if(inpopsub[1,t]>0){
#         Find[3,t,(inpop[1,t]+1):(inpopsub[1,t]+inpop[1,t])]<-1
#         for(i in (inpop[1,t]+1):(inpopsub[1,t]+inpop[1,t])){
#           survFF<-rbinom(1,1,phifl)
#           Find[3,t,i]<-ifelse(survFF==1, 1, 0)
#           survFJ<-rbinom(1,1,phij)
#           Find[4,t,i]<-ifelse(survFJ==1 & Find[3,t,i]==1, 1, 0)
#           survFA<-rbinom(1,1,phiad)
#           Find[1,t+1,i]<-ifelse(survFA==1 & Find[4,t,i]==1, 1, 0)
#           Find[5,t,i]<-ifelse(survFA==0 | Find[4,t,i]==0, 1,NA)
#           Find[5,t+1,i]<-ifelse(survFA==0 | Find[4,t,i]==0, 1,NA)
#         }
#       }
#       if(inpopsub[2,t]>0){
#         Mind[3,t,(inpop[2,t]+1):(inpopsub[2,t]+inpop[2,t])]<-1
#         for(i in (inpop[2,t]+1):(inpopsub[2,t]+inpop[2,t])){
#           survFM<-rbinom(1,1,phifl)
#           Mind[3,t,i]<-ifelse(survFM==1, 1, 0)
#           survMJ<-rbinom(1,1,phij)
#           Mind[4,t,i]<-ifelse(survMJ==1 & Mind[3,t,i]==1, 1, 0)
#           survMA<-rbinom(1,1,phiad)
#           Mind[1,t+1,i]<-ifelse(survMA==1 & Mind[4,t,i]==1, 1, 0)
#           Mind[5,t,i]<-ifelse(survMA==0 | Mind[4,t,i]==0, 1,NA)
#           Mind[5,t+1,i]<-ifelse(survMA==0 | Mind[4,t,i]==0, 1,NA)
#         }
#       }
#       inpop[1,t+1]<-sum(inpop[1,t],inpopsub[1,t])
#       inpop[2,t+1]<-sum(inpop[2,t],inpopsub[2,t])
#     }
#     sims[[s]]<-list(Find=Find, Mind=Mind)
#   }
#   
#   return(sims)
# }
# 
# #placeholder for sim functions, might need to replace if there are tweaks
# simKoko_FN<-function(nyears, nmales, nfemales, nsims, alternative, site,
#                      hyperpriors){
#   
#   #get parameters
#   phiad_temp<-SaliParams %>% filter(SaliParams$Parameter=="Ko'ko' Adult Survival", 
#                                     SaliParams$Scenario==alternative, SaliParams$Site==site)
#   phiad<-replicate(nsims, rbetamix(alpha=phiad_temp$alphas, beta=phiad_temp$betas, num_ex=5))
#   
#   phij_temp<-SaliParams %>% filter(SaliParams$Parameter=="Ko'ko' Juvenile Survival", 
#                                    SaliParams$Scenario==alternative, SaliParams$Site==site)
#   phij<-replicate(nsims, rbetamix(alpha=phij_temp$alphas, beta=phij_temp$betas, num_ex=5))
#   
#   nestsuccess_temp<-SaliParams %>% filter(SaliParams$Parameter=="Ko'ko' Nest Success", 
#                                           SaliParams$Scenario==alternative, SaliParams$Site==site)
#   nestsuccess<-replicate(nsims, rbetamix(alpha=nestsuccess_temp$alphas, 
#                                          beta=nestsuccess_temp$betas, num_ex=5))
#   nestattempts_temp<-SaliParams %>% filter(SaliParams$Parameter=="Ko'ko' Nest Attempts", 
#                                            SaliParams$Scenario==alternative, SaliParams$Site==site)
#   nestattempts<-replicate(nsims, rgammamix(alpha=nestattempts_temp$alphas, 
#                                            beta=nestattempts_temp$betas, num_ex=5))
#   hatchlingspnest_temp<-SaliParams %>% filter(SaliParams$Parameter=="Ko'ko' Hatchlings per Nest", 
#                                               SaliParams$Scenario==alternative, SaliParams$Site==site)
#   hatchlingspnest<-replicate(nsims, rgammamix(alpha=hatchlingspnest_temp$alphas, 
#                                               beta=hatchlingspnest_temp$betas, num_ex=5))
#   fec<-(nestsuccess*nestattempts*hatchlingspnest)/2
#   
#   #run sims
#   sims<-NULL
#   for(s in 1:nsims){
#     #parameter stuff up here?
#     maxind<-nfemales*nmales*200
#     Find<-array(dim=c(4, nyears+1, maxind))
#     #5 in array: 
#     #1: no. F adults, 2: no. female chicks produced, 3: F Juvs 4: dead
#     Mind<-array(dim=c(4, nyears+1, maxind))
#     #5 in array: 
#     #1: no. M adults, 2: no. M chicks produced, 3:  M Juvs 4: dead
#     
#     Find[1,1,1:nfemales]<-1
#     Mind[1,1,1:nmales]<-1
#     tempind<-array(dim=c(2,nyears, maxind)) #2: 1=females, 2=males
#     inpop<-array(dim=c(2,nyears+1)) #2: 1=females, 2=males
#     inpopsub<-array(dim=c(2,nyears+1))
#     inpop[1,1]<-nfemales
#     inpop[2,1]<-nmales
#     for(t in 1:nyears){
#       for(i in 1:inpop[1,t]){
#         tempind[1,t,i]<-which(Find[c(1,4),t,i]==1)
#         if(tempind[1,t,i]==1){ #if adult female
#           Find[2,t,i]<-rpois(1,fec)
#           survF<-rbinom(1,1,phiad)
#           Find[1,t+1,i]<-ifelse(survF==1, 1, 0)
#           Find[3,t+1,i]<-NA
#           Find[4,t+1,i]<-ifelse(survF==0, 1, NA)
#         } else if(tempind[1,t,i]==2){ #they are dead
#           Find[1:3,t+1,i]<-NA
#           Find[4,t+1,i]<-1
#         }
#       }
#       for(i in 1:inpop[2,t]){
#         tempind[2,t,i]<-which(Mind[c(1,4),t,i]==1)
#         if(tempind[2,t,i]==1){ #if adult male
#           Mind[2,t,i]<-rpois(1,fec)
#           survM<-rbinom(1,1,phiad)
#           Mind[1,t+1,i]<-ifelse(survM==1, 1, 0)
#           Mind[3,t+1,i]<-NA
#           Mind[4,t+1,i]<-ifelse(survM==0, 1, NA)
#         } else if(tempind[2,t,i]==2){ #they are dead
#           Mind[1:3,t+1,i]<-NA
#           Mind[4,t+1,i]<-1
#         }
#       }#i loop for adult fate
#       #add in the offspring from current year, see if they survive to 
#       # be recruited to adults
#       inpopsub[1,t]<-sum(Find[2,t,1:inpop[1,t]], na.rm=T)
#       inpopsub[2,t]<-sum(Mind[2,t,1:inpop[2,t]],na.rm=T)
#       if(inpopsub[1,t]>0){
#         Find[3,t,(inpop[1,t]+1):(inpopsub[1,t]+inpop[1,t])]<-1
#         for(i in (inpop[1,t]+1):(inpopsub[1,t]+inpop[1,t])){
#           survFJ<-rbinom(1,1,phij)
#           Find[3,t,i]<-ifelse(survFJ==1, 1, 0)
#           survFA<-rbinom(1,1,phiad)
#           Find[1,t+1,i]<-ifelse(survFA==1 & Find[3,t,i]==1, 1, 0)
#           Find[4,t,i]<-ifelse(survFA==0 | Find[3,t,i]==0, 1,NA)
#           Find[4,t+1,i]<-ifelse(survFA==0 | Find[3,t,i]==0, 1,NA)
#         }
#       }
#       if(inpopsub[2,t]>0){
#         Mind[3,t,(inpop[2,t]+1):(inpopsub[2,t]+inpop[2,t])]<-1
#         for(i in (inpop[2,t]+1):(inpopsub[2,t]+inpop[2,t])){
#           survMJ<-rbinom(1,1,phij)
#           Mind[3,t,i]<-ifelse(survMJ==1, 1, 0)
#           survMA<-rbinom(1,1,phiad)
#           Mind[1,t+1,i]<-ifelse(survMA==1 & Mind[3,t,i]==1, 1, 0)
#           Mind[4,t,i]<-ifelse(survMA==0 | Mind[3,t,i]==0, 1,NA)
#           Mind[4,t+1,i]<-ifelse(survMA==0 | Mind[3,t,i]==0, 1,NA)
#         }
#       }
#       inpop[1,t+1]<-sum(inpop[1,t],inpopsub[1,t])
#       inpop[2,t+1]<-sum(inpop[2,t],inpopsub[2,t])
#     }
#     sims[[s]]<-list(Find=Find, Mind=Mind)
#   }
# }




