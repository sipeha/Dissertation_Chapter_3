library(gsheet);library(mc2d);library(reshape2);library(dplyr);library(stringi);library(ggplot2);library(tidyverse)
library(here); library(cowplot)
setwd("~/Desktop/Desktop - Hannah’s MacBook Pro/Guam Project/Guam_II/Guam elicitation take two")
n.experts<-7 #?
output<-as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1CAbvvDbaGnGEuL5qNCuGff19WM0Xe-7uhfHe_u_Co7o/edit?resourcekey#gid=1879243998'))[,-1]
colnames(output)<-c("Expert", "best","min","max","conf","round")


#do fitting
beta.fit <- function(min.data,ml.data,max.data,conf){
  
  #function to return sum of squares for fitting pert parameters 
  beta.ss <- function(par){ 
    
    #parameters to estimate
    alpha <- par[1]
    
    #get desired quantiles from confidence 
    pr1 <- (1-conf)/2 
    pr2 <- 1-(1-conf)/2
    
    #beta quantile function
    min.model <- qbeta(p = pr1, shape1 = alpha, shape2 = (alpha*(1-ml.data))/ml.data, lower.tail = TRUE, log = FALSE)
    max.model <- qbeta(p = pr2, shape1 = alpha, shape2 = (alpha*(1-ml.data))/ml.data, lower.tail = TRUE, log = FALSE)
    
    #sum of squares
    ss <- (min.data-min.model)^2 + (max.data-max.model)^2
    return(ss)
    
  }
  #run optim - use min and max as starting values 
  fit <- optim(c(1),beta.ss, method = "Brent",lower = 0, upper = 5000)
  
  alpha <- fit$par
  beta <- (alpha*(1-ml.data))/(ml.data)
  
  params <- list("alpha" = alpha,"beta" = beta)
  return(params)
}

#Round 1
Round1<-output[which(output$round=="Round 1"),]
Round1params<-matrix(nrow=n.experts, ncol=2)
colnames(Round1params)<-c("alpha","beta")
#make output
for(i in 1:n.experts){
  Round1params[i,]<-as.numeric(unlist(beta.fit(min.data=Round1$min[i],ml.data=Round1$best[i],
                                  max.data=Round1$max[i],conf=(Round1$conf[i]/100))))
}
Round1<-cbind(Round1, Round1params)

Round1output<-NULL
Round1outputS<-NULL
for(i in 1:n.experts){
  Round1output[[i]]<-rbeta(1000000,shape1 = Round1$alpha[i],shape2 = Round1$beta[i])
  tempAB<-beta.fit(min.data=as.numeric(quantile(Round1output[[i]], c(0.05))), 
                   ml.data=Round1$best[i], max.data=as.numeric(quantile(Round1output[[i]], c(0.95))),
                   conf=(Round1$conf[i]/100))
  Round1outputS[[i]]<-rbeta(1000000,shape1 = tempAB$alpha,shape2 = tempAB$beta)
}

all_90 <- beta.fit(quantile(unlist(Round1outputS), c(0.05)),
                    mean(Round1$best),
                    quantile(unlist(Round1outputS), c(0.95)),
                    0.9)
#generate randoms from combined means
devs_all_90<-rbeta(1000000,shape1 = all_90$alpha,shape2 = all_90$beta)

#TODO
####mixture
sq<-seq(0,1,by=(1/n.experts))
rmixbeta<-function(){
  p = runif(1)
  
  if(p<=sq[2]){
    x = rbeta(1,Round1params[1,1],Round1params[1,2])
  }
  if(p>sq[2] && p<= sq[3]){
    x = rbeta(1,Round1params[2,1],Round1params[2,2])
  }
  if(p>sq[3] && p<=sq[4]){
    x=rbeta(1,Round1params[3,1],Round1params[3,2])
  }
  if(p>sq[4] && p<=sq[5]){
    x=rbeta(1,Round1params[4,1],Round1params[4,2])
  }
  if(p>sq[5] && p<=sq[6]){
    x=rbeta(1,Round1params[5,1],Round1params[5,2])
  }
  if(p>sq[6] && p<= sq[7]){
    x=rbeta(1,Round1params[6,1],Round1params[6,2])
  }
  
  if(p>sq[7] ){
    x=rbeta(1,Round1params[7,1],Round1params[7,2])
  }
  
  return(x)
}

#generate randoms from beta mixture
mixsamps<-replicate(800000, rmixbeta())

#TODO
dfR1<-data.frame(ID=c(
                     c(rep(Round1$Expert[1], length(Round1outputS[[1]]))), 
                     c(rep(Round1$Expert[2], length(Round1outputS[[2]]))),
                     c(rep(Round1$Expert[3], length(Round1outputS[[3]]))),
                     c(rep(Round1$Expert[4], length(Round1outputS[[4]]))),
                     c(rep(Round1$Expert[5], length(Round1outputS[[5]]))),
                     c(rep(Round1$Expert[6], length(Round1outputS[[6]]))),
                     c(rep(Round1$Expert[7], length(Round1outputS[[7]]))),
                     c(rep("1 -Combined 90% means", length(devs_all_90))),
                     c(rep("2 -Combined 90% mixture", length(c(mixsamps))))),
                values=c(Round1outputS[[1]], Round1outputS[[2]], Round1outputS[[3]],
                         Round1outputS[[4]], Round1outputS[[5]],Round1outputS[[6]],
                         Round1outputS[[7]],
                         devs_all_90, 
                         mixsamps))


R1<-ggplot(dfR1, aes(x=ID, y=values, fill=ID))+geom_violin()+
  xlab("")+ylab("Probability")+labs(title="Chichirika Adult Survival",
                                    subtitle="Elicited values standardized to 90%",fill="",
                                    caption="Round 1")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="right")+
  expand_limits(y=c(0,1))
R1
ggsave(R1, filename = "Practice Question Round 1.png",
       width = 10, height = 6, dpi = 300, units = "in", device='png')

#TODO
Round1NonSDF<-data.frame(ID=c(
  c(rep(paste0(Round1$Expert[1]," ",Round1$conf[1],"%"), length(Round1output[[1]]))), 
  c(rep(paste0(Round1$Expert[2]," ",Round1$conf[2],"%"), length(Round1output[[2]]))),
  c(rep(paste0(Round1$Expert[3]," ",Round1$conf[3],"%"), length(Round1output[[3]]))),
  c(rep(paste0(Round1$Expert[4]," ",Round1$conf[4],"%"), length(Round1output[[4]]))),
  c(rep(paste0(Round1$Expert[5]," ",Round1$conf[5],"%"), length(Round1output[[5]]))),
  c(rep(paste0(Round1$Expert[6]," ",Round1$conf[6],"%"), length(Round1output[[6]]))),
  c(rep(paste0(Round1$Expert[7]," ",Round1$conf[7],"%"), length(Round1output[[7]]))),
  c(rep(paste0(Round1$Expert[1]," 90%"), length(Round1outputS[[1]]))),
  c(rep(paste0(Round1$Expert[2]," 90%"), length(Round1outputS[[2]]))),
  c(rep(paste0(Round1$Expert[3]," 90%"), length(Round1outputS[[3]]))),
  c(rep(paste0(Round1$Expert[4]," 90%"), length(Round1outputS[[4]]))),
  c(rep(paste0(Round1$Expert[5]," 90%"), length(Round1outputS[[5]]))),
  c(rep(paste0(Round1$Expert[6]," 90%"), length(Round1outputS[[6]]))),
  (rep(paste0(Round1$Expert[7]," 90%"), length(Round1outputS[[7]])))
  
  ),
  values=c(Round1output[[1]], Round1output[[2]], Round1output[[3]],
           Round1output[[4]], Round1output[[5]],Round1output[[6]],
           Round1output[[7]],
           Round1outputS[[1]], Round1outputS[[2]], Round1outputS[[3]],
           Round1outputS[[4]], Round1outputS[[5]], Round1outputS[[6]],
           Round1outputS[[7]]))
R1NS<-ggplot(Round1NonSDF, aes(x=ID, y=values, fill=ID))+geom_violin()+
  xlab("")+ylab("Probability")+labs(title="Chichirika Adult Survival",
                                    subtitle="Elicited values",fill="",
                                    caption="Round 1")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="right")+
  expand_limits(y=c(0,1))
R1NS
ggsave(R1NS, filename = "Practice Question Round 1NS.png",
       width = 10, height = 6, dpi = 300, units = "in", device='png')

#########
#Round 2
Round2<-output[which(output$round=="Round 2"),]
Round2params<-matrix(nrow=n.experts, ncol=2)
colnames(Round2params)<-c("alpha","beta")
#make output
for(i in 1:n.experts){
  Round2params[i,]<-as.numeric(unlist(beta.fit(min.data=Round2$min[i],ml.data=Round2$best[i],
                                               max.data=Round2$max[i],conf=(Round2$conf[i]/100))))
}
Round2<-cbind(Round2, Round2params)

Round2output<-NULL
Round2outputS<-NULL
for(i in 1:n.experts){
  Round2output[[i]]<-rbeta(1000000,shape1 = Round2$alpha[i],shape2 = Round2$beta[i])
  tempAB<-beta.fit(min.data=as.numeric(quantile(Round2output[[i]], c(0.05))), 
                   ml.data=Round2$best[i], max.data=as.numeric(quantile(Round2output[[i]], c(0.95))),
                   conf=(Round2$conf[i]/100))
  Round2outputS[[i]]<-rbeta(1000000,shape1 = tempAB$alpha,shape2 = tempAB$beta)
}

all_90R2 <- beta.fit(quantile(unlist(Round2outputS), c(0.05)),
                   mean(Round2$best),
                   quantile(unlist(Round2outputS), c(0.95)),
                   0.9)
#generate randoms from combined means
devs_all_90R2<-rbeta(1000000,shape1 = all_90R2$alpha,shape2 = all_90R2$beta)

#TODO
####mixture
sq<-seq(0,1,by=(1/n.experts))
rmixbetaR2<-function(){
  p = runif(1)
  
  if(p<=sq[2]){
    x = rbeta(1,Round2params[1,1],Round2params[1,2])
  }
  if(p>sq[2] && p<= sq[3]){
    x = rbeta(1,Round2params[2,1],Round2params[2,2])
  }
  if(p>sq[3] && p<=sq[4]){
    x=rbeta(1,Round2params[3,1],Round2params[3,2])
  }
  if(p>sq[4] && p<=sq[5]){
    x=rbeta(1,Round2params[4,1],Round2params[4,2])
  }
  if(p>sq[5] && p<=sq[6]){
    x=rbeta(1,Round2params[5,1],Round2params[5,2])
  }
  if(p>sq[6] && p<= sq[7]){
    x=rbeta(1,Round2params[6,1],Round2params[6,2])
  }
  
  if(p>sq[7] ){
    x=rbeta(1,Round2params[7,1],Round2params[7,2])
  }
  
  return(x)
}

#generate randoms from beta mixture
mixsampsR2<-replicate(800000, rmixbetaR2())


dfR2<-data.frame(ID=c(
  c(rep(Round2$Expert[1], length(Round2outputS[[1]]))), 
  c(rep(Round2$Expert[2], length(Round2outputS[[2]]))),
  c(rep(Round2$Expert[3], length(Round2outputS[[3]]))),
  c(rep(Round2$Expert[4], length(Round2outputS[[4]]))),
  c(rep(Round2$Expert[5], length(Round2outputS[[5]]))),
  c(rep(Round2$Expert[6], length(Round2outputS[[6]]))),
  c(rep(Round2$Expert[7], length(Round2outputS[[7]]))),
  c(rep("1- Combined 90% means", length(devs_all_90R2))),
  c(rep("2-Combined 90% mixture", length(c(mixsampsR2))))),
  values=c(Round2outputS[[1]], Round2outputS[[2]], Round2outputS[[3]],
           Round2outputS[[4]], Round2outputS[[5]],
           Round2outputS[[6]], Round2outputS[[7]],
           devs_all_90R2, 
           mixsampsR2))


R2<-ggplot(dfR2, aes(x=ID, y=values, fill=ID))+geom_violin()+
  xlab("")+ylab("Probability")+labs(title="Chichirika Adult Survival",
                                    subtitle="Elicited values standardized to 90%",fill="",
                                    caption="Round 2")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="right")+
  expand_limits(y=c(0,1))
R2
ggsave(R2, filename = "Practice question Round 2.png",
       width = 8, height = 6, dpi = 300, units = "in", device='png')


#########
Round2NonSDF<-data.frame(ID=c(
  c(rep(paste0(Round2$Expert[1]," ",Round2$conf[1],"%"), length(Round2output[[1]]))), 
  c(rep(paste0(Round2$Expert[2]," ",Round2$conf[2],"%"), length(Round2output[[2]]))),
  c(rep(paste0(Round2$Expert[3]," ",Round2$conf[3],"%"), length(Round2output[[3]]))),
  c(rep(paste0(Round2$Expert[4]," ",Round2$conf[4],"%"), length(Round2output[[4]]))),
  c(rep(paste0(Round2$Expert[5]," ",Round2$conf[5],"%"), length(Round2output[[5]]))),
  c(rep(paste0(Round2$Expert[1]," 90%"), length(Round2outputS[[1]]))),
  c(rep(paste0(Round2$Expert[2]," 90%"), length(Round2outputS[[2]]))),
  c(rep(paste0(Round2$Expert[3]," 90%"), length(Round2outputS[[3]]))),
  c(rep(paste0(Round2$Expert[4]," 90%"), length(Round2outputS[[4]]))),
  c(rep(paste0(Round2$Expert[5]," 90%"), length(Round2outputS[[5]]))),
),
values=c(Round2output[[1]], Round2output[[2]], Round2output[[3]],
         Round2output[[4]], Round2output[[5]],
         Round2outputS[[1]], Round2outputS[[2]], Round2outputS[[3]],
         Round2outputS[[4]], Round2outputS[[5]]
         ))
R2NS<-ggplot(Round2NonSDF, aes(x=ID, y=values, fill=ID))+geom_violin()+
  xlab("")+ylab("Probability")+labs(title="Chichirika Adult Survival",
                                    subtitle="Elicited values",fill="",
                                    caption="Round 2")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="right")+
  expand_limits(y=c(0,1))
R2NS
ggsave(R1NS, filename = "Practice Question Round 2NS.png",
       width = 8, height = 6, dpi = 300, units = "in", device='png')
###########

dfR1$Round<-"Round 1"
dfR2$Round<-"Round 2"

bothDF<-rbind(dfR1, dfR2)

BothPlot<-ggplot(bothDF, aes(x=ID, y=values, fill=ID))+geom_violin()+
  facet_wrap(~factor(Round,levels=c("Round 1", "Round 2")), scale="free")+
  xlab("")+ylab("Probability")+labs(title="Chichirika Adult Survival",
                                    subtitle="Elicited values standardized to 90%",fill="")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="right")+
  expand_limits(y=c(0,1))
BothPlot
ggsave(BothPlot, filename = "Practice question Both Rounds.png",
       width = 10, height = 6, dpi = 300, units = "in", device='png')

