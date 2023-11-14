beta.fit <- function(vect){
  vect<-c(sort(vect[1:3]), vect[4])
  min.data<-vect[1]
  ml.data<-vect[2]
  max.data<-vect[3]
  conf<-vect[4]
  calculate.mode <- function(alpha, beta) {
    return((alpha-1) / (alpha+beta-2))
  }
  if(min.data==ml.data && ml.data==max.data){
    min.data<-min.data-0.000003
    ml.data<-ml.data-0.000002
  }
  if(min.data==ml.data){
    ml.data<-ml.data+0.0001
    min.data<-min.data-0.0001
  }
  # if(ml.data==max.data){
  #   ml.data<-ml.data-0.0000001
  # }
  if(max.data==ml.data){
    ml.data<-ml.data-0.0001
  }
  #first beta.ss gets good initial values for fitting
  beta.ss <- function(par){ 
    #parameters to estimate
    alpha <- par[1]
    #get desired quantiles from confidence 
    pr1 <- (1-conf)/2 
    pr2 <- 1-(1-conf)/2
    #beta quantile function
    min.model <- qbeta(p = pr1, shape1 = alpha, shape2 = (alpha*(1-ml.data))/ml.data, lower.tail = TRUE, log = FALSE)
    max.model <- qbeta(p = pr2, shape1 = alpha, shape2 = (alpha*(1-ml.data))/ml.data, lower.tail = TRUE, log = FALSE)
    #mode.model<-alpha/(alpha+(alpha*(1-ml.data))/ml.data)
    mode.model <- (alpha-1)/(alpha+(alpha*(1-ml.data))/(ml.data)-2)
    #sum of squares
    ss <- (min.data-min.model)^2 + (max.data-max.model)^2 + (ml.data-mode.model)^2
    return(ss)
    
  }
  #run optim - use min and max as starting values 
  fit <- optim(c(1),beta.ss, method = "Brent",lower = 0, upper = 5000)
  
  alpha1 <- fit$par
  beta1 <- (alpha1*(1-ml.data))/(ml.data)
  
  #beta.ss2 fits the mode better, using beta.fit estimated alpha and beta
  beta.ss2 <- function(par2){ 
    #parameters to estimate
    alpha <- par2[1]
    beta<-par2[2]
    #get desired quantiles from confidence 
    pr1 <- (1-conf)/2 
    pr2 <- 1-(1-conf)/2
    #beta quantile function
    min.model <- qbeta(p = pr1, shape1 = alpha, shape2 = beta, lower.tail = TRUE, log = FALSE)
    max.model <- qbeta(p = pr2, shape1 = alpha, shape2 = beta, lower.tail = TRUE, log = FALSE)
    
    mode.model <- calculate.mode(alpha, beta)
    #mode.model <- calculate.mean(alpha, beta)
    
    #sum of squares
    ss <- (min.data-min.model)^2 + (max.data-max.model)^2 + (ml.data-mode.model)^2
    return(ss)
    
  }
  #run optim - use min and max as starting values 
  fit2 <- optim(par=c(alpha1,beta1),beta.ss2, method="CG")
  # method = "L-BFGS-B",
  # lower = c(0,0), upper = c(50,50))
  
  alpha.out <- fit2$par[1]
  beta.out <- fit2$par[2]
  mode.out <- (alpha.out-1) / (alpha.out+beta.out-2)
  #mode.out <- (alpha.out) / (alpha.out+beta.out)
  
  rands<-rbeta(100000, alpha.out, beta.out)
  params <- list("alpha" = alpha.out,"beta" = beta.out, "rands"=rands, 
                 ml.data=ml.data)
  return(params)
}

gamma.fit <- function(vect){
  vect<-c(sort(vect[1:3]), vect[4])
  min.data<-vect[1]
  ml.data<-vect[2]
  max.data<-vect[3]
  conf<-vect[4]
  calculate.mode <- function(alpha, beta) {
    return((alpha-1) / (alpha+beta-2))
  }
  if(min.data==ml.data && ml.data==max.data){
    min.data<-min.data-0.000003
    ml.data<-ml.data-0.000002
  }
  if(min.data==ml.data){
    ml.data<-ml.data+0.0001
    min.data<-min.data-0.0001
  }
  # if(ml.data==max.data){
  #   ml.data<-ml.data-0.0000001
  # }
  if(max.data==ml.data){
    ml.data<-ml.data-0.0001
  }
  #function to return sum of squares for fitting gamma parameters 
  gamma.ss <- function(par){ 
    
    #parameters to estimate
    alpha <- par[1]
    
    #get desired quantiles from confidence 
    pr1 <- (1-conf)/2 
    pr2 <- 1-(1-conf)/2
    
    #gamma quantile function
    min.model <-qgamma(p=pr1, shape = alpha, rate = (alpha/ml.data), lower.tail = T, log=F)
    max.model <-qgamma(p=pr2, shape = alpha, rate = (alpha/ml.data), lower.tail = T, log=F)
    mode.model <- (alpha-1)/(alpha/ml.data)
    #sum of squares
    ss <- (min.data-min.model)^2 + (max.data-max.model)^2 + (ml.data - mode.model)^2
    
    return(ss)
    
  }
  #run optim - use min and max as starting values 
  fit <- optim(c(2),gamma.ss, method = "Brent",lower = 0, upper = 5000)
  
  alpha1 <- fit$par
  beta1 <- alpha1/ml.data #change this to Gamma param
  gamma.ss <- function(par){ 
    
    #parameters to estimate
    alpha <- par[1]
    beta <- par[2]
    
    #get desired quantiles from confidence 
    pr1 <- (1-conf)/2 
    pr2 <- 1-(1-conf)/2
    
    #gamma quantile function
    min.model <-qgamma(p=pr1, shape = alpha, rate = beta, lower.tail = T, log=F)
    max.model <-qgamma(p=pr2, shape = alpha, rate = beta, lower.tail = T, log=F)
    mode.model <- calculate.mode(alpha, beta)
    #sum of squares
    ss <- (min.data-min.model)^2 + (max.data-max.model)^2 + (ml.data - mode.model)^2
    
    return(ss)
    
  }
  fit2 <- optim(c(alpha1, beta1),gamma.ss, method="CG")
  alpha.out<-fit2$par[1]
  beta.out<-fit2$par[2]
  mode.out<-(alpha.out-1)/beta.out
  #params <- list("alpha" = alpha.out,"beta" = beta.out, "mode"=mode.out)
  rands<-rgamma(100000, alpha.out, rate=beta.out)
  params <- list("alpha" = alpha.out,"beta" = beta.out, "rands"=rands, 
                 ml.data=ml.data)
  return(params)
}


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


datacleanfn<-function(datalist, num_ex, nqs, prob){
  out<-datalist
  if(prob==T){
    for(e in 1:num_ex){
      for(i in 1:nqs){
        for(j in 1:4){
          if(datalist[[e]][i,j]>1){
            datalist[[e]][i,j]<-datalist[[e]][i,j]/100
          }else{
            datalist[[e]][i,j]<-datalist[[e]][i,j]
          }
          out[[e]][i,1:4]<-c(datalist[[e]][i,2], datalist[[e]][i,1], datalist[[e]][i,3],
                             datalist[[e]][i,4])
        }
      }
    }
  }else{
    for(e in 1:num_ex){
      for(i in 1:nqs){
        if(datalist[[e]][i,4]>1){
          datalist[[e]][i,4]<-datalist[[e]][i,4]/100
        }else{
          datalist[[e]][i,4]<-datalist[[e]][i,4]
        }
        out[[e]][i,1:4]<-c(datalist[[e]][i,2], datalist[[e]][i,1], datalist[[e]][i,3],
                           datalist[[e]][i,4])
      }
    }
  }
  out
}




getparams<-function(prob, paramvect){
  if(prob==T){
    temp1<-beta.fit(paramvect)
    tempq<-quantile(temp1$rands, c(0.005,0.995))
    standardvect<-c(as.numeric(tempq[1]), temp1$ml.data, 
                as.numeric(tempq[2]),0.99)
    out<-beta.fit(standardvect)
  } else{
    temp1<-gamma.fit(paramvect)
    tempq<-quantile(temp1$rands, c(0.005,0.995))
    standardvect<-c(as.numeric(tempq[1]), temp1$ml.data, 
                    as.numeric(tempq[2]),0.99)
    out<-gamma.fit(standardvect)
  }
  return(out)
}

makerands12<-function(nalts, num_ex, datalist){
  standardmat<-datalist
  combined<-matrix(nrow=4, ncol=nalts)
  E1out<-E2out<-E3out<-E4out<-E5out<-Cout<-alphas<-betas<-NULL
  for(i in 1:nalts){
    for(e in 1:num_ex){
      temp<-beta.fit(as.numeric(datalist[[e]][i,5:8]))
      tempq<-quantile(temp$rands, c(0.005,0.995))
      standardmat[[e]][i,5:8]<-c(as.numeric(tempq[1]), temp$ml.data, 
                                 as.numeric(tempq[2]),datalist[[e]]$Conf[i])
      # temp<-beta.fit(as.numeric(datalist[[e]][i,6:9]))
      # standardmat[[e]][i,6:9]<-c(as.numeric(quantile(temp$rands, c(0.05))), temp$ml.data,#datalist[[e]]$Best[i], 
      #                            as.numeric(quantile(temp$rands, c(0.95))), datalist[[e]]$Conf[i])
    }
    E1out[[i]]<-beta.fit(vect=as.numeric(standardmat[[1]][i, 6:9]))
    E2out[[i]]<-beta.fit(vect=as.numeric(standardmat[[2]][i, 6:9]))
    E3out[[i]]<-beta.fit(vect=as.numeric(standardmat[[3]][i, 6:9]))
    E4out[[i]]<-beta.fit(vect=as.numeric(standardmat[[4]][i, 6:9]))
    E5out[[i]]<-beta.fit(vect=as.numeric(standardmat[[5]][i, 6:9]))
    alphas[[i]]<-c(E1out[[i]]$alpha, E2out[[i]]$alpha, E3out[[i]]$alpha, 
                   E4out[[i]]$alpha, E5out[[i]]$alpha)
    betas[[i]]<-c(E1out[[i]]$beta, E2out[[i]]$beta, E3out[[i]]$beta,
                  E4out[[i]]$beta, E5out[[i]]$beta)
    #Cout[[i]]<-replicate(300000, rmixbeta(alphas[[i]], betas[[i]]))
    Cout[[i]]<-replicate(300000, rmixbetatake2(alphas[[i]], betas[[i]], num_ex))
    combined[,i]<-c(as.numeric(quantile(Cout[[i]], c(0.05))),
                    mean(Cout[[i]]),
                    as.numeric(quantile(Cout[[i]], c(0.95))),
                    0.9)
  }
  
  Par<-NULL
  
  for(i in 1:nalts){
    Par[[i]]<-data.frame(
      ID = c(rep(paste0("2 - ",datalist$E1$Expert[1]), 100000),  rep(paste0("3 - ",datalist$E2$Expert[1]), 100000),
             rep(paste0("4 - ",datalist$E3$Expert[1]), 100000), rep(paste0("1 - ","Combined"), 300000),
             rep(paste0("5 - ",datalist$E4$Expert[1]), 100000),  rep(paste0("6 - ",datalist$E5$Expert[1]), 100000)),
      value= c(E1out[[i]]$rands, E2out[[i]]$rands, E3out[[i]]$rands, Cout[[i]], E4out[[i]]$rands, E5out[[i]]$rands),
      scenario=c(rep(datalist$E1$Scenario[i], 800000))
    )
  }
  ParSite<-NULL
  sq<-seq(1,7,by=1)
  for(j in 1:4){ #only 2 sites
    ParSite[[j]]<-rbind(Par[[sq[1]+(7*(j-1))]], Par[[sq[2]+(7*(j-1))]],Par[[sq[3]+(7*(j-1))]],
                        Par[[sq[4]+(7*(j-1))]],Par[[sq[5]+(7*(j-1))]],Par[[sq[6]+(7*(j-1))]],
                        Par[[sq[7]+(7*(j-1))]])
  }
  ParSite
}

makerands12_gamma<-function(nalts, num_ex, datalist, roundnum){
  standardmat<-datalist
  combined<-matrix(nrow=4, ncol=nalts)
  E1out<-E2out<-E3out<-E4out<-E5out<-Cout<-alphas<-betas<-NULL
  for(i in 1:nalts){
    for(e in 1:num_ex){
      temp<-gamma.fit(as.numeric(datalist[[e]][i,6:9]))
      standardmat[[e]][i,6:9]<-c(as.numeric(quantile(temp$rands, c(0.05))), temp$ml.data,#datalist[[e]]$Best[i], 
                                 as.numeric(quantile(temp$rands, c(0.95))), datalist[[e]]$Conf[i])
    }
    E1out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[1]][i, 6:9]))
    E2out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[2]][i, 6:9]))
    E3out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[3]][i, 6:9]))
    E4out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[4]][i, 6:9]))
    E5out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[5]][i, 6:9]))
    alphas[[i]]<-c(E1out[[i]]$alpha, E2out[[i]]$alpha, E3out[[i]]$alpha,E4out[[i]]$alpha, E5out[[i]]$alpha)
    betas[[i]]<-c(E1out[[i]]$beta, E2out[[i]]$beta, E3out[[i]]$beta,E4out[[i]]$beta, E5out[[i]]$beta)
    Cout[[i]]<-replicate(300000, rmixgammas(alphas[[i]], betas[[i]], num_ex))
    combined[,i]<-c(as.numeric(quantile(Cout[[i]], c(0.05))),
                    mean(Cout[[i]]),
                    as.numeric(quantile(Cout[[i]], c(0.95))),
                    0.9)
  }
  
  Par<-NULL
  
  for(i in 1:nalts){
    Par[[i]]<-data.frame(
      ID = c(rep(paste0("2 - ",datalist$E1$Expert[1]), 100000),  rep(paste0("3 - ",datalist$E2$Expert[1]), 100000),
             rep(paste0("4 - ",datalist$E3$Expert[1]), 100000), rep(paste0("1 - ","Combined"), 300000),
             rep(paste0("5 - ",datalist$E4$Expert[1]), 100000),  rep(paste0("6 - ",datalist$E5$Expert[1]), 100000)),
      value= c(E1out[[i]]$rands, E2out[[i]]$rands, E3out[[i]]$rands, Cout[[i]],
               E4out[[i]]$rands, E5out[[i]]$rands),
      scenario=c(rep(datalist$E1$Scenario[i], 800000))
      # round=c(rep(paste0("Round ", roundnum), 900000)) #900000
    )
  }
  ParSite<-NULL
  sq<-seq(1,7,by=1)
  for(j in 1:2){ #only 2 sites
    ParSite[[j]]<-rbind(Par[[sq[1]+(7*(j-1))]], Par[[sq[2]+(7*(j-1))]],Par[[sq[3]+(7*(j-1))]],
                        Par[[sq[4]+(7*(j-1))]],Par[[sq[5]+(7*(j-1))]],Par[[sq[6]+(7*(j-1))]],
                        Par[[sq[7]+(7*(j-1))]])
  }
  ParSite
}


makerandsCom<-function(datalist, nalts, num_ex){
  standardmat<-datalist
  combined<-matrix(nrow=4, ncol=nalts)
  E1out<-E2out<-E3out<-Cout<-E4out<-E5out<-alphas<-betas<-NULL
  for(i in 1:nalts){
    for(e in 1:num_ex){
      temp<-beta.fit(as.numeric(datalist[[e]][i,6:9]))
      standardmat[[e]][i,6:9]<-c(as.numeric(quantile(temp$rands, c(0.05))), temp$ml.data, 
                                 as.numeric(quantile(temp$rands, c(0.95))), datalist[[e]]$Conf[i])
    }
    E1out[[i]]<-beta.fit(vect=as.numeric(standardmat[[1]][i, 6:9]))
    E2out[[i]]<-beta.fit(vect=as.numeric(standardmat[[2]][i, 6:9]))
    E3out[[i]]<-beta.fit(vect=as.numeric(standardmat[[3]][i, 6:9]))
    E4out[[i]]<-beta.fit(vect=as.numeric(standardmat[[4]][i, 6:9]))
    E5out[[i]]<-beta.fit(vect=as.numeric(standardmat[[5]][i, 6:9]))
    alphas[[i]]<-c(E1out[[i]]$alpha, E2out[[i]]$alpha, E3out[[i]]$alpha,
                   E4out[[i]]$alpha, E5out[[i]]$alpha)
    betas[[i]]<-c(E1out[[i]]$beta, E2out[[i]]$beta, E3out[[i]]$beta, 
                  E4out[[i]]$beta, E5out[[i]]$beta)
    Cout[[i]]<-replicate(300000, rmixbetatake2(alphas[[i]], betas[[i]], num_ex))
    combined[,i]<-c(as.numeric(quantile(Cout[[i]], c(0.05))),
                    mean(Cout[[i]]),
                    as.numeric(quantile(Cout[[i]], c(0.95))),
                    0.9)
  }
  Par<-NULL
  abc<-c("a","b","c","d","e","f","g")
  for(i in 1:nalts){
    Par[[i]]<-data.frame(
      ID=c(rep(datalist$E1$Scenario[i], length(Cout[[i]]))),
      # ID=c(rep(paste0(abc[i]," - ", datalist$E1$Scenario[i]), length(Cout[[i]]))),
      value=c(Cout[[i]]))
  }
  # out<-list(Par=Par)
  # out
  Parx<-do.call(rbind.data.frame, Par)
  Parx
  
}

makerandsCom_gamma<-function(datalist, nalts, num_ex){
  standardmat<-datalist
  combined<-matrix(nrow=4, ncol=nalts)
  E1out<-E2out<-E3out<-Cout<-E4out<-E5out<-alphas<-betas<-NULL
  for(i in 1:nalts){
    for(e in 1:num_ex){
      temp<-gamma.fit(as.numeric(datalist[[e]][i,6:9]))
      standardmat[[e]][i,6:9]<-c(as.numeric(quantile(temp$rands, c(0.05))), temp$ml.data, 
                                 as.numeric(quantile(temp$rands, c(0.95))), datalist[[e]]$Conf[i])
    }
    E1out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[1]][i, 6:9]))
    E2out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[2]][i, 6:9]))
    E3out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[3]][i, 6:9]))
    E4out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[4]][i, 6:9]))
    E5out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[5]][i, 6:9]))
    alphas[[i]]<-c(E1out[[i]]$alpha, E2out[[i]]$alpha, E3out[[i]]$alpha, 
                   E4out[[i]]$alpha, E5out[[i]]$alpha)
    betas[[i]]<-c(E1out[[i]]$beta, E2out[[i]]$beta, E3out[[i]]$beta, 
                  E4out[[i]]$beta, E5out[[i]]$beta)
    Cout[[i]]<-replicate(300000, rmixgammas(alphas[[i]], betas[[i]], num_ex))
    combined[,i]<-c(as.numeric(quantile(Cout[[i]], c(0.05))),
                    mean(Cout[[i]]),
                    as.numeric(quantile(Cout[[i]], c(0.95))),
                    0.9)
  }
  Par<-NULL
  abc<-c("a","b","c","d","e","f","g")
  for(i in 1:nalts){
    Par[[i]]<-data.frame(
      ID=c(rep(datalist$E1$Scenario[i], length(Cout[[i]]))),
      # ID=c(rep(paste0(abc[i]," - ", datalist$E1$Scenario[i]), length(Cout[[i]]))),
      value=c(Cout[[i]]),
      site=c(datalist$E1$Site[i], length(Cout[[i]])))
  }
  # ParSite<-NULL
  # sq<-seq(1,7,by=1)
  # for(j in 1:4){
  #   ParSite[[j]]<-rbind(Par[[sq[1]+(7*(j-1))]], Par[[sq[2]+(7*(j-1))]],Par[[sq[3]+(7*(j-1))]],
  #                       Par[[sq[4]+(7*(j-1))]],Par[[sq[5]+(7*(j-1))]],Par[[sq[6]+(7*(j-1))]],
  #                       Par[[sq[7]+(7*(j-1))]])
  # }
  # ParSite
  # out<-list(Par=Par)
  # out
  Parx<-do.call(rbind.data.frame, Par)
  Parx
  
}



makerandsComSite<-function(datalist, nalts, num_ex){
  standardmat<-datalist
  combined<-matrix(nrow=4, ncol=nalts)
  E1out<-E2out<-E3out<-Cout<-E4out<-E5out<-alphas<-betas<-NULL
  for(i in 1:nalts){
    for(e in 1:num_ex){
      temp<-beta.fit(as.numeric(datalist[[e]][i,6:9]))
      standardmat[[e]][i,6:9]<-c(as.numeric(quantile(temp$rands, c(0.05))), temp$ml.data, 
                                 as.numeric(quantile(temp$rands, c(0.95))), datalist[[e]]$Conf[i])
    }
    E1out[[i]]<-beta.fit(vect=as.numeric(standardmat[[1]][i, 6:9]))
    E2out[[i]]<-beta.fit(vect=as.numeric(standardmat[[2]][i, 6:9]))
    E3out[[i]]<-beta.fit(vect=as.numeric(standardmat[[3]][i, 6:9]))
    E4out[[i]]<-beta.fit(vect=as.numeric(standardmat[[4]][i, 6:9]))
    E5out[[i]]<-beta.fit(vect=as.numeric(standardmat[[5]][i, 6:9]))
    alphas[[i]]<-c(E1out[[i]]$alpha, E2out[[i]]$alpha, E3out[[i]]$alpha,
                   E4out[[i]]$alpha, E5out[[i]]$alpha)
    betas[[i]]<-c(E1out[[i]]$beta, E2out[[i]]$beta, E3out[[i]]$beta, 
                  E4out[[i]]$beta, E5out[[i]]$beta)
    Cout[[i]]<-replicate(300000, rmixbetatake2(alphas[[i]], betas[[i]], num_ex))
    combined[,i]<-c(as.numeric(quantile(Cout[[i]], c(0.05))),
                    mean(Cout[[i]]),
                    as.numeric(quantile(Cout[[i]], c(0.95))),
                    0.9)
  }
  Par<-NULL
  for(i in 1:nalts){
    Par[[i]]<-data.frame(
      ID=c(rep(datalist$E1$Site[i], length(Cout[[i]]))),
      value=c(Cout[[i]]),
      scenario=c(rep(datalist$E1$Scenario[i], length(Cout[[i]]))))
  }
  ParSite<-NULL
  sq<-seq(1,7,by=1)
  for(j in 1:2){
    ParSite[[j]]<-rbind(Par[[sq[1]+(7*(j-1))]], Par[[sq[2]+(7*(j-1))]],Par[[sq[3]+(7*(j-1))]],
                        Par[[sq[4]+(7*(j-1))]],Par[[sq[5]+(7*(j-1))]],Par[[sq[6]+(7*(j-1))]],
                        Par[[sq[7]+(7*(j-1))]])
  }
  ParSite
  # out<-list(Par=Par)
  # out
  Parx<-do.call(rbind.data.frame, ParSite)
  Parx
  
}


makerandsComSite_gamma<-function(datalist, nalts, num_ex){
  standardmat<-datalist
  combined<-matrix(nrow=4, ncol=nalts)
  E1out<-E2out<-E3out<-Cout<-E4out<-E5out<-alphas<-betas<-NULL
  for(i in 1:nalts){
    for(e in 1:num_ex){
      temp<-gamma.fit(as.numeric(datalist[[e]][i,6:9]))
      standardmat[[e]][i,6:9]<-c(as.numeric(quantile(temp$rands, c(0.05))), temp$ml.data, 
                                 as.numeric(quantile(temp$rands, c(0.95))), datalist[[e]]$Conf[i])
    }
    E1out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[1]][i, 6:9]))
    E2out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[2]][i, 6:9]))
    E3out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[3]][i, 6:9]))
    E4out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[4]][i, 6:9]))
    E5out[[i]]<-gamma.fit(vect=as.numeric(standardmat[[5]][i, 6:9]))
    alphas[[i]]<-c(E1out[[i]]$alpha, E2out[[i]]$alpha, E3out[[i]]$alpha,
                   E4out[[i]]$alpha, E5out[[i]]$alpha)
    betas[[i]]<-c(E1out[[i]]$beta, E2out[[i]]$beta, E3out[[i]]$beta, 
                  E4out[[i]]$beta, E5out[[i]]$beta)
    Cout[[i]]<-replicate(300000, rmixgammas(alphas[[i]], betas[[i]], num_ex))
    combined[,i]<-c(as.numeric(quantile(Cout[[i]], c(0.05))),
                    mean(Cout[[i]]),
                    as.numeric(quantile(Cout[[i]], c(0.95))),
                    0.9)
  }
  Par<-NULL
  for(i in 1:nalts){
    Par[[i]]<-data.frame(
      ID=c(rep(datalist$E1$Site[i], length(Cout[[i]]))),
      value=c(Cout[[i]]),
      scenario=c(rep(datalist$E1$Scenario[i], length(Cout[[i]]))))
  }
  ParSite<-NULL
  sq<-seq(1,7,by=1)
  for(j in 1:2){
    ParSite[[j]]<-rbind(Par[[sq[1]+(7*(j-1))]], Par[[sq[2]+(7*(j-1))]],Par[[sq[3]+(7*(j-1))]],
                        Par[[sq[4]+(7*(j-1))]],Par[[sq[5]+(7*(j-1))]],Par[[sq[6]+(7*(j-1))]],
                        Par[[sq[7]+(7*(j-1))]])
  }
  ParSite
  # out<-list(Par=Par)
  # out
  Parx<-do.call(rbind.data.frame, ParSite)
  Parx
  
}

