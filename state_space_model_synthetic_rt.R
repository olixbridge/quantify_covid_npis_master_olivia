

#intialize data and generate synthetic rt
######
library(lubridate)
library(plyr)
library(EpiEstim)
library(tidyverse)
library(zoo)
library(ggplot2)
library(reshape2)
library(readxl)
library(MASS)
library(smooth)
library(forecast)
library(ggpubr)
require(methods)
library(dplyr)
library(dlm)
province="QC"
alldata<-read.csv('./Covid19Canada/timeseries_prov/cases_timeseries_prov.csv') 
ON<-alldata[alldata$province=='Quebec',]
ON.date<-as.Date(ON$date_report,format="%d-%m-%Y", tryFormats= "%Y-%m-%d") 
ON.count<-ON$cases
ON.mydata<-data.frame(date=as.Date(ON.date,format="%d-%m-%Y",tryFormats= "%Y-%m-%d"),count=ON.count)
ON.mydata<-ON.mydata %>% arrange(date)
ON.mydata<-ON.mydata %>% filter("2020-08-01"<date)%>% filter( date<'2021-07-01')
ON.mydata$y<-ON.mydata$count
ON.mydata$y[146]<-2101 #december 26
ON.mydata$y[153]<-3659

actual<-ON.mydata$y
tryR<-rep(0,1000)
set.seed(137)
tryR[1]<-rnorm(1,0.1,0.02)
for(i in 2:1000){
  tryR[i]=0.9*tryR[i-1]+rnorm(1,0,0.04)
}
tryR=tryR+1
# plot(x=seq(1,length(tryR),1),y=tryR,type='l')
prior.m=4#1.1
prior.s=2
aval=7#4.4
bval=1.1
tryW<-dgamma(1:1000,aval,bval)
tryW=tryW/sum(tryW)
w=tryW/sum(tryW)

n=length(w)
tryI=c(actual[1:14],rep(1,(n-14)))
Rvec<-unlist(tryR)
R_val<-Rvec[!is.na(Rvec)]

#find poisson case
Iv<-c(actual[1:14],seq(1,(n-14),1))
Rvec<-unlist(R_val)
Rvec<-c(Rvec,rep(Rvec[length(Rvec)],7))

for (i in 14:n) {
  svec<-c(1:(i-1))
  svec<-svec[w[svec]>0]
  Iv[i]<-ceiling(Rvec[i]*sum(Iv[i-svec]*w[svec]))}

df_original<-data.frame(date=seq(1,1000,1),Iv=Iv,tryR=tryR)

synthetic_cases<-ggplot(data=df_original,aes(x=date))+
  geom_point(aes(y=Iv),cex=0.5)+
  ylab("Cases")+
  theme(axis.title.x=element_blank())

synthetic_rt<-ggplot(data=df_original,aes(x=date))+
  geom_line(aes(y=tryR),cex=0.5)+
  ylab(expression(R[t]))+
  ylim(0.7,1.5)+
  theme(axis.title.x=element_blank())

synthetic_cases_rt<-ggarrange(synthetic_cases,synthetic_rt,nrow=2)
synthetic_cases_rt
ggsave("../LatexImages/synthetic_cases_rt.png",synthetic_cases_rt,scale=1)
knitr::plot_crop("../LatexImages/synthetic_cases_rt.png")
list_mean_w<-list()
list_std_w<-list()
listallR<-list()
list_w<-list()
######


#2000 iterations
########
synthetic_case<-data.frame(y<-Iv,
                           count<-Iv,
                           date<-seq(1,1000,1))

parm_rest <- function(parm){ # set parameter restrictions (only variances here)
  return(exp(parm))}

ssm1 <- function(parm){
  parm <- parm_rest(parm)
  Fmat<-matrix(c(1,1,0,0,0,0,0),nrow=1)
  Gmat<-matrix(0,7,7)
  Gmat[1,1]<-1
  Gmat[2,2:7]<--1
  for(j in 3:7){Gmat[j,j-1]<-1}    
  return( dlm(FF=Fmat,V=parm[1],
              GG=Gmat,W=diag(c(parm[2],parm[3],rep(0,5))),
              m0=matrix(parm[-c(1:3)],ncol=1),C0=diag(rep(10^7,7))))
}

yt<-log(synthetic_case$y)
fit1 <- dlmMLE(y=yt,parm=c(-1,-1,-1,log(yt[1]),rep(0,6)),build=ssm1,hessian=T) # estimate parameters
mod1 <- ssm1(fit1$par)       # filter and smooth and get standard errors
mod1f <- dlmFilter(yt,mod1)  # filtered estimates
mod1s <- dlmSmooth(mod1f)    # smoothed estimates

sigV<-exp(fit1$par[1]/2)+0.05 #exp(fit1$par[1]/2)
xtfilt <- ts(mod1f$m[-1,1],start=1)               # filtered values as ts object

# par(mar=c(4,4,1,0))
# xtp<-apply(mod1f$m[-1,2:7],1,sum)
# xtm<-mod1f$m[-1,1]+mod1f$m[-1,2]
# plot(exp(xtm),type='l',main='Estimated mean',ylab='Count')
# points(exp(yt[-1]),pch=19,cex=0.5)

xtsmooth <- ts(mod1s$s[-1,1],start=1)             # smoothed values

nreps<-200
ysamp<-matrix(0,nrow=length(yt),ncol=nreps)
for (i in 1:nreps){
  muv<-dropFirst(dlmBSample(mod1f))
  ysamp[,i]<-rpois(nrow(ysamp),exp(muv[,1]+rnorm(nrow(ysamp))*sigV))
}

ysummary<-apply(ysamp,1,quantile,prob=c(0.025,0.50,0.975))
# par(mar=c(4,4,2,0))

df<-data.frame(y=exp(yt),
               date=synthetic_case$date,
               lower=ysummary[1,],
               upper=ysummary[3,])
########

#define functions and run the first fit
##########################
window_size=7
estimateW.like<-function(case,mean_w,std_w,rmean,rstd){
  t0<-seq(2,length(case)-window_size)
  t1<-t0+window_size
  res_i <- estimate_R(case, method = "parametric_si",
                      config = make_config(list(mean_si = mean_w,std_si = std_w,
                                                mean_prior = rmean, std_prior = rstd,
                                                t_start=t0,t_end=t1)))
  R_val<-res_i$R$`Mean(R)`
  w<-res_i$si_distr
  Rvec<-rep(1,length(case))
  Rvec[t1]<-R_val
  Iv<-case
  svec<-t1
  sv0<-t1[1]-2
  for (i in svec) {
    sv0<-c(sv0,i-1)
    svec<-sv0[w[sv0]>0]
    Iv[i]<-Rvec[i]*sum(case[i-svec]*w[svec])
  }
  return(Iv)
}

likelihood_loss.0<-function(par,case,rmean,rstd,plt=TRUE){
  mean_w<-exp(par[1])+1
  std_w<-exp(par[2])
  if(plt){print(c(mean_w,std_w))}
  muvec<-estimateW.like(case,mean_w,std_w,rmean,rstd)
  llike<-sum(dpois(case[-c(1:14)],muvec[-c(1:14)],log=T))
  if(plt){points(mean_w,std_w,col='blue',pch=19,cex=0.75)}
  return(llike)
}

prior.m<-4
prior.s<-2

actual<-ysamp[,1]
opo<-optim(par=c(log(7-1),log(0.5)),likelihood_loss.0,case=actual,
           rmean=prior.m,rstd=prior.s,plt=FALSE,control = list(maxit=500,fnscale=-1))

m.max<-exp(opo$par[1])+1
s.max<-exp(opo$par[2])
#take example simulation, estimate the parameters of that simulation -> 
#in order to get the serial interval for estimate R
t0<-seq(2,length(actual)-window_size)
t1<-t0+window_size
res_1 <- estimate_R(actual, method = "parametric_si",
                    config = make_config(list(mean_si = m.max, std_si = s.max,
                                              mean_prior = prior.m, std_prior = prior.s, 
                                              t_start=t0, t_end=t1)))

first_R<-c(res_1$R$`Mean(R)`,rep(0.989,8))
NonNAindex <- which(!is.na(first_R))
firstNonNA <- min(NonNAindex)
for(k in 1:firstNonNA){first_R[k]<-first_R[firstNonNA]}

listallR<-list()
listallR[1]<-list(first_R)
list_w[1]<-list(res_1$si_distr)

Rsamp<-matrix(0,ncol=length(yt)-window_size,nrow=nreps)
parsamp<-matrix(0,ncol=2,nrow=nreps)
for(irep in 1:nreps){
  actual<-ysamp[,irep]
  res_1 <- estimate_R(actual, method = "parametric_si",
                      config = make_config(list(mean_si = m.max, std_si = s.max,
                                                mean_prior = prior.m, std_prior = prior.s, 
                                                t_start=t0, t_end=t1)))
  Rsamp[irep,-1]<-res_1$R$`Mean(R)`
  parsamp[irep,]<-c(m.max,s.max)
}


Rest<-apply(Rsamp,2,quantile,prob=c(0.025,0.50,0.975),na.rm=T)
par(mar=c(4,4,2,0))

dfr_state_space<-data.frame(date=synthetic_case$date,
                            R=first_R,
                            lower=c(Rest[1,-1],rep(NA,8)),
                            upper=c(Rest[3,-1],rep(NA,8)))

##########################


#just for plots (state space)
######
dfr_state_space$lower=c(Rest[1,-1],rep(NA,8))
dfr_state_space$upper=c(Rest[3,-1],rep(NA,8))
dfr_state_space$R=Rest[2,-1]

df<-data.frame(y=exp(yt),
               date=synthetic_case$date,
               lower=ysummary[1,],
               upper=ysummary[3,])

fig_rt_emulation<-ggplot(dfr_state_space,aes(x=date))+
  geom_line(aes(y=R),cex=0.5) +
  ylab(expression(R[t]))+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.1, linetype="dashed",color="grey")+
  ylim(0.5,2)+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        axis.title.x=element_blank())

fig_case_emulation<-ggplot(df,aes(x=date))+
  geom_line(aes(y=y),cex=0.5) +
  ylab("cases")+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.1, linetype="dashed",color="grey")+
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size=unit(0.5,"lines"),
        legend.key = element_blank(),
        axis.title.x=element_blank())

ggarrange(fig_case_emulation,fig_rt_emulation,nrow=2)
######

# Only run it if skipping state space model
######
opo<-optim(par=c(log(7-1),log(0.5)),likelihood_loss.1,case=Iv,
           rmean=prior.m,rstd=prior.s,plt=FALSE,control = list(maxit=500,fnscale=-1))

m.max<-exp(opo$par[1])+1
s.max<-exp(opo$par[2])
list_mean_w[1]=exp(opo$par[1])+1
list_std_w[1]=exp(opo$par[2])
#take example simulation, estimate the parameters of that simulation -> 
#in order to get the serial interval for estimate R

window_size=7
t0<-seq(2,length(Iv)-window_size)
t1<-t0+window_size
res_1 <- estimate_R(Iv, method = "parametric_si",
                    config = make_config(list(mean_si = m.max, std_si = s.max,
                                              mean_prior = prior.m, std_prior = prior.s, 
                                              t_start=t0, t_end=t1)))


first_R<-c(res_1$R$`Mean(R)`,rep(0.989,8))
NonNAindex <- which(!is.na(first_R))
firstNonNA <- min(NonNAindex)
for(k in 1:firstNonNA){first_R[k]<-first_R[firstNonNA]}

listallR<-list()
listallR[1]<-list(first_R)
list_w[1]<-list(res_1$si_distr)
######

#define functions (find_poisson_case, likelihood_loss.1, simulate_forward)
####################
find_poisson_case<-function(case,mean_w,std_w,rmean,rstd){
  nv=length(case)
  t0<-seq(2,length(case)-6)
  t1<-t0+6
  res_i <- estimate_R(case, method = "parametric_si",
                      config = make_config(list(mean_si = mean_w,
                                                std_si = std_w, 
                                                mean_prior = rmean,
                                                std_prior=rstd,
                                                t_start=t0,t_end=t1)))
  R_val<-res_i$R$`Mean(R)`
  R_val<-R_val[!is.na(R_val)]
  nv<-min(length(R_val),length(case))
  Iv<-c(case[1:14],seq(1,(nv-14),1))
  w<-res_i$si_distr
  Rvec<-unlist(R_val)
  Rvec<-c(Rvec,rep(Rvec[length(Rvec)],7))
  for (i in 14:nv) {
    svec<-c(1:(i-1))
    svec<-svec[w[svec]>0]
    Iv[i]<-Rvec[i]*sum(case[i-svec]*w[svec])
  }
  # for (i in svec) {
  #   sv0<-c(sv0,i-1)
  #   svec<-sv0[w[sv0]>0]
  #   Iv[i]<-Rvec[i]*sum(case[i-svec]*w[svec])
  # }
  return(Iv)
}


likelihood_loss.1<-function(par,case,rmean,rstd,plt=F){
  # if((par[1]<1)|(par[2]>20)){
  #   return(-1e10)
  # }
  # if(par[2]<0.1){
  #   return(-1e10)
  # }
  # mean_w<-exp(par[1])+1
  # std_w<-exp(par[2])
  # if(plt){print(c(mean_w,std_w))}
  # muvec<-find_poisson_case(case,mean_w,std_w,rmean,rstd)
  # llike<-sum(dpois(case[-c(1:14)],muvec[-c(1:14)],log=T))
  # if(plt){points(mean_w,std_w,col='blue',pch=19,cex=0.75)}
  # return(llike)
  mean_w<-exp(par[1])+1
  std_w<-exp(par[2])
  if(mean_w<1.01){
    return(-1e8)
  }
  if(std_w<=0.01){
    return(-1e8)
  }
  if(plt){print(c(mean_w,std_w))}
  muvec<-find_poisson_case(case,mean_w,std_w,rmean,rstd)
  # estimateW.like(case,mean_w,std_w,rmean,rstd)
  llike<-sum(dpois(case[-c(1:14)],muvec[-c(1:14)],log=T))
  if(plt){points(mean_w,std_w,col='blue',pch=19,cex=0.75)}
  return(llike)
}


simulate_forward<-function(wv){
  nv=1000
  Iv=c(Iv[1:14],seq(1,(nv-14),1))#trybaseline?
  Rvec<-unlist(listallR[1])
  for (i in 14:nv) {
    svec=c(1:(i-1))
    svec<-svec[wv[svec]>0]
    poisson_mean=Rvec[i]*sum(Iv[i-svec]*wv[svec])
    if(is.na(rpois(1, lambda=poisson_mean))){
      Iv[i]=Iv[i-1]
    }
    else{
      Iv[i]=rpois(1, lambda=poisson_mean)
    }
  }      
  return(Iv)
}


#m.max=11.5,s.max=3.02
par1=c(log(7-1),log(0.5))
# c(log(m.max-1),log(s.max))
####################


#no need please skip
#default prior.m is 4 and the fit is a lot lower than actual   4,2   6,2
####################
prior.m=4
prior.s=2
try_opo<-optim(par=par1,likelihood_loss.1,
               case=Iv,
               rmean=prior.m,
               rstd=prior.s,method="BFGS",
               control = list(maxit=1000,fnscale=-1,trace=10))

# list_mean_w[1]=exp(try_opo$par[1])+1
# list_std_w[1]=exp(try_opo$par[2])

#1.1,0.2
tryBaseline<-find_poisson_case(case=Iv, mean_w=list_mean_w[[1]], std_w=list_std_w[[1]], rmean=prior.m, rstd=prior.s)
tryBaseline<-c(tryBaseline,rep(tryBaseline[[length(tryBaseline)]],9))[1:1000]
# this is mixed with state space model 
df<-data.frame(actual=Iv, date=synthetic_case$date,
               first_fit_simulation=c(tryBaseline,rep(tryBaseline[[length(tryBaseline)]],9))[1:1000])

ggplot(df,aes(x=date))+
  geom_point(aes(y=actual),cex=0.5) +ylim(0,5000)+
  geom_line(aes(y=first_fit_simulation),cex=0.5,col="red") +
  ylab("cases")+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),
        legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size=unit(0.5,"lines"),
        legend.key = element_blank(),
        axis.title.x=element_blank())
####################

#no need please skip 
############
res_1 <- estimate_R(tryBaseline, method = "parametric_si",
                    config = make_config(list(mean_si = list_mean_w[[1]],std_si =list_std_w[[1]],
                                              mean_prior=prior.m,
                                              std_prior=prior.s)))

first_R<-c(res_1$R$`Mean(R)`,rep(0.989,8))
NonNAindex <- which(!is.na(first_R))
firstNonNA <- min(NonNAindex)
for(k in 1:firstNonNA){first_R[k]<-first_R[firstNonNA]}
list_simulated_I<-list()
listallR<-list()
listallR[1]<-list(first_R)
list_w[1]<-list(res_1$si_distr)

list_w[1]<-list(res_1$si_distr)
list_simulated_I[1]<-list(tryBaseline)#list(Iv)#
############


#the iteration for the cori model
############
num=5
list_simulated_I<-list()
list_simulated_I[1]<-list(Iv)#list(tryBaseline)
for(i in 2:num){
  print(i)
  list_simulated_I[i]<-list(simulate_forward(wv=list_w[[1]]))
  incid_sim=list_simulated_I[[i]]
  xstart<-par1
  # xstart<-c(log(m.max-1),log(s.max))
  # xstart<-c(log(list_mean_w[[i-1]]-1),log(list_std_w[[i-1]]))+rnorm(2,0,0.1)
  op<-optim(par=xstart, likelihood_loss.1,
            case=Iv, rmean=prior.m, rstd=prior.s,
            method="BFGS",
            control = list(maxit=500,fnscale=-1,trace=10))[[1]]
  
  list_mean_w[i]<-exp(op[1])+1
  list_std_w[i]<-exp(op[2])
  res_i <- estimate_R(incid_sim, method = "parametric_si",
                      config = make_config(list(mean_si = m.max, std_si = s.max,
                                                mean_prior = prior.m, std_prior = prior.s, 
                                                t_start=t0, t_end=t1)))    
  ith_R<-res_i$R$`Mean(R)`
  NonNAindex <- which(!is.na(ith_R))
  firstNonNA <- min(NonNAindex)
  for(k in 1:firstNonNA){ith_R[k]<-ith_R[firstNonNA]}
  listallR[i]<-list(ith_R)
  list_w[i]<-list(res_i$si_distr)
}

n=length(Iv)-7
dfr<-data.frame(actual_R=seq(1,n,1),lower=seq(1,n,1),
                upper=seq(1,n,1),date=seq(1,n,1))

df<-data.frame(actual=seq(1,n,1),lower=seq(1,n,1),
               upper=seq(1,n,1),date=seq(1,n,1))

for(i in 2:n){
  new<-list()
  newr<-list()
  if(i>5){
    dfr$actual_R[i]=listallR[[2]][i]}
  else{dfr$actual_R[i]=listallR[[2]][i]}
  df$actual[i]=Iv[i]
  for(j in 2:num){
    new[j]=(list_simulated_I[[j]][[i]])
    newr[j]=(listallR[[j]][[i]])
  }
  dfr$lower[i]=quantile(unlist(newr),prob=c(0.025,0.975))[[1]]
  dfr$upper[i]=quantile(unlist(newr),prob=c(0.025,0.975))[[2]]
  
  df$lower[i]=quantile(unlist(new),prob=c(0.025,0.975))[[1]]
  df$upper[i]=quantile(unlist(new),prob=c(0.025,0.975))[[2]]
}

fig_case_simulation<-ggplot(df,aes(x=date))+geom_point(aes(y=actual),cex=0.5) +
  ylab("cases")+geom_line(aes(y=lower,col="lower"))+
  geom_line(aes(y=upper,col="upper")) +
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),
        legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size=unit(0.5,"lines"),
        legend.key = element_blank(),
        axis.title.x=element_blank())

fig_case_simulation





# ggarrange(fig_case,fig_case_simulation,nrow=2)
# emulation_vs_simulation_rt<-ggarrange(fig_rt_emulation,fig_rt_simulation,nrow=2)
# ggsave("../LatexImages/emulation_vs_simulation_rt.png",emulation_vs_simulation_rt,scale=1)
# emulation_vs_simulation_rt
# ggsave("../LatexImages/emulation_vs_simulation_rt_zoomed.png",emulation_vs_simulation_rt,scale=1)
# 

############


#total plots
##########
fig_cori_r<-ggplot(dfr,aes(x=date))+
  geom_line(aes(y=actual_R),col="orange",cex=0.5)+
  geom_ribbon(aes(ymin=lower,ymax=upper),col="wheat3",alpha=0.1, linetype="dashed")+
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_text(size = 10),legend.text = element_text(size = 10),
        legend.key = element_blank(),axis.title.x=element_blank())+
  ylab(expression("R"[t]))+ylim(0.5,1.75)

fig_statespace_r<-ggplot(dfr_state_space,aes(x=date))+
  geom_line(aes(y=R),col="blue",cex=0.5)+
  geom_ribbon(aes(ymin=lower,ymax=upper),col="lightblue",alpha=0.1, linetype="dashed")+
  theme(axis.title.x=element_blank())+
  ylab(expression("R"[t]))+ylim(0.5,1.75)

emulation_vs_simulation_rt_zoomed<-ggarrange(fig_cori_r,fig_statespace_r,nrow=2,labels=c("A","B"))
emulation_vs_simulation_rt_zoomed

# ggsave("../LatexImages/emulation_vs_simulation_rt_zoomed.png",emulation_vs_simulation_rt_zoomed)
# knitr::plot_crop("../LatexImages/emulation_vs_simulation_rt_zoomed.png")
