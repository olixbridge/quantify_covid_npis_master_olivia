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
library(dlm)
library(dplyr)


#Quebec
#####
province="QC"
alldata<-read.csv('./Covid19Canada/timeseries_prov/cases_timeseries_prov.csv') 

ON<-alldata[alldata$province=='Quebec',]
ON.date<-as.Date(ON$date_report,format="%d-%m-%Y", tryFormats= "%Y-%m-%d") 
ON.count<-ON$cases
ON.mydata<-data.frame(date=as.Date(ON.date,format="%d-%m-%Y", tryFormats= "%Y-%m-%d") ,
                      count=ON.count)
ON.mydata<-ON.mydata %>% arrange(date)
ON.mydata<-ON.mydata %>% filter("2020-03-15"<date)%>% filter( date<'2021-12-10')
ON.mydata$y<-ON.mydata$count
ON.mydata$y[ON.mydata$date=="2020-12-26"]<-2101 #december 26  #146 when 2020 08 01
ON.mydata$y[ON.mydata$date=="2020-01-01"]<-3659
ON.mydata$y[ON.mydata$date=='2020-05-03'] <- 1102
ON.mydata$y[ON.mydata$date=='2021-11-23'] <- 1000
ON.mydata$y[ON.mydata$date=='2021-11-24'] <- 1200
ON.mydata$y[ON.mydata$date=='2021-11-25']  <- 1300
ON.mydata$y[ON.mydata$date=='2021-09-25'] <- 732
ON.mydata$y[ON.mydata$date=='2021-09-26'] <- 659
ON.mydata$y[ON.mydata$date=='2021-09-27'] <- 608
ON.mydata$y[ON.mydata$date=='2021-10-15'] <- 650
ON.mydata$y[ON.mydata$date=='2021-10-16'] <-705
ON.mydata$y[ON.mydata$date=='2020-12-05'] <-1600

for(i in 6:length(ON.mydata$y)){
  if(ON.mydata$y[i]<=5){
    ON.mydata$y[i]=ON.mydata$y[i-1]+1
  }
  if(ON.mydata$y[i]>4000){
    ON.mydata$y[i]=ON.mydata$y[i-1]+35
  }
}


#####

#Ontario
#####
province="ON"
alldata<-read.csv('/Users/oliviashi/Documents/MASTER_RESEARCH/Covid19Canada/timeseries_prov/cases_timeseries_prov.csv') 

ON<-alldata[alldata$province=='Ontario',] 
ON.date<-as.Date(ON$date_report,format="%d-%m-%Y", tryFormats= "%Y-%m-%d") 
ON.count<-ON$cases
ON.mydata<-data.frame(date=as.Date(ON.date,format="%d-%m-%Y", tryFormats= "%Y-%m-%d") ,
                      count=ON.count)
ON.mydata<-ON.mydata %>% arrange(date)
ON.mydata<-ON.mydata %>% filter("2020-04-01"<date)%>% filter( date<'2021-12-10')
ON.mydata$y<-ON.mydata$count
ON.mydata$y[ON.mydata$date=='2020-08-22']<-165
ON.mydata$y[ON.mydata$date=='2020-08-23']<-178
ON.mydata$y[ON.mydata$date=='2020-08-25']<-130
ON.mydata$y[ON.mydata$date=='2020-08-30']<-189
ON.mydata$y[ON.mydata$date=='2020-09-01']<-190
ON.mydata$y[ON.mydata$date=='2020-12-25']<-2143
ON.mydata$y[ON.mydata$date=='2021-01-01']<-3012
ON.mydata$y[ON.mydata$date=='2021-05-24']<-1760
ON.mydata$y[ON.mydata$date=='2021-11-28']<-570
ON.mydata$y[ON.mydata$date=='2021-11-29']<-800
ON.mydata$y[ON.mydata$date=='2021-11-20']<-760
ON.mydata$y[ON.mydata$date=='2021-11-21']<-900
ON.mydata$y[ON.mydata$date=='2021-11-22']<-670
ON.mydata$y[ON.mydata$date=='2021-11-06']<-531
ON.mydata$y[ON.mydata$date=='2021-11-07']<-490
ON.mydata$y[ON.mydata$date=='2021-11-08']<-481
ON.mydata$y[ON.mydata$date=='2021-12-04']<-1269
ON.mydata$y[ON.mydata$date=='2021-12-05']<-1409
ON.mydata$y[ON.mydata$date=='2021-12-06']<-1102
#####

########
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

yt<-log(ON.mydata$y)
fit1 <- dlmMLE(y=yt,parm=c(-1,-1,-1,log(yt[1]),rep(0,6)),build=ssm1,hessian=T) # estimate parameters
mod1 <- ssm1(fit1$par)       # filter and smooth and get standard errors
mod1f <- dlmFilter(yt,mod1)  # filtered estimates
mod1s <- dlmSmooth(mod1f)    # smoothed estimates
#sapply(dlmSvd2var(mod1f$U.C,mod1f$D.C),diag)
sigV<-exp(fit1$par[1]/2)



xtfilt <- ts(mod1f$m[-1,1],start=1)               # filtered values as ts object

par(mar=c(4,4,1,0))
xtp<-apply(mod1f$m[-1,2:7],1,sum)
xtm<-mod1f$m[-1,1]+mod1f$m[-1,2]
plot(exp(xtm),type='l',main='Estimated mean',ylab='Count')
points(exp(yt[-1]),pch=19,cex=0.5)

xtsmooth <- ts(mod1s$s[-1,1],start=1)             # smoothed values



nreps<-2000
ysamp<-matrix(0,nrow=length(yt),ncol=nreps)
for (i in 1:nreps){
muv<-dropFirst(dlmBSample(mod1f))
ysamp[,i]<-rpois(nrow(ysamp),exp(muv[,1]+rnorm(nrow(ysamp))*sigV))
}

ysummary<-apply(ysamp,1,quantile,prob=c(0.025,0.50,0.975))
par(mar=c(4,4,2,0))

df<-data.frame(y=exp(yt),date=ON.mydata$date,
               lower=ysummary[1,],upper=ysummary[3,])
########

#window size can be changed here
##########################
window_size=5
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

#The first fit
prior.m<-10
prior.s<-10

actual<-ysamp[,1]

opo<-optim(par=c(log(7-1),log(0.5)),likelihood_loss.0,case=actual,
     rmean=prior.m,rstd=prior.s,plt=FALSE,control = list(maxit=500,fnscale=-1))


m.max<-exp(opo$par[1])+1
s.max<-exp(opo$par[2])
#take example simulation, estimate the parameters of that simulation -> 
#in order to get the serial interval for estimate R


# t1<-seq(2,length(actual),by=1)
# t0<-t1
t0<-seq(2,length(actual)-window_size)
t1<-t0+window_size
res_1 <- estimate_R(actual, method = "parametric_si",
              config = make_config(list(mean_si = m.max, std_si = s.max,
                                        mean_prior = prior.m, std_prior = prior.s, 
                                        t_start=t0, t_end=t1)))

first_R<-res_1$R$`Mean(R)`

dfr<-data.frame(date=ON.mydata$date[(window_size+2):length(ON.mydata$date)],R=first_R)


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
plot(dfr$date[2: length(dfr$date)],Rest[2,-1][2: length(dfr$date)],type='l',xaxt='n',ylim=range(0,4),ylab=expression(R[t]),xlab='Date')

at <- seq(from = min(ON.mydata$date), to = max(ON.mydata$date), by = "2 months")
axis.Date(1, at=at, format="%b %Y", cex.axis = .7)
lines(dfr$date[2: length(dfr$date)],Rest[1,-1][2: length(dfr$date)],col='red',lty=2)
lines(dfr$date[2: length(dfr$date)],Rest[3,-1][2: length(dfr$date)],col='red',lty=2)
##########################

#window size = 1
##########
window_size=1
estimateW.like<-function(case,mean_w,std_w,rmean,rstd){
  t1<-seq(14,length(case),by=1)
  t0<-t1
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

#The first fit
prior.m<-10
prior.s<-10

actual<-ysamp[,1]

opo<-optim(par=c(log(7-1),log(0.5)),likelihood_loss.0,case=actual,
           rmean=prior.m,rstd=prior.s,plt=FALSE,control = list(maxit=500,fnscale=-1))
m.max<-exp(opo$par[1])+1
s.max<-exp(opo$par[2])


t1<-seq(2,length(actual),by=1)
t0<-t1

res_1 <- estimate_R(actual, method = "parametric_si",
                    config = make_config(list(mean_si = m.max, std_si = s.max,
                                              mean_prior = prior.m, std_prior = prior.s, t_start=t0, t_end=t1)))

first_R<-res_1$R$`Mean(R)`

dfr<-data.frame(date=ON.mydata$date[-1],R=first_R)

Rsamp<-matrix(0,ncol=length(yt),nrow=nreps)
parsamp<-matrix(0,ncol=2,nrow=nreps)
for(irep in 1:nreps){
  actual<-ysamp[,irep]
  res_1 <- estimate_R(actual, method = "parametric_si",
                      config = make_config(list(mean_si = m.max, std_si = s.max,
                                                mean_prior = prior.m, std_prior = prior.s, t_start=t0, t_end=t1)))
  Rsamp[irep,-1]<-res_1$R$`Mean(R)`
  parsamp[irep,]<-c(m.max,s.max)
}

Rest<-apply(Rsamp,2,quantile,prob=c(0.025,0.50,0.975),na.rm=T)
par(mar=c(4,4,2,0))
plot(dfr$date,Rest[2,-1][1:length(dfr$date)],type='l',xaxt='n',ylim=range(0,4),ylab=expression(R[t]),xlab='Date')
at <- seq(from = min(ON.mydata$date), to = max(ON.mydata$date), by = "month")
axis.Date(1, at=at, format="%m/%y", cex.axis = .7)
lines(dfr$date,Rest[1,-1][1:length(dfr$date)],col='red',lty=2)
lines(dfr$date,Rest[3,-1][1:length(dfr$date)],col='red',lty=2)
##########



#just for csv
######
dfr$lower=Rest[1,-1][1:length(dfr$date)]
dfr$upper=Rest[3,-1][1:length(dfr$date)]
dfr$R=Rest[2,-1][1:length(dfr$date)]
dfr$t_end=res_1$R$t_end
write.csv(df, sprintf("/Users/oliviashi/Documents/MASTER_RESEARCH/data_output/df_output_2000_%s_dlm_ws%i.csv",province,window_size))
write.csv(dfr, sprintf("/Users/oliviashi/Documents/MASTER_RESEARCH/data_output/dfr_output_2000_%s_dlm_ws%i.csv",province,window_size))
######





#just for plots
######
dfr$lower=Rest[1,-1][1:length(dfr$date)]
dfr$upper=Rest[3,-1][1:length(dfr$date)]
dfr$R=Rest[2,-1][1:length(dfr$date)]
dfr$t_end=res_1$R$t_end

rplot_14<-ggplot(dfr,aes(x=date))+
  geom_point(aes(y=R),cex=0.5) +
  ylab(expression(R[t]))+
  geom_line(aes(y=lower,col="lower"))+
  geom_line(aes(y=upper,col="upper")) +
  ylim(0,4)+
  labs(colour="Confidence Interval")+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = c(as.Date('2020-09-01'), as.Date('2021-07-01')))+
  theme(legend.position = c(0.9, 0.8),legend.title = element_text(size = 9),legend.text =
          element_text(size = 8),legend.key.size=unit(0.5,"lines"),legend.key = element_blank())

ggarrange(rplot_1+ggtitle(sprintf("Confidence Interval for Rt with %d iterations for Québec, window size = 1",nreps)),
          rplot_5+ggtitle(sprintf("Confidence Interval for Rt with %d iterations for Québec, window size = 5",nreps)),
          rplot_7+ggtitle(sprintf("Confidence Interval for Rt with %d iterations for Québec, window size = 7",nreps)),
          rplot_14+ggtitle(sprintf("Confidence Interval for Rt with %d iterations for Québec, window size = 14",nreps)),
          nrow=4)#Québec
######



fig1<-ggplot(df,aes(x=date))+geom_point(aes(y=y),cex=0.5) +
  ylab("cases")+geom_line(aes(y=lower,col="lower"))+
  geom_line(aes(y=upper,col="upper")) +
  ggtitle(sprintf("Confidence Interval for cases with %d iterations",nreps)) +
  labs(colour="Confidence Interval")+
  scale_x_date(breaks = "1 month", date_labels = "%m/%y")+
  theme(legend.position = c(0.8, 0.8),legend.title = element_text(size = 5),legend.text =
          element_text(size = 5),legend.key.size=unit(0.5,"lines"),legend.key = element_blank())

fig1

