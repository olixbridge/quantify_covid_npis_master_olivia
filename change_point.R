library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

#initiation
######
window_size=7
assign_npi_type <- function(df_ci_both,jurisdiction_val){
  s3 <- read_excel("./covid-19-intervention-timeline-in-canada-en.xlsx",sheet=3,col_names=TRUE,skip=2)
  s3$date <- as.Date(s3$`Start date`,format="%d-%m-%Y", tryFormats= "%Y-%m-%d")
  s3$cat <- s3$`Intervention Category`
  s3 <- s3 %>% dplyr::select("Entry ID","date","cat","Action","Jurisdiction","Description") %>% filter(Jurisdiction==jurisdiction_val)#Que.#Ont.
  s3 <- tibble(s3)
  da <- join(df_ci_both,s3,by="date",type="left",match="first")
  
  NPI <- da %>% dplyr::select(date,cat,Action,Description,R_actual_QC,R_upper_QC,R_lower_QC,I_actual_QC,I_upper_QC,I_lower_QC)
  #,R_actual_ON,R_upper_ON,R_lower_ON,I_actual_ON,I_upper_ON,I_lower_ON)
  
  NPI <- pivot_wider(data=NPI,names_from=cat,values_from=Description) %>% dplyr::select(date,Action,R_actual_QC,R_upper_QC,R_lower_QC,I_actual_QC,I_upper_QC,I_lower_QC,`Closures/openings`,Distancing)
  #,R_actual_ON,R_upper_ON,R_lower_ON,I_actual_ON,I_upper_ON,I_lower_ON,`Closures/openings`,Distancing)
  
  
  NPI$closures<-NA
  NPI$openings<-NA
  NPI$type<-NA
  for(i in 1:length(NPI$date)){
    if(is.na(NPI$Action[i])){
      a<-1 
    }
    else{
      if(grepl('school',NPI$`Closures/openings`[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Schools"
      }
      if(grepl('non-essential',NPI$`Closures/openings`[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Non-essential"
      }
      if(grepl('cinemas',NPI$`Closures/openings`[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Non-essential"
      }
      if(grepl('public places',NPI$`Closures/openings`[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Public spaces"
      }   
      if(grepl('bar',NPI$`Closures/openings`[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Non-essential"
      }   
      if(grepl('restaurant',NPI$`Closures/openings`[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Non-essential"
      }   
      if(grepl('parks',NPI$`Closures/openings`[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Public spaces"
      }   
      if(grepl('museums',NPI$`Closures/openings`[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Public spaces"
      }   
      if(grepl('curfew',NPI$Distancing[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Curfew"
      }
      if(grepl('gathering',NPI$Distancing[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Gathering"
      }
      if(grepl('worship',NPI$Distancing[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Public spaces"
      }
      if(grepl('worship',NPI$Distancing[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Public spaces"
      }
      if(grepl('non-essential',NPI$Distancing[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Non-essential"
      }
    }
  }
  for(i in 1:length(NPI$date)){
    if(is.na(NPI$Action[i])){
      a<-1 
    }
    else if(NPI$Action[i]=="New"){
      NPI$closures[i]<-NPI$type[i]
    }
    else if(NPI$Action[i]=="Eased"){
      NPI$openings[i]<-NPI$type[i]
    }
  }
  return(NPI)
}

df_QC<-read.csv(sprintf('./data_output/df_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,y,lower,upper) 
names(df_QC)<- c("date","I_actual_QC","I_lower_QC","I_upper_QC")
dfr_QC<-read.csv(sprintf('./data_output/dfr_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,R,lower,upper) 
names(dfr_QC)<- c("date","R_actual_QC","R_lower_QC","R_upper_QC")
df_ci_both<-merge(df_QC,dfr_QC,by="date")
df_ci_both$date<-as.Date(df_ci_both$date)-floor(window_size/2)


NPI_QC<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Que.")%>%dplyr::select(date,I_actual_QC,R_actual_QC,R_upper_QC,R_lower_QC,closures,openings)


plot_date<-c("2020-04-01","2021-12-01")

plot_closures_qc<-ggplot(NPI_QC,aes(x=date))+
  geom_line(aes(y=I_actual_QC),cex=0.5)+
  geom_point(aes(y=I_actual_QC,colour = closures))+
  scale_fill_discrete(na.translate=FALSE)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab("Cases")+
  scale_x_date(breaks = "2 months", date_labels = "%b/%y",limits = as.Date(plot_date))+
  annotation_custom(grobTree(textGrob(sprintf("Closures"), x=0.05,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

plot_openings_qc<-ggplot(data=NPI_QC,aes(x=date))+
  geom_line(aes(y=I_actual_QC),cex=0.5)+
  geom_point(aes(y=I_actual_QC,colour = openings))+
  scale_fill_discrete(na.translate=FALSE)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab("Cases")+
  scale_x_date(breaks = "2 months", date_labels = "%b/%y",limits = as.Date(plot_date))+
  annotation_custom(grobTree(textGrob("Openings", x=0.05,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))



cases_rt_npi_for_qc<-ggarrange(plot_closures_qc,plot_openings_qc,nrow=2,labels=c("A","B"))
cases_rt_npi_for_qc

ggsave("../LatexImages/cases_rt_npi_for_qc.png",cases_rt_npi_for_qc,scale=1)
knitr::plot_crop("../LatexImages/cases_rt_npi_for_qc.png")
######


#define functions
############
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
    Iv[i]<-Rvec[i]*sum(Iv[i-svec]*w[svec])
  }
  return(Iv)
}

likelihood_loss.1<-function(par,case,rmean,rstd,plt=F){
  if((par[1]<1)|(par[2]>20)){
    return(-1e10)
  }
  if(par[2]<0.1){
    return(-1e10)
  }
  mean_w<-exp(par[1])+1
  std_w<-exp(par[2])
  if(plt){print(c(mean_w,std_w))}
  muvec<-find_poisson_case(case,mean_w,std_w,rmean,rstd)
  llike<-sum(dpois(case[-c(1:14)],muvec[-c(1:14)],log=T))
  if(plt){points(mean_w,std_w,col='blue',pch=19,cex=0.75)}
  return(llike)
}

df_ci_both


prior.m=10
prior.s=10


list_mean_w<-list()
list_std_w<-list()

opo<-optim(par=c(log(7-1),log(0.5)),likelihood_loss.1,case=df_ci_both$I_actual_QC,
           rmean=prior.m,rstd=prior.s,plt=FALSE,control = list(maxit=500,fnscale=-1))

list_mean_w[1]=exp(opo$par[1])+1
list_std_w[1]=exp(opo$par[2])

Iv<-df_ci_both$I_actual_QC
tryBaseline<-find_poisson_case(case=Iv,
                               mean_w=list_mean_w[[1]],
                               std_w=list_std_w[[1]],
                               rmean=prior.m,
                               rstd=prior.s)

# this is mixed with state space model 
df<-data.frame(actual=Iv,
               date=df_ci_both$date,
               first_fit_simulation=c(tryBaseline,rep(25,100))[1:626])

ggplot(data=df,aes(x=date))+
  geom_point(aes(y=Iv),col="red")+geom_point(aes(y=first_fit_simulation))




#get CI for R
list_w<-list()
res_i <- estimate_R(df_ci_both$I_actual_QC, method = "parametric_si",
                    config = make_config(list(mean_si = log(list_mean_w[[1]]-1),std_si =(list_std_w[[1]]),
                                              mean_prior=prior.m,std_prior=prior.s)))


ith_R<-res_i$R$`Mean(R)`
NonNAindex <- which(!is.na(ith_R))
firstNonNA <- min(NonNAindex)
for(k in 1:firstNonNA){ith_R[k]<-ith_R[firstNonNA]}
list_w[1]<-list(res_i$si_distr)
############


# The progression of Rt in the absence of intervention
##################
NPI_QC%>%filter(closures=="Curfew")
change_point=as.Date('2021-01-09')-min(df_ci_both$date)+1
case_without_intervention<-function(R_val,I,change_point){
  for(i in change_point:length(I)){
    R_val[i]<-R_val[change_point]
    I[i]<-0}
  
    w<-list_w[[1]]
    for (i in change_point:length(I)) {
      svec<-c(1:(i-1))
      svec<-svec[w[svec]>0]
      I[i]<-R_val[i]*sum(I[i-svec]*w[svec])
    }
  return(list(R_val,I))
}


df_ci_both$R_l_QC<-(df_ci_both$R_actual_QC+df_ci_both$R_lower_QC)/2
df_ci_both$R_u_QC<-(df_ci_both$R_actual_QC+df_ci_both$R_upper_QC)/2
# ggplot(data=df_ci_both,aes(x=date))+geom_point(aes(y=actual))+geom_point(aes(y=new_r))
df_ci_both$new_R <- case_without_intervention(R_val=df_ci_both$R_actual_QC,I=df_ci_both$I_actual_QC,change_point=change_point-1)[[1]]
df_ci_both$new_I <- case_without_intervention(R_val=df_ci_both$R_actual_QC,I=df_ci_both$I_actual_QC,change_point=change_point-1)[[2]]


df_ci_both$new_I_lower<- case_without_intervention(R_val=df_ci_both$R_actual_QC,I=df_ci_both$I_lower_QC,change_point=change_point-1)[[2]]
df_ci_both$new_I_upper<- case_without_intervention(R_val=df_ci_both$R_actual_QC,I=df_ci_both$I_upper_QC,change_point=change_point-1)[[2]]

g2<-ggplot(data=df_ci_both,aes(x=date))+
  ylab("")+
  ylim(0,25000)+
  geom_line(aes(y=new_I,col="forecast"),cex=0.5)+ 
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        plot.margin = margin(0,0,0,0, "cm"),
        axis.title.x=element_blank(),legend.position = "none")+
  geom_line(aes(y=I_actual_QC),cex=0.5)+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-10-01","2021-07-01")))+
  geom_ribbon(aes(ymin=new_I_lower,ymax=new_I_upper),alpha=0.1, linetype="dashed",color="grey")
g2


fig_change_point<-ggarrange(g1,g2,nrow=2,labels=c("A","B"))
fig_change_point


ggsave("../LatexImages/fig_change_point.png",fig_change_point,scale=1)
knitr::plot_crop("../LatexImages/fig_change_point.png")

##################













