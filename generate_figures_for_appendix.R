#Install dependencies
######
library(lubridate)
library(plyr)
library(dplyr)
library(EpiEstim)
library(tidyverse)
library(zoo)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(readxl)
library(MASS)
library(dplyr)
library(ggrepel)
library(grid)
library(magick)
######



#define function and generate plot for A.1
########
one_prov_all_ws <- function(province,pr,date_start,date_end,ylim_val){
  dfr<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/useful/dfr_output_2000_%s_dlm_ws1.csv',pr)) %>% dplyr::select(date,R,lower,upper) 
  names(dfr)<- c("date","R_actual","R_lower","R_upper")
  dfr$date<-as.Date(dfr$date)
  
  dfr5<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/useful/dfr_output_2000_%s_dlm_ws5.csv',pr)) %>% dplyr::select(date,R,lower,upper) 
  names(dfr5)<- c("date","R_actual_5","R_lower_5","R_upper_5")
  dfr5$date<-as.Date(dfr5$date)-3
  
  dfr14<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/useful/dfr_output_2000_%s_dlm_ws14.csv',pr))  %>% dplyr::select(date,R,lower,upper)
  names(dfr14)<- c("date","R_actual_14","R_lower_14","R_upper_14")
  dfr14$date<-as.Date(dfr14$date)-7
  
  dfr7<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/useful/dfr_output_2000_%s_dlm_ws7.csv',pr))  %>% dplyr::select(date,R,lower,upper) 
  names(dfr7)<- c("date","R_actual_7","R_lower_7","R_upper_7")
  dfr7$date<-as.Date(dfr7$date)-3
  
  dfr_ws <- merge(dfr5,dfr,by="date")
  dfr_ws <- merge(dfr_ws,dfr14,by="date")
  dfr_ws <- merge(dfr_ws,dfr7,by="date")
  
  grob <- grobTree(textGrob(sprintf("%s",province), x=0.1,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12)))
  
  plot<-ggplot(dfr_ws,aes(x=date))+
    geom_line(aes(y=R_actual_14,col="14"))+
    geom_line(aes(y=R_actual_7,col="7"),col="lightpink1")+
    geom_line(aes(y=R_actual,col="1"),col="lightblue")+
    geom_line(aes(y=R_actual_5,col="5"),col="lightgreen")+
    geom_ribbon(aes(ymin=R_lower_5,ymax=R_upper_5),col="lightgreen",alpha=0.1, linetype="dashed")+
    geom_ribbon(aes(ymin=R_lower,ymax=R_upper),col="lightblue",alpha=0.1, linetype="dashed")+
    geom_ribbon(aes(ymin=R_lower_7,ymax=R_upper_7),col="lightpink1",alpha=0.1, linetype="dashed")+
    geom_ribbon(aes(ymin=R_lower_14,ymax=R_upper_14),col="grey",alpha=0.1, linetype="dashed")+
    scale_color_manual(values = c("14"="grey","7"="lightpink1","5"="lightgreen", "1"="lightblue"))+  
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),legend.position = c(0.9, 0.8),
          legend.title = element_text(size = 10),legend.text = element_text(size = 10),
          legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
    scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c(date_start,date_end)))+
    labs(color="Window Size")+ylab(expression("R"[t]))+
    ylim(0,ylim_val)+
    annotation_custom(grob)
  return(plot)
}

province="Ontario"
pr="ON"

ON_all_ws<-one_prov_all_ws(province="Ontario",pr="ON",date_start="2020-09-01",date_end="2021-07-01",ylim_val=4.3)
QC_all_ws<-one_prov_all_ws(province="Québec",pr="QC",date_start="2020-09-01",date_end="2021-07-01",ylim_val=4.3)

both_prov_all_ws<-ggarrange(ON_all_ws,QC_all_ws,nrow=2,ncol=1)
ggsave("../LatexImages/all_ws_two_prov.png",both_prov_all_ws,scale=1)

#zoomed version 
ON_all_ws_zoomed<-one_prov_all_ws(province="Ontario",pr="ON",date_start="2020-12-01",date_end="2021-06-01",ylim_val=3)
QC_all_ws_zoomed<-one_prov_all_ws(province="Québec",pr="QC",date_start="2020-12-01",date_end="2021-06-01",ylim_val=3)

both_prov_all_ws_zoomed<-ggarrange(ON_all_ws_zoomed,QC_all_ws_zoomed,nrow=2,ncol=1)
ggsave("../LatexImages/all_ws_two_prov_zoomed.png",both_prov_all_ws_zoomed,scale=1)
########



#define function and generate plot
#df_ci_both
##########
one_prov_all_ws_df <- function(province,pr,date_start,date_end,ylim_val){
  dfr<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/useful/dfr_output_2000_%s_dlm_ws1.csv',pr)) %>% dplyr::select(date,R,lower,upper) 
  names(dfr)<- c("date","R_actual","R_lower","R_upper")
  dfr$date<-as.Date(dfr$date)
  
  dfr5<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/useful/dfr_output_2000_%s_dlm_ws5.csv',pr)) %>% dplyr::select(date,R,lower,upper) 
  names(dfr5)<- c("date","R_actual_5","R_lower_5","R_upper_5")
  dfr5$date<-as.Date(dfr5$date)-3
  
  dfr14<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/useful/dfr_output_2000_%s_dlm_ws14.csv',pr))  %>% dplyr::select(date,R,lower,upper)
  names(dfr14)<- c("date","R_actual_14","R_lower_14","R_upper_14")
  dfr14$date<-as.Date(dfr14$date)-7
  
  dfr7<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/useful/dfr_output_2000_%s_dlm_ws7.csv',pr))  %>% dplyr::select(date,R,lower,upper) 
  names(dfr7)<- c("date","R_actual_7","R_lower_7","R_upper_7")
  dfr7$date<-as.Date(dfr7$date)-3
  
  dfr_ws <- merge(dfr5,dfr,by="date")
  dfr_ws <- merge(dfr_ws,dfr14,by="date")
  dfr_ws <- merge(dfr_ws,dfr7,by="date")
  return(dfr_ws)}

ON_all_ws<-one_prov_all_ws_df(province="Ontario",pr="ON") %>%dplyr::select(date,R_actual_7,R_lower_7,R_upper_7)
names(ON_all_ws)<-c("date","R_actual_ON","R_lower_ON","R_upper_ON")
QC_all_ws<-one_prov_all_ws_df(province="Québec",pr="QC")%>%dplyr::select(date,R_actual_7,R_lower_7,R_upper_7)
names(QC_all_ws)<-c("date","R_actual_QC","R_lower_QC","R_upper_QC")
df_ci_both<-merge(ON_all_ws,QC_all_ws,by="date")
df_ci_both
##########

#df_ci_both
############
window_size=7
df_ON<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/data_output/df_output_2000_ON_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,y,lower,upper) 
names(df_ON)<- c("date","I_actual_ON","I_lower_ON","I_upper_ON")
df_QC<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/data_output/df_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,y,lower,upper) 
names(df_QC)<- c("date","I_actual_QC","I_lower_QC","I_upper_QC")
dfr_ON<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/data_output/dfr_output_2000_ON_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,R,lower,upper) 
dfr_QC<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/data_output/dfr_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,R,lower,upper) 
names(dfr_ON)<- c("date","R_actual_ON","R_lower_ON","R_upper_ON")
names(dfr_QC)<- c("date","R_actual_QC","R_lower_QC","R_upper_QC")
total_df <- merge(df_ON,df_QC,by="date")
total_dfr <- merge(dfr_ON,dfr_QC,by="date")
total<-merge(total_df,total_dfr,by="date")
df_ci_both<-total
df_ci_both$date<-as.Date(df_ci_both$date)-floor(window_size/2)
df_ci_both
############




#chunk C
#npi_both_zoomed.png
############
assign_npi_type <- function(df_ci_both,jurisdiction_val){
  s3 <- read_excel("/Users/oliviashi/Library/Containers/com.microsoft.Excel/Data/Downloads/aoda-covid-19-intervention-timeline-in-canada-en.xlsx",sheet=3,col_names=TRUE,skip=2)
  s3$date <- as.Date(s3$`Start date`,format="%d-%m-%Y", tryFormats= "%Y-%m-%d")
  s3$cat <- s3$`Intervention Category`
  s3 <- s3 %>% dplyr::select("Entry ID","date","cat","Action","Jurisdiction","Description") %>% filter(Jurisdiction==jurisdiction_val)#Que.#Ont.
  s3 <- tibble(s3)
  da <- join(df_ci_both,s3,by="date",type="left",match="first")
  
  NPI <- da %>% dplyr::select(date,cat,Action,Description,R_actual_QC,R_actual_ON,R_lower_QC,R_upper_QC,R_upper_ON,R_lower_ON)
  NPI <- pivot_wider(data=NPI,names_from=cat,values_from=Description) %>% dplyr::select(date,Action,R_actual_QC,R_actual_ON,R_lower_QC,R_upper_QC,R_upper_ON,R_lower_ON,`Closures/openings`,Distancing)
  
  
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


NPI_QC<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Que.")
NPI_ON<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Ont.")

province="Québec"
plot_closures_qc<-ggplot(NPI_QC,aes(x=date))+
  geom_line(aes(y=R_actual_QC))+
  geom_point(aes(y=R_actual_QC,colour = closures))+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob(sprintf("Closures for %s",province), x=0.1,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

plot_openings_qc<-ggplot(NPI_QC,aes(x=date))+
  geom_line(aes(y=R_actual_QC))+
  geom_point(aes(y=R_actual_QC,colour = openings))+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob(sprintf("Openings for %s",province), x=0.1,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

province="Ontario"
plot_closures_on<-ggplot(NPI_ON,aes(x=date))+
  geom_line(aes(y=R_actual_ON))+
  geom_point(aes(y=R_actual_ON,colour = closures))+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob(sprintf("Closures for %s",province), x=0.1,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

plot_openings_on<-ggplot(NPI_ON,aes(x=date))+
  geom_line(aes(y=R_actual_ON))+
  geom_point(aes(y=R_actual_ON,colour = openings))+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob(sprintf("Openings for %s",province), x=0.1,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

npi_both_zoomed<-ggarrange(plot_closures_qc,plot_closures_on,plot_openings_qc,plot_openings_on,nrow=4,ncol=1)
ggsave("../LatexImages/npi_both_zoomed.png",npi_both_zoomed,scale=1)
############



#npi_both_zoomed_ci.png
############
assign_npi_type <- function(df_ci_both,jurisdiction_val){
  s3 <- read_excel("/Users/oliviashi/Library/Containers/com.microsoft.Excel/Data/Downloads/aoda-covid-19-intervention-timeline-in-canada-en.xlsx",sheet=3,col_names=TRUE,skip=2)
  s3$date <- as.Date(s3$`Start date`,format="%d-%m-%Y", tryFormats= "%Y-%m-%d")
  s3$cat <- s3$`Intervention Category`
  s3 <- s3 %>% dplyr::select("Entry ID","date","cat","Action","Jurisdiction","Description") %>% filter(Jurisdiction==jurisdiction_val)#Que.#Ont.
  s3 <- tibble(s3)
  da <- join(df_ci_both,s3,by="date",type="left",match="first")
  
  NPI <- da %>% dplyr::select(date,cat,Action,Description,R_actual_QC,R_actual_ON,R_lower_QC,R_upper_QC,R_upper_ON,R_lower_ON)
  NPI <- pivot_wider(data=NPI,names_from=cat,values_from=Description) %>% dplyr::select(date,Action,R_actual_QC,R_actual_ON,R_lower_QC,R_upper_QC,R_upper_ON,R_lower_ON,`Closures/openings`,Distancing)
  
  
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


NPI_QC<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Que.")
NPI_ON<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Ont.")

province="Québec"
plot_closures_qc<-ggplot(NPI_QC,aes(x=date))+
  geom_line(aes(y=R_actual_QC))+
  geom_point(aes(y=R_actual_QC,colour = closures))+
  geom_ribbon(aes(ymin=R_lower_QC,ymax=R_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob(sprintf("Closures for %s",province), x=0.1,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

plot_openings_qc<-ggplot(NPI_QC,aes(x=date))+
  geom_line(aes(y=R_actual_QC))+
  geom_point(aes(y=R_actual_QC,colour = openings))+
  geom_ribbon(aes(ymin=R_lower_QC,ymax=R_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob(sprintf("Openings for %s",province), x=0.1,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

province="Ontario"
plot_closures_on<-ggplot(NPI_ON,aes(x=date))+
  geom_line(aes(y=R_actual_ON))+
  geom_point(aes(y=R_actual_ON,colour = closures))+
  geom_ribbon(aes(ymin=R_lower_ON,ymax=R_upper_ON),col="lightpink1",alpha=0.1, linetype="dashed")+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob(sprintf("Closures for %s",province), x=0.1,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

plot_openings_on<-ggplot(NPI_ON,aes(x=date))+
  geom_line(aes(y=R_actual_ON))+
  geom_point(aes(y=R_actual_ON,colour = openings))+
  geom_ribbon(aes(ymin=R_lower_ON,ymax=R_upper_ON),col="lightpink1",alpha=0.1, linetype="dashed")+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob(sprintf("Openings for %s",province), x=0.1,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

npi_both_zoomed_ci<-ggarrange(plot_closures_qc,plot_closures_on,plot_openings_qc,plot_openings_on,nrow=4,ncol=1)
ggsave("../LatexImages/npi_both_zoomed_ci.png",npi_both_zoomed_ci,scale=1)
############




#chunk D
#npi_both_zoomed_in_one_figure.png
############
NPI_QC<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Que.")%>%dplyr::select(date,R_actual_QC,closures,openings)
NPI_ON<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Ont.")%>%dplyr::select(date,R_actual_ON,closures,openings)
names(NPI_QC)<-c("date","R_actual_QC","closures_QC","openings_QC")
names(NPI_ON)<-c("date","R_actual_ON","closures_ON","openings_ON")
NPI_both<-merge(NPI_QC,NPI_ON,by="date")


plot_closures<-ggplot(NPI_both,aes(x=date))+
  geom_point(aes(y=R_actual_QC),col="lightblue",cex=0.5)+
  geom_point(aes(y=R_actual_ON),col="lightpink1",cex=0.5)+
  geom_point(aes(y=R_actual_QC,colour = closures_QC))+
  geom_point(aes(y=R_actual_ON,colour = closures_ON))+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  ylim(0.2,2)+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob("Closures for Ontario (red) and Québec (blue)", x=0.05,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))

plot_openings<-ggplot(NPI_both,aes(x=date))+
  geom_point(aes(y=R_actual_QC),col="lightblue",cex=0.5)+
  geom_point(aes(y=R_actual_ON),col="lightpink1",cex=0.5)+
  geom_point(aes(y=R_actual_QC,colour = openings_QC))+
  geom_point(aes(y=R_actual_ON,colour = openings_ON))+
  scale_fill_discrete(na.translate=FALSE)+ylim(0,2.5)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  ylim(0.2,2)+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob("Openings for Ontario (red) and Québec (blue)", x=0.05,  y=0.9, hjust=0,gp=gpar(col="black", fontsize=12))))
plot_openings


rt_w_int_on_qc<-ggarrange(plot_closures,plot_openings,nrow=2)

ggsave("../LatexImages/rt_w_int_on_qc.png",rt_w_int_on_qc)
knitr::plot_crop("../LatexImages/rt_w_int_on_qc.png")

############





#rt_ci_w_int_on_qc with confidence interval
#####
NPI_QC<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Que.")%>%dplyr::select(date,R_actual_QC,R_upper_QC,R_lower_QC,closures,openings)
NPI_ON<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Ont.")%>%dplyr::select(date,R_actual_ON,R_upper_ON,R_lower_ON,closures,openings)
names(NPI_QC)<-c("date","R_actual_QC","R_upper_QC","R_lower_QC","closures_QC","openings_QC")
names(NPI_ON)<-c("date","R_actual_ON","R_upper_ON","R_lower_ON","closures_ON","openings_ON")
NPI_both<-merge(NPI_QC,NPI_ON,by="date")


plot_closures<-ggplot(NPI_both,aes(x=date))+
  geom_point(aes(y=R_actual_QC),col="lightblue",cex=0.5)+
  geom_point(aes(y=R_actual_ON),col="lightpink1",cex=0.5)+
  geom_point(aes(y=R_actual_QC,colour = closures_QC))+
  geom_point(aes(y=R_actual_ON,colour = closures_ON))+
  geom_ribbon(aes(ymin=R_lower_QC,ymax=R_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  geom_ribbon(aes(ymin=R_lower_ON,ymax=R_upper_ON),col="lightpink1",alpha=0.1, linetype="dashed")+
  scale_fill_discrete(na.translate=F)+ylim(0.25,2.25)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),
        legend.position = c(0.9, 0.7),legend.title = element_blank(),
        legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),
        legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob("Closures for Ontario (red) and Québec (blue)", x=0.05,  y=0.85, hjust=0,gp=gpar(col="black", fontsize=12))))

plot_openings<-ggplot(NPI_both,aes(x=date))+
  geom_point(aes(y=R_actual_QC),col="lightblue",cex=0.5)+
  geom_point(aes(y=R_actual_ON),col="lightpink1",cex=0.5)+
  geom_point(aes(y=R_actual_QC,colour = openings_QC))+
  geom_point(aes(y=R_actual_ON,colour = openings_ON))+
  geom_ribbon(aes(ymin=R_lower_QC,ymax=R_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  geom_ribbon(aes(ymin=R_lower_ON,ymax=R_upper_ON),col="lightpink1",alpha=0.1, linetype="dashed")+
  scale_fill_discrete(na.translate=F)+ylim(0.25,2.25)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),
        legend.position = c(0.9, 0.7),legend.title = element_blank(),
        legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),
        legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  annotation_custom(grobTree(textGrob("Openings for Ontario (red) and Québec (blue)", x=0.05,  y=0.85, hjust=0,gp=gpar(col="black", fontsize=12))))
plot_openings

rt_ci_w_int_on_qc<-ggarrange(plot_closures,plot_openings,align="v",nrow=2)

ggsave("../LatexImages/rt_ci_w_int_on_qc.png",rt_ci_w_int_on_qc,scale=1)
knitr::plot_crop("../LatexImages/rt_ci_w_int_on_qc.png")
#####




# fig_ci_qc_cases_rt
###############
window_size=7
df_QC<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/data_output/df_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,y,lower,upper) 
names(df_QC)<- c("date","I_actual_QC","I_lower_QC","I_upper_QC")
dfr_QC<-read.csv(sprintf('/Users/oliviashi/Documents/MASTER_RESEARCH/data_output/dfr_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,R,lower,upper) 
names(dfr_QC)<- c("date","R_actual_QC","R_lower_QC","R_upper_QC")
df_ci_both<-merge(df_QC,dfr_QC,by="date")
df_ci_both$date<-as.Date(df_ci_both$date)-floor(window_size/2)

date_start='2020-04-01'
date_end='2021-11-01'
fig_ci_cases<-ggplot(df_ci_both,aes(x=date))+
  geom_point(aes(y=I_actual_QC),col="blue",cex=0.5)+
  geom_ribbon(aes(ymin=I_lower_QC,ymax=I_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),legend.position = c(0.9, 0.8),
        legend.title = element_text(size = 10),legend.text = element_text(size = 10),
        legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_x_date(breaks = "2 months", date_labels = "%b/%y",limits = as.Date(c(date_start,date_end)))+ylab("cases")+
  ylim(0,4300)

fig_ci_rt<-ggplot(df_ci_both,aes(x=date))+
  geom_line(aes(y=R_actual_QC),col="blue",cex=0.5)+
  geom_ribbon(aes(ymin=R_lower_QC,ymax=R_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),legend.position = c(0.9, 0.8),
        legend.title = element_text(size = 10),legend.text = element_text(size = 10),
        legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  scale_x_date(breaks = "2 months", date_labels = "%b/%y",limits = as.Date(c(date_start,date_end)))+ylab(expression(R[t]))+
  ylim(0.3,2.6)
fig_ci_rt

fig_ci_qc_cases_rt<-ggarrange(fig_ci_cases,fig_ci_rt,nrow=2,align="v",labels=c("A","B"))
ggsave("../LatexImages/fig_ci_qc_cases_rt.png",fig_ci_qc_cases_rt,scale=1)
###############



#GOOD fig_cases_on_qc.png, fig_cpp_cases_on_qc.png
###############
population_ON=14789778
population_QC=8604500
df_ci_both$cases_pc_ON<-(df_ci_both$I_actual_ON*100000/population_ON)
df_ci_both$cases_pc_QC<-(df_ci_both$I_actual_QC*100000/population_QC)
df_ci_both$cpp_I_lower_QC<-(df_ci_both$I_lower_QC*100000/population_QC)
df_ci_both$cpp_I_upper_QC<-(df_ci_both$I_upper_QC*100000/population_QC)
df_ci_both$cpp_I_lower_ON<-(df_ci_both$I_lower_ON*100000/population_ON)
df_ci_both$cpp_I_upper_ON<-(df_ci_both$I_upper_ON*100000/population_ON)

fig_cases_on_qc<-ggplot(df_ci_both,aes(x=date))+
  geom_point(aes(y=I_actual_QC,col="QC"),cex=0.5)+
  geom_point(aes(y=I_actual_ON,col="ON"),cex=0.5)+
  geom_ribbon(aes(ymin=I_lower_QC,ymax=I_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  geom_ribbon(aes(ymin=I_lower_ON,ymax=I_upper_ON),col="lightpink1",alpha=0.1, linetype="dashed")+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-09-01","2021-07-01")))+
  scale_color_manual(values = c("ON"="red","QC"="blue")) +  
  theme(aspect.ratio=0.3,legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  ylim(0,6000)+ylab("cases")

ggsave("../LatexImages/fig_cases_on_qc.png",fig_cases_on_qc)
knitr::plot_crop("../LatexImages/fig_cases_on_qc.png")


fig_cpp_cases_on_qc<-ggplot(data=df_ci_both,aes(x=date))+
  geom_point(aes(y=cases_pc_ON,color="ON"),cex=0.5)+
  geom_point(aes(y=cases_pc_QC,color="QC"),cex=0.5)+
  geom_ribbon(aes(ymin=cpp_I_lower_QC,ymax=cpp_I_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  geom_ribbon(aes(ymin=cpp_I_lower_ON,ymax=cpp_I_upper_ON),col="lightpink1",alpha=0.1, linetype="dashed")+
  theme(aspect.ratio=0.3,legend.background=element_rect(fill = alpha("white", 0.5)),legend.position = c(0.9, 0.8),legend.title = element_blank(),legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  ylab("adjusted cases")+
  scale_color_manual(values = c("ON"="red","QC"="blue"))+
  scale_x_date(breaks = "2 months", date_labels = "%b %y",limits = as.Date(c("2020-09-01","2021-07-01")))

ggsave("../LatexImages/fig_cpp_cases_on_qc.png",fig_cpp_cases_on_qc)
knitr::plot_crop("../LatexImages/fig_cpp_cases_on_qc.png")

###############


#GOOD fig_ci_rt_on_qc
#################
fig_ci_rt_on_qc<-ggplot(df_ci_both,aes(x=date))+
  geom_line(aes(y=R_actual_QC,color="QC"),cex=0.5)+
  geom_line(aes(y=R_actual_ON,,color="ON"),cex=0.5)+
  geom_ribbon(aes(ymin=R_lower_QC,ymax=R_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  geom_ribbon(aes(ymin=R_lower_ON,ymax=R_upper_ON),col="lightpink1",alpha=0.1, linetype="dashed")+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-10-01","2021-07-01")))+
  scale_color_manual(values = c("ON"="red","QC"="blue")) +  
  theme(aspect.ratio=0.3,legend.background=element_rect(fill = alpha("white", 0.5)),
        legend.position = c(0.9, 0.8),legend.title = element_blank(),
        legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),legend.key = element_blank(),axis.title.x=element_blank())+
  ylim(0.3,2.6)+ylab(expression(R[t]))

ggsave("../LatexImages/fig_ci_rt_on_qc.png",fig_ci_rt_on_qc)
knitr::plot_crop("../LatexImages/fig_ci_rt_on_qc.png")

#################



#cpp
###############

#per capita, with CI, only ON
ci_on_cases <- ggplot(data=df_ci_both,aes(x=date),cex=0.1)+
  geom_point(aes(y=I_actual_ON),cex=0.5,color="red")+
  geom_ribbon(aes(ymin=I_lower_ON,ymax=I_upper_ON),col="lightpink1",alpha=0.1, linetype="dashed")+
  scale_x_date(breaks = "2 months", date_labels = "%b %y",limits = as.Date(c("2020-10-01","2021-07-01")))+
  ylab("cases")+theme(axis.title.x=element_blank())

ci_qc_cases <- ggplot(data=df_ci_both,aes(x=date),cex=0.1)+
  geom_point(aes(y=I_actual_QC),cex=0.5,color="blue")+  
  geom_ribbon(aes(ymin=I_lower_QC,ymax=I_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-10-01","2021-07-01")))+
  ylab("cases")+theme(axis.title.x=element_blank())

#graph 1.2
# ggarrange(fig_cpp_ON,fig_cpp_QC,nrow=2)
ci_both_cases<-ggarrange(ci_on_cases,ci_qc_cases,nrow=2,labels=c("A","B"))

ggsave("../LatexImages/ci_both_cases.png",ci_both_cases)
knitr::plot_crop("../LatexImages/ci_both_cases.png")



#GOOD figure cases and NPI



###############################################################



###############################################################
