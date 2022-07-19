########
assign_npi_type <- function(df_ci_both,jurisdiction_val){
  s3 <- read_excel("./covid-19-intervention-timeline-in-canada-en.xlsx",sheet=3,col_names=TRUE,skip=2)
  s3$date <- as.Date(s3$`Start date`,format="%d-%m-%Y", tryFormats= "%Y-%m-%d")
  s3$cat <- s3$`Intervention Category`
  s3 <- s3 %>% dplyr::select("Entry ID","date","cat","Action","Jurisdiction","Description") %>% filter(Jurisdiction==jurisdiction_val)#Que.#Ont.
  s3 <- tibble(s3)
  da <- join(df_ci_both,s3,by="date",type="left",match="first")
  
  NPI <- da %>% dplyr::select(date,cat,Action,Description,R_actual_QC,R_upper_QC,R_lower_QC,I_actual_QC,I_upper_QC,I_lower_QC,R_actual_ON,R_upper_ON,R_lower_ON,I_actual_ON,I_upper_ON,I_lower_ON)
  #,R_actual_ON,R_upper_ON,R_lower_ON,I_actual_ON,I_upper_ON,I_lower_ON)
  
  NPI <- pivot_wider(data=NPI,names_from=cat,values_from=Description) %>% dplyr::select(date,Action,R_actual_QC,R_upper_QC,R_lower_QC,I_actual_QC,I_upper_QC,I_lower_QC,R_actual_ON,R_upper_ON,R_lower_ON,I_actual_ON,I_upper_ON,I_lower_ON,`Closures/openings`,Distancing)
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

########


########
window_size=7
df_ON<-read.csv(sprintf('./data_output/df_output_2000_ON_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,y,lower,upper) 
names(df_ON)<- c("date","I_actual_ON","I_lower_ON","I_upper_ON")
df_QC<-read.csv(sprintf('./data_output/df_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,y,lower,upper) 
names(df_QC)<- c("date","I_actual_QC","I_lower_QC","I_upper_QC")
dfr_ON<-read.csv(sprintf('./data_output/dfr_output_2000_ON_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,R,lower,upper) 
dfr_QC<-read.csv(sprintf('./data_output/dfr_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,R,lower,upper) 
names(dfr_ON)<- c("date","R_actual_ON","R_lower_ON","R_upper_ON")
names(dfr_QC)<- c("date","R_actual_QC","R_lower_QC","R_upper_QC")
total_df <- merge(df_ON,df_QC,by="date")
total_dfr <- merge(dfr_ON,dfr_QC,by="date")
df_ci_both<-merge(total_df,total_dfr,by="date")
df_ci_both$date<-as.Date(df_ci_both$date)-floor(window_size/2)
df_ci_both

NPI_QC<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Que.")%>%dplyr::select(date,R_actual_QC,R_upper_QC,R_lower_QC,closures,openings)
NPI_ON<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Ont.")%>%dplyr::select(date,R_actual_ON,R_upper_ON,R_lower_ON,closures,openings)
names(NPI_QC)<-c("date","R_actual_QC","R_upper_QC","R_lower_QC","closures_QC","openings_QC")
names(NPI_ON)<-c("date","R_actual_ON","R_upper_ON","R_lower_ON","closures_ON","openings_ON")
NPI_both<-merge(NPI_QC,NPI_ON,by="date") %>% filter(date>'2020-12-01')


lines <- data.frame(vlines = subset(NPI_both, !is.na(openings_QC))$date, labels = c("1", "2","3","4","5","6","7"), stringsAsFactors = FALSE)
lines_closed <- data.frame(vlines = subset(NPI_both, !is.na(closures_QC))$date, labels = c("1", "2","3","4","5","6","7"), stringsAsFactors = FALSE)


annotated_plot_qc_npi<-ggplot(NPI_both,aes(x=date))+
  geom_point(aes(y=R_actual_QC),col="lightblue",cex=0.5)+
  geom_point(data=(NPI_both %>% filter(!is.na(closures_QC))),aes(y=R_actual_QC,shape = closures_QC,na.rm=TRUE),na.rm=TRUE)+
  geom_point(data=(NPI_both %>% filter(!is.na(openings_QC))),aes(y=R_actual_QC,shape = openings_QC,na.rm=TRUE),na.rm=TRUE)+
  geom_ribbon(aes(ymin=R_lower_QC,ymax=R_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  scale_fill_discrete(na.translate=F)+ylim(0.25,2.25)+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),
        legend.position = c(0.9, 0.7),legend.title = element_blank(),
        legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),
        legend.key = element_blank(),axis.title.x=element_blank())+
  scale_colour_discrete(na.translate = F)+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "2 months", date_labels = "%b %Y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  geom_vline(data = subset(NPI_both, !is.na(closures_QC)), # filter data source
             aes(xintercept = date),
             size = 0.2, colour = "darkgreen")+
  geom_vline(data = subset(NPI_both, !is.na(openings_QC)), # filter data source
             aes(xintercept = date),
             size = 0.2, colour = "orange")+
geom_text(data = lines, aes(x = vlines, y = 0.5, label = labels))

annotated_plot_qc_npi



# ggsave("../LatexImages/rt_ci_w_int_on_qc.png",rt_ci_w_int_on_qc,scale=1)
# knitr::plot_crop("../LatexImages/rt_ci_w_int_on_qc.png")


########



#only curfew and school openings
########
assign_npi_type <- function(df_ci_both,jurisdiction_val){
  s3 <- read_excel("/Users/oliviashi/Library/Containers/com.microsoft.Excel/Data/Downloads/aoda-covid-19-intervention-timeline-in-canada-en.xlsx",sheet=3,col_names=TRUE,skip=2)
  s3$date <- as.Date(s3$`Start date`,format="%d-%m-%Y", tryFormats= "%Y-%m-%d")
  s3$cat <- s3$`Intervention Category`
  s3 <- s3 %>% dplyr::select("Entry ID","date","cat","Action","Jurisdiction","Description") %>% filter(Jurisdiction==jurisdiction_val)#Que.#Ont.
  s3 <- tibble(s3)
  da <- join(df_ci_both,s3,by="date",type="left",match="first")
  
  NPI <- da %>% dplyr::select(date,cat,Action,Description,R_actual_QC,R_upper_QC,R_lower_QC,I_actual_QC,I_upper_QC,I_lower_QC,R_actual_ON,R_upper_ON,R_lower_ON,I_actual_ON,I_upper_ON,I_lower_ON)

  NPI <- pivot_wider(data=NPI,names_from=cat,values_from=Description) %>% dplyr::select(date,Action,R_actual_QC,R_upper_QC,R_lower_QC,I_actual_QC,I_upper_QC,I_lower_QC,R_actual_ON,R_upper_ON,R_lower_ON,I_actual_ON,I_upper_ON,I_lower_ON,`Closures/openings`,Distancing)

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
      if(grepl('curfew',NPI$Distancing[i],ignore.case=T,fixed=F)){
        NPI$type[i]<-"Curfew"
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

########


########
window_size=7
df_ON<-read.csv(sprintf('./data_output/df_output_2000_ON_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,y,lower,upper) 
names(df_ON)<- c("date","I_actual_ON","I_lower_ON","I_upper_ON")
df_QC<-read.csv(sprintf('./data_output/df_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,y,lower,upper) 
names(df_QC)<- c("date","I_actual_QC","I_lower_QC","I_upper_QC")
dfr_ON<-read.csv(sprintf('./data_output/dfr_output_2000_ON_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,R,lower,upper) 
dfr_QC<-read.csv(sprintf('./data_output/dfr_output_2000_QC_dlm_ws%i.csv',window_size)) %>% dplyr::select(date,R,lower,upper) 
names(dfr_ON)<- c("date","R_actual_ON","R_lower_ON","R_upper_ON")
names(dfr_QC)<- c("date","R_actual_QC","R_lower_QC","R_upper_QC")
total_df <- merge(df_ON,df_QC,by="date")
total_dfr <- merge(dfr_ON,dfr_QC,by="date")
df_ci_both<-merge(total_df,total_dfr,by="date")
df_ci_both$date<-as.Date(df_ci_both$date)-floor(window_size/2)
df_ci_both

NPI_QC<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Que.")%>%dplyr::select(date,R_actual_QC,R_upper_QC,R_lower_QC,closures,openings)
NPI_ON<-assign_npi_type(df_ci_both=df_ci_both,jurisdiction_val="Ont.")%>%dplyr::select(date,R_actual_ON,R_upper_ON,R_lower_ON,closures,openings)
names(NPI_QC)<-c("date","R_actual_QC","R_upper_QC","R_lower_QC","closures_QC","openings_QC")
names(NPI_ON)<-c("date","R_actual_ON","R_upper_ON","R_lower_ON","closures_ON","openings_ON")
NPI_both<-merge(NPI_QC,NPI_ON,by="date") %>% filter(date>'2020-12-01')


lines_open_qc <- data.frame(vlines = subset(NPI_both, !is.na(openings_QC))$date, labels = c("1", "2","3","4","5","6"), stringsAsFactors = FALSE)
lines_closed_qc <- data.frame(vlines = subset(NPI_both, !is.na(closures_QC))$date, labels = c("a", "b","c","d","e"), stringsAsFactors = FALSE)


annotated_plot_qc_npi<-ggplot(NPI_both,aes(x=date))+
  geom_line(aes(y=R_actual_QC),col="lightblue",cex=0.5)+
  geom_point(data=(NPI_both %>% filter(!is.na(closures_QC))),aes(y=R_actual_QC,shape = closures_QC),na.rm=TRUE)+
  geom_point(data=(NPI_both %>% filter(!is.na(openings_QC))),aes(y=R_actual_QC,shape = openings_QC),na.rm=TRUE)+
  geom_ribbon(aes(ymin=R_lower_QC,ymax=R_upper_QC),col="lightblue",alpha=0.1, linetype="dashed")+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),
        legend.position = c(0.9, 0.8),legend.title = element_blank(),
        legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),
        legend.key = element_blank(),axis.title.x=element_blank())+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "1 months", date_labels = "%b/%y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  geom_vline(data = subset(NPI_both, !is.na(closures_QC)), 
             aes(xintercept = date),
             size = 0.2, colour = "darkgreen")+
  geom_vline(data = subset(NPI_both, !is.na(openings_QC)), # filter data source
             aes(xintercept = date),
             size = 0.2, colour = "orange")+
  geom_text(data = lines_open_qc, aes(x = vlines, y = 0.5, label = labels))+
  geom_text(data = lines_closed_qc, aes(x = vlines, y = c(rep(0.3,4),0.4), label = labels))+
  ylim(0.25,2)

lines <- data.frame(vlines = subset(NPI_both, !is.na(openings_ON))$date, labels = c("1", "2","3"), stringsAsFactors = FALSE)
lines_closed <- data.frame(vlines = subset(NPI_both, !is.na(closures_ON))$date, labels = c("a", "b"), stringsAsFactors = FALSE)
lines_closed

annotated_plot_on_npi<-ggplot(NPI_both,aes(x=date))+
  geom_line(aes(y=R_actual_ON),col="pink",cex=0.5)+
  geom_point(data=(NPI_both %>% filter(!is.na(closures_ON))),aes(y=R_actual_ON,shape = closures_ON),na.rm=TRUE)+
  geom_point(data=(NPI_both %>% filter(!is.na(openings_ON))),aes(y=R_actual_ON,shape = openings_ON),na.rm=TRUE)+
  geom_ribbon(aes(ymin=R_lower_ON,ymax=R_upper_ON),col="pink",alpha=0.1, linetype="dashed")+
  theme(legend.background=element_rect(fill = alpha("white", 0.5)),
        legend.position = c(0.9, 0.8),legend.title = element_blank(),
        legend.text = element_text(size = 10),legend.key.size=unit(0.5,"lines"),
        legend.key = element_blank(),axis.title.x=element_blank())+
  ylab(expression("R"[t]))+
  scale_x_date(breaks = "1 months", date_labels = "%b/%y",limits = as.Date(c("2020-12-01","2021-06-01")))+
  geom_vline(data = subset(NPI_both, !is.na(closures_ON)), # filter data source
             aes(xintercept = date),
             size = 0.2, colour = "darkgreen")+
  geom_vline(data = subset(NPI_both, !is.na(openings_ON)), # filter data source
             aes(xintercept = date),
             size = 0.2, colour = "orange")+
  geom_text(data = lines, aes(x = vlines, y = 0.5, label = labels))+
  geom_text(data = lines_closed, aes(x = vlines, y = 0.3, label = labels))+ylim(0.25,2)

annotated_plot_npi_qc_on<-ggarrange(annotated_plot_qc_npi,annotated_plot_on_npi,nrow=2,labels=c("A","B"))
annotated_plot_npi_qc_on
ggsave("../LatexImages/annotated_plot_npi_qc_on.png",annotated_plot_npi_qc_on,scale=1)
knitr::plot_crop("../LatexImages/annotated_plot_npi_qc_on.png")


########
