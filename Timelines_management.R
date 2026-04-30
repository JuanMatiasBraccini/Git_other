#Read in Management timeline data
Management=read.csv(handl_OneDrive('Management/Sharks/Timeline management measures/Management_timeline.csv'))

#New timeline
fun.management.timeline=function(Management,drop.data.labels=TRUE,Ymax=NA)
{
  Start.managed=Management%>%filter(Label=='JASDGDLF comes into effect')%>%pull(StartDate)
  
  Management=Management%>%
    mutate(Use=case_when(Label==""~'No',
                         Label%in%c("Concern over mercury","Sale ban of large sharks")~'No',
                         grepl(paste(c('requiem sharks',' dusky'),collapse = '|'),Label)~'Yes',
                         .default=Use))%>%
    filter(Use=='Yes')%>%
    mutate(date=as.POSIXct(StartDate,format="%d/%m/%Y"))%>%
    dplyr::select(date,Label)
  
  
  if(drop.data.labels)
  {
    Management=Management%>%
      mutate(Label=case_when(Label=='Daily data reporting & VMS monitoring'~'VMS monitoring',
                             TRUE~Label))%>%
      filter(!grepl('data reporting',Label))
  }
  
  Management=Management%>%
    arrange(date)%>%
    mutate(id = -row_number(),
           id=rev(id),
           KLS=case_when(grepl('Closure',Label)~'Closure',
                         grepl('listing',Label)~'Listing',
                         TRUE~'Other'),
           Label=str_remove(Label, "Closure - "))
  
  
  
  Hoy=as.character(format(Sys.Date(), "%d/%m/%Y"))
  Time.blocks=data.frame(Type=factor(c('Open access','Managed'),levels=c('Open access','Managed')),
                         Date.start=as.POSIXct(c("01/01/1940",Start.managed),format="%d/%m/%Y"),
                         Date.end=as.POSIXct(c(Start.managed,Hoy),format="%d/%m/%Y"))%>%
    mutate(Mid=Date.start+(Date.end-Date.start)/2 )
  
  
  p=ggplot()+
    geom_rect(data=Time.blocks,aes(xmin = Date.start,xmax = Date.end,ymin=0,ymax=2,
                                   fill=Type),show.legend = FALSE)+
    geom_text(data=Time.blocks,aes(x=Mid,y=1,label=Type),size=5,color='white')+
    scale_x_date(date_labels = "%Y", date_breaks = "10 years")+
    geom_segment(data=Management,aes(x = date, y = id, xend = date, yend = 0),
                 linetype = "dashed", color = "grey40")+
    geom_text(data = Management, aes(x = date, y = id, label = Label,color = KLS), 
              angle = 0, vjust = .5, hjust = -.025, size = 3.5)+
    geom_point(data = Management,aes(x=date,y=id))+xlab('')+
    scale_y_continuous(limits = c(min(Management$id),Ymax),
                       labels = function(x) ifelse(x < 0, "", x))+
    coord_cartesian(clip = "off")+ 
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = 'bottom',
          legend.title = element_blank(),
          plot.margin = margin(5.5, 130, 5.5, 5.5, "pt"),
          legend.key = element_blank())+
    scale_fill_manual(values = c("Open access" = "black", "Managed" = "grey40"))+
    scale_color_manual(values = c("Closure" = "red4", "Listing" = "steelblue",
                                  "Other" = "black"))+
    guides(color = guide_legend(
      override.aes = list(label = c("Closure", "Listing", "Other"),size=5, face = "bold"), 
      label.theme = element_text(color='transparent') 
    ))
  return(p)
}

#Old timeline used in SOFAR
AMM.ktch.eff.managmtnt=function(GROUP,LAT1,LAT2,MGMT,SEP,Lab.font,Lab.back,
                                nn,frms,n.frms,do.animation,speed)
{
  dat=subset(Data.monthly,SPECIES%in%GROUP & LAT<=LAT1 & LAT >=LAT2 & METHOD%in%c("GN","LL") &
               Estuary=="NO")%>%
    filter(!Shark.fishery=="non.shark.fishery")
  
  #aggregate by total
  annual.catch.total=aggregate(LIVEWT.c~FINYEAR,data=dat,sum,na.rm=T)%>%
    mutate(finyear=1+as.numeric(substr(FINYEAR,1,4)),
           LIVEWT.c=LIVEWT.c/1000)%>%
    dplyr::select(-FINYEAR)
  
  dummy=annual.catch.total[1:nrow(MGMT),]%>%
    mutate(LIVEWT.c=NA,
           finyear=MGMT$finyear)%>%
    filter(!finyear%in%annual.catch.total$finyear)
  annual.catch.total=rbind(annual.catch.total,dummy)
  
  Man=MGMT%>%dplyr::select(Event,finyear,Category)%>%mutate(id=1:nrow(MGMT))
  
  d=annual.catch.total%>%
    left_join(Man,by='finyear')%>%
    arrange(finyear)%>%
    filter(finyear>=1975)%>%
    mutate(LIVEWT.c=ifelse(is.na(LIVEWT.c),na.approx(LIVEWT.c),LIVEWT.c),
           Category=ifelse(is.na(Category),"",Category),
           Event=ifelse(is.na(Event),"",Event))
  
  set.seed(666)
  colors=c(Data='red',Closure='steelblue',Other='forestgreen','NA'='transparent')
  p1=ggplot(d,
            aes(finyear, LIVEWT.c,label=Event,color = factor(Category))) +
    geom_line(colour="orange",size=1.25) + geom_point() +
    geom_label_repel(box.padding=SEP,hjust = 0,size = Lab.font,fill=Lab.back) + 
    geom_line(colour="orange",size=1.25, alpha = 0.3) + geom_point(alpha = 0.4)+
    ylab("Cach (tonnes live wt)") + xlab("Financial year")+
    theme(legend.position = "none",
          axis.text=element_text(size=12),
          axis.title=element_text(size=14)
          # panel.background = element_rect(fill = "black", color  =  NA),
          # panel.border = element_rect(fill = NA, color = "white"),  
          # panel.grid.major = element_line(color = "grey20"),  
          # panel.grid.minor = element_line(color = "grey20"),  
          # panel.spacing = unit(0.5, "lines")
    )+
    scale_color_manual(values = colors)
  list.out=list(p1=p1) 
  
  #animation
  if(do.animation)
  {
    Min.yr=min(dat$YEAR.c)
    
    a=dat%>%
      mutate(finyear=1+as.numeric(substr(FINYEAR,1,4)))%>%
      group_by(finyear)%>%
      summarise(LIVEWT.c=sum(LIVEWT.c)/1000)
    
    b=a[rep(1:nrow(a),nn),]%>%
      arrange(finyear)
    SEQ=seq(1,nrow(b),by=nn)
    b$finyear[-SEQ]=NA
    b$LIVEWT.c[-SEQ]=NA
    b$finyear=with(b,ifelse(is.na(finyear),na.approx(finyear),finyear))
    b$LIVEWT.c=with(b,ifelse(is.na(LIVEWT.c),na.approx(LIVEWT.c),LIVEWT.c))
    
    Man1=b%>%
      full_join(Man%>%filter(finyear>=Min.yr),by='finyear')%>%
      filter(finyear>=Min.yr)%>%
      mutate(Event=ifelse(is.na(Event),'',Event),
             Category=ifelse(is.na(Category),'',Category))%>%
      group_by(Event)%>%
      mutate(n=n())%>%
      mutate(temp = case_when((n >= n.frms) ~ (1),
                              (n<n.frms) ~ (n.frms))) %>%
      uncount(temp)%>%
      dplyr::select(-c(id,n))%>%
      arrange(finyear)%>%
      data.frame
    #dummy=Man1[rep(1,5),]
    #dummy$Event=dummy$Category=''
    #dummy$finyear=seq(dummy$finyear[1]-.5,dummy$finyear[1]-.99,length.out=nrow(dummy))
    #Man1=rbind(dummy,Man1)
    Man1$ID=1:nrow(Man1)
    Man1$LIVEWT.c=with(Man1,ifelse(is.na(LIVEWT.c),na.approx(LIVEWT.c, rule=2),LIVEWT.c))
    Man1$Event.yr=with(Man1,ifelse(Event!='',paste(round(finyear),Event,sep=': '),''))
    anim <- ggplot(Man1, aes(finyear, LIVEWT.c,label=Event.yr)) +
      geom_line(colour="orange",size=3) +
      geom_label_repel(box.padding=SEP,hjust = 0,size = 9,fill=Lab.back)+
      theme(legend.position = "none",
            axis.text=element_text(size=20),
            axis.title=element_text(size=24))+
      scale_color_manual(values = colors)+
      ylab("Cach (tonnes live wt)") + xlab("Financial year")+
      transition_reveal(as.numeric(ID))
    p.animate=animate(anim,nframes=frms,duration = speed, height = 700, width = 1000)
    list.out$p.animate=p.animate
    
  }
  return(list.out)
}
AMM.eff.managmtnt=function(dat,MGMT,SEP,Lab.font,Lab.back,Effort.lab)
{
  d=dat%>%
    mutate(finyear=as.numeric(substr(FINYEAR,1,4)))
  Man=MGMT%>%
    dplyr::select(Event,finyear,Category)%>%
    mutate(id=1:nrow(MGMT))%>%
    filter(finyear>=min(d$finyear))
  d=d%>%
    full_join(Man,by='finyear')%>%
    arrange(finyear)%>%
    filter(finyear>=1975)%>%
    mutate(Total=ifelse(is.na(Total),na.approx(Total),Total),
           Category=ifelse(is.na(Category),"",Category),
           Event=ifelse(is.na(Event),"",Event))
  
  this=which(!is.na(d$FINYEAR))
  this=this[length(this)]
  
  if(is.na(d$FINYEAR[nrow(d)])) 
  {
    d$Total[(this+1):nrow(d)]=d$Total[this]
  }
  
  colors=c(Data='red',Closure='steelblue',Other='forestgreen','None'='orange')
  p=d%>%
    mutate(Category=ifelse(Category=='','None',Category),
           Category=factor(Category))%>%
    ggplot(aes(finyear, Total,label=Event,color = Category)) +
    geom_line(colour="orange",size=1.25) + 
    #geom_point() +
    geom_label_repel(box.padding=SEP,min.segment.length=0,hjust = 0,size = Lab.font,fill=Lab.back,max.overlaps=30) + 
    geom_line(colour="orange",size=1.25, alpha = 0.3) + 
    #geom_point(alpha = 0.4)+
    ylab(Effort.lab) + xlab("Financial year")+
    theme(legend.position = "none",
          axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    scale_color_manual(values = colors)
  return(print(p))
}