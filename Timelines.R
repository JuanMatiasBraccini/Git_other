#Some sources
# https://r-graph-gallery.com/web-sankey-refugees.html
#https://r-graph-gallery.com/web-sankey-diagram-with-highlight.html


library(tidyverse)
library(ggsankey)
library(shadowtext)
library(readr)
library(ggrepel)
library(png)
library(cowplot)
library(stringr)
library(scales)
library(rphylopic)
library(rnaturalearth)
library(readxl)

if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

hndl.in=handl_OneDrive("Scientific manuscripts/Perspective_100 years WA Fisheries/Timelines/")
hndl.out=handl_OneDrive("Scientific manuscripts/Perspective_100 years WA Fisheries/Timelines/Outputs/")



# Functions -----------------------------------------------------------
function.sankey.timeline=function(d,pal,Title,Subtitle,Caption,Marg1, Marg2, Marg3, Marg4)
{
  #base plot
  p <- ggplot(d, aes(x = decade, node = node, fill = node, value = prop, label = node)) +
    geom_sankey_bump() +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "grey99", color = NA))
  
  #prepare labels
  g_labs_start <- ggplot_build(p) %>%
    .$data %>%
    .[[1]] %>%
    group_by(label) %>%
    filter(x == min(x)) %>%
    reframe(
      x,
      y = mean(y)
    ) %>%
    left_join(d %>% group_by(node) %>% filter(as.numeric(decade) == min(as.numeric(decade))), by = c("label"="node"))%>%
    mutate(node=label)
  
  g_labs_end <- ggplot_build(p) %>%
    .$data %>%
    .[[1]] %>%
    group_by(label) %>%
    filter(x == max(x)) %>%
    reframe(
      x,
      y = mean(y)
    ) %>%
    left_join(d %>% group_by(node) %>% filter(as.numeric(decade) == max(as.numeric(decade))), by = c("label" = "node"))%>%
    mutate(node=label)
  
  #combine all up
  Plot <- p +
    geom_shadowtext(data = g_labs_start, aes(x, y, label = paste(label, "·", n), color = label), hjust = 1, nudge_x = -0.1, bg.color = "grey99", fontface = "bold") +
    geom_shadowtext(data = g_labs_end, aes(x, y, label = paste(label, "·", n), color = label), hjust = 0, nudge_x = 0.1, bg.color = "grey99") +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    labs(title = Title, subtitle = Subtitle, caption = Caption) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "grey99", color = NA),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(lineheight = 1),
      plot.caption = element_text(margin = margin(10, 0, 0, 0), hjust = 0),
      plot.margin = margin(Marg1, Marg2, Marg3, Marg4))
  
  return(Plot)
}

function.segment.timeline=function(d,Start.x,End.x,Show.Leg=TRUE,repel.free=TRUE,pt.size=2.5,
                                   add.point=TRUE,lbl.size=9,axis.leg.size=16,
                                   x_axis.pos='bottom',Hjust=0.1)
{
  d=d%>%
    mutate(y=row_number())
  if('No.of.staff'%in%names(d))
  {
    d=d%>%
      mutate(y=approxfun(range(No.of.staff), c(min(y), max(y)))(No.of.staff))
  }
  if('Topic'%in%names(d))
  {
    p=d%>%
      ggplot(aes(Year_from,y,color=Topic))+
      geom_segment(aes(xend=Year_to,yend=y,color=Topic),linewidth = 1.2*pt.size)
  }else
  {
    p=d%>%
      ggplot(aes(Year_from,y))+
      geom_segment(aes(xend=Year_to,yend=y),linewidth = 1.2*pt.size)
  }
  if(add.point) p=p+geom_point(size=pt.size,shape=15)
  p=p+
    scale_x_date(name = "", date_breaks = "20 years", date_labels = "%Y",
                 limits=c(as.Date(Start.x),as.Date(End.x)),position=x_axis.pos) +
    scale_y_discrete(name = "") +
    theme_minimal() +
    theme(legend.position = "top")
  if(!is.null(lbl.size))
  {
    if(!repel.free)
    {
      p=p+
        geom_text(aes(x=Year_from,y=y,label=Label),size=lbl.size,show.legend = FALSE, vjust = -1,hjust=Hjust)
    }else
    {
      p=p+
        geom_text_repel(aes(x=Year_from,y=y,label=Label),size=lbl.size,show.legend = FALSE) 
    }
  }
  if(is.null(lbl.size))
  {
    if(!repel.free)
    {
      p=p+
        geom_text(aes(x=Year_from,y=y,label=Label,size=lbl.size),show.legend = FALSE, vjust = -1,hjust=0.1)
    }else
    {
      p=p+
        geom_text_repel(aes(x=Year_from,y=y,label=Label,size=lbl.size),show.legend = FALSE) 
    }
  }
  
  if(!Show.Leg) p=p+theme(legend.position = 'none')
  p=p+theme(axis.text.x = element_text(size = axis.leg.size))
  return(p)
}

function.chronology.basic.timeline=function(d,Start.x,End.x,yr.seq=25,Lbl.width=20,delta.y=0.3)
{
  Years=seq(year(Start.x), year(End.x))
  Years.cut=seq(year(Start.x), year(End.x),by=yr.seq)
  basemap.polygon=data.frame(Start=Years.cut)%>%
                            mutate(To=lead(Start))%>%
                            filter(!is.na(To))%>%
                            mutate(To=To-1,
                                   y=2*rev(row_number()),
                                   x=y/2)
  #create basemap
  basemap=basemap.polygon%>%
            ggplot(aes(xmin = 1, xmax = max(x), ymin = y-delta.y, ymax = y)) +
            geom_rect(fill = "powderblue",color='black')+
                      theme(legend.position = "top")+
            ylim(NA,max(basemap.polygon$y)+1.5)
  
  #add year ranges
  X.min=min(basemap.polygon$x)
  X.max=max(basemap.polygon$x)
  X.mid=mean(c(X.min,X.max))
  yr.txt=basemap.polygon%>%
    gather(Period,Year,-c(x,y))%>%
    arrange(-y)
  yr.txt=rbind(yr.txt,yr.txt%>%
                 filter(Period=='Start')%>%
                 mutate(Period='Start.1',
                        Year=Year+mean(basemap.polygon$To-basemap.polygon$Start)/2))%>%
    arrange(-y,Period)%>%
    mutate(x=case_when(Period=="Start" ~ X.min,
                       Period=="Start.1" ~ X.mid-.1,
                       Period=="To" ~ X.max-.27))
  p=basemap+
    geom_text(data=yr.txt,aes(x,y,label=Year),size=4,show.legend = FALSE, vjust = 1.1,hjust=0)
  
  #add text  
  Text.lbls=d%>%
    dplyr::select(-c(Year_from,Year_to))%>%
    mutate(Label=str_wrap(Label,width=Lbl.width))
  
  df=basemap.polygon%>%
    dplyr::select(-x)%>%
    gather(Period,Year,-y)%>%
    arrange(-y)%>%
    dplyr::select(-Period)
  expanded_df <- df %>%
    group_by(y) %>%
    expand(Year = full_seq(Year, 1)) %>%
    left_join(df, by = c("y", "Year"))
  
  dummy=vector('list',nrow(Text.lbls))
  for(i in 1:length(dummy))
  {
    ddd=data.frame(Year_start=Text.lbls$Year_start[i])
    ddd=ddd%>%
      mutate(y=expanded_df$y[which.min(abs(expanded_df$Year - ddd$Year_start))])
    Get.x=basemap.polygon%>%filter(y==ddd$y)
    X.yrs=seq(Get.x$Start,Get.x$To)
    X.yrs.scaled=seq(X.min,X.max,length=length(X.yrs))
    ddd=ddd%>%
      mutate(x=X.yrs.scaled[which.min(abs(X.yrs - ddd$Year_start))],
             y=y-delta.y)
    dummy[[i]]=ddd
  }
  dummy=do.call(rbind,dummy)
  Text.lbls=Text.lbls%>%
    left_join(dummy,by='Year_start')
  
  p=p+
    geom_segment(data=Text.lbls,
                 aes(x = x, y = y+delta.y*.5, xend = x, yend = y+2*delta.y), # Connect labels to timeline
                 color = "black") +
    geom_label(data=Text.lbls,
               aes(x,y,label = Label,fill=Topic),show.legend = FALSE,vjust = -0.5,hjust = 0.5)

  
  #final touches
  p=p+
    theme_void()
  

  return(p) 
}

colfunc <- colorRampPalette(c("slategray1", "royalblue4")) #plot(1:8,1:8,col=colfunc(8), pch = 19, cex = 3)
function.chronology.pop.growth.timeline=function(d,Labls,lbl.width,start.year,end.year,
                                                 Nudge_y,Nudge_x,Point.padding,Box.padding,
                                                 Force,text.labelling,lbl.size,ln.width,
                                                 Arrow.width,Kls,Max.ovrlp,add.arrow,Topic.kls)
{
  #base map
  p=d%>%
    ggplot(aes(Year,Total))+
    labs(caption = 'Source: ABS Historical Population Statistics')+
    scale_x_date(name = "", date_breaks = "20 years", date_labels = "%Y",
                 limits=c(as.Date(paste0(start.year-6,'-01-01')),as.Date(paste0(end.year,'-01-01')))) +
    scale_y_continuous(name = 'Population size of Western Australia',labels = label_number(scale = 1e-6, suffix = "M"))+
    theme_minimal() +
    theme(legend.position = "top",
          axis.title=element_text(size=14),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
  if(!add.arrow)  p=p+geom_line(linewidth=ln.width,color=Kls[7])
  if(add.arrow) p=p+geom_line(linewidth=ln.width,color=Kls[7],arrow = arrow(type = "open",length=unit(Arrow.width, "npc")))
  
  #color code periods
  p=p+
    geom_line(data=d%>%filter(Year>=as.Date(paste0(start.year-6,'-01-01')) & Year<= as.Date(paste0(1900,'-01-01'))),
              aes(Year,Total),linewidth=ln.width,color=Kls[1])+
    geom_line(data=d%>%filter(Year>=as.Date(paste0(1899,'-01-01')) & Year<= as.Date(paste0(1920,'-01-01'))),
              aes(Year,Total),linewidth=ln.width,color=Kls[2])+
    geom_line(data=d%>%filter(Year>=as.Date(paste0(1919,'-01-01')) & Year<= as.Date(paste0(1940,'-01-01'))),
              aes(Year,Total),linewidth=ln.width,color=Kls[3])+
    geom_line(data=d%>%filter(Year>=as.Date(paste0(1939,'-01-01')) & Year<= as.Date(paste0(1960,'-01-01'))),
              aes(Year,Total),linewidth=ln.width,color=Kls[4])+
    geom_line(data=d%>%filter(Year>=as.Date(paste0(1959,'-01-01')) & Year<= as.Date(paste0(1980,'-01-01'))),
              aes(Year,Total),linewidth=ln.width,color=Kls[5])+
    geom_line(data=d%>%filter(Year>=as.Date(paste0(1979,'-01-01')) & Year<= as.Date(paste0(2000,'-01-01'))),
              aes(Year,Total),linewidth=ln.width,color=Kls[6])+
    geom_line(data=d%>%filter(Year>=as.Date(paste0(2000,'-01-01')) & Year<= as.Date(paste0(2020,'-01-01'))),
              aes(Year,Total),linewidth=ln.width,color=Kls[7])+
    geom_line(data=d%>%filter(Year>=as.Date(paste0(2020,'-01-01')) & Year<= as.Date(paste0(2025,'-01-01'))),
              aes(Year,Total),linewidth=ln.width,color=Kls[8])
  
  #add labels
  timeline.labls=Labls%>%
    rename(x=Year_from)%>%
    dplyr::select(x,Label,Topic)
  timeline.labls$y=d$Total[match(timeline.labls$x,d$Year)]
  
  if(text.labelling=='repel')
  {
    timeline.labls=timeline.labls%>%mutate(Label=str_wrap(Label,width=lbl.width))
    p=p+
      geom_text_repel(data=timeline.labls,aes(x,y,label = Label,color=Topic),
                      show.legend = FALSE,max.overlaps = Max.ovrlp,min.segment.length = 0,
                      nudge_y=Nudge_y,nudge_x = Nudge_x, point.padding = Point.padding,
                      box.padding = Box.padding,force=Force,seed=666,size=lbl.size)+
      scale_color_manual(values=Topic.kls) 
  }
  if(text.labelling=='basic')
  {
    timeline.labls2=timeline.labls%>%
      mutate(x2=lag(x),
             Delta.t=x-x2,
             v=ifelse(Delta.t<1500,-2,0.5),
             h=ifelse(x>(as.Date('1960-01-01')),1.2,-0.2))
    p=p+
      geom_text(data=timeline.labls2,aes(x,y,label = Label,
                                         hjust = h, vjust=v),size=lbl.size,
                show.legend = FALSE)
  }
  return(p)
}


# Chronology population growth-----------------------------------------------------------
Start.Fisheries.Department=1893
WA.population=read.csv(paste0(hndl.in,'WA population.csv'))%>%
  mutate(Year=as.Date(paste0(Year,'-01-01')),
         Total=as.numeric(Total))
Chronology_original=read.csv(paste0(hndl.in,'table - timeline chronology.csv'))
Chronology_Karina=read_excel(paste0(hndl.in,'table - timeline chronology (1)(100 years timeline milestones).xlsx'), sheet = "Data",skip = 0)
Chronology=read_excel(paste0(hndl.in,'timeline - condensed for figure_AB.xlsx'), sheet = "Sheet1",skip = 0)

if(!'ForTimelineFig'%in%names(Chronology)) Chronology$ForTimelineFig='Y'
Chronology=Chronology%>%
  mutate(Topic=ifelse(is.na(Theme),'No topic',Theme),
         Label=ifelse(is.na(Label),'No label',
               ifelse(Label=='Right to fish' & Year<1900,paste0(Label,' (',Year,')'),
                      Label)),
         ForTimelineFig=case_when(ForTimelineFig=='YES'~'Y',
                                  TRUE~ForTimelineFig),
         Year=ifelse(Year<1900 & grepl('Right to fish',Label),1900,Year),
         Year_start=Year,
         Year_end=NA,
         Year_start1=ifelse(!is.na(Year_end),(Year_end+Year_start)/2,Year_start),
         Year_from=as.Date(paste0(round(Year_start1),'-01-01')))%>%
  filter(!is.na(ForTimelineFig))%>%
  filter(!ForTimelineFig=='N')%>%
  arrange(Year_from)%>%
  dplyr::select(-c(Year_end))

#Start chronology
Start.chronos=1925
Chronology=Chronology%>%filter(Year>=Start.chronos)

# Chronology=Chronology%>%
#   mutate(type=ifelse(is.na(Year_end),'dot','bar'),
#          Year_start1=ifelse(!is.na(Year_end),(Year_end+Year_start)/2,Year_start),
#          Year_from=as.Date(paste0(Year_start1,'-01-01')),
#          Year_to=ifelse(is.na(Year_end), Year_start1,Year_end),
#          Year_to=as.Date(paste0(Year_to,'-01-01')))%>%
#   arrange(Year_from)


#Inset - Add shape of main commercial species proportional to catch
#note: relative importance from FISHCUBE query of total catch ever reported
#http://f01-fims-webp01/FishCubeWA/Query.aspx?CubeId=CommercialDPIRDOnly&QueryId=e921033d-5244-463d-9d06-ce4309298d83
get.fishcube=FALSE
if(get.fishcube)
{
  FishCubeWA=read_excel(paste0(hndl.in,'Fish Cube WA - Data Extract -Catch totals all spp.xlsx'), sheet = "Data",skip = 0)
  FishCubeWA=FishCubeWA%>%
    rename(Group='Species Group',
           Species='Species CAAB Code',
           Name='Species Common Name',
           finyear='Financial Year',
           Tonnes='Weight (Tonnes)')%>%
    filter(!Name%in%c('Grand Total','Nil Fish Caught'))%>%
    filter(!Species%in%c('Grand Total','Nil Fish Caught'))%>%
    filter(!Group%in%c('Grand Total','Nil Fish Caught'))%>%
    mutate(Group1=case_when(grepl('Lobster',Name)~'Lobster',
                            grepl('Crab',Name)~'Crab',
                            grepl('Abalone',Name)~'Abalone',
                            grepl('Scallop',Name)~'Scallop',
                            TRUE~Group))
  #mean annual catch since 1980
  Top.catch=FishCubeWA%>%
    mutate(finyear=as.numeric(substr(finyear,1,4)))%>%
    filter(finyear>=1980)%>%
    group_by(Group1)%>%
    summarise(Tonnes=sum(Tonnes))%>%   
    arrange(-Tonnes)
  
  Tot.years=FishCubeWA%>%
    mutate(finyear=as.numeric(substr(finyear,1,4)))%>%
    filter(finyear>=1980)%>%
    distinct(Group1,finyear)%>%
    group_by(Group1)%>%
    tally()%>%rename(n.years=n)
  
  Mean.annual.catch=Top.catch%>%
                      left_join(Tot.years,by='Group1')%>%
                      mutate(Mean.anual.catch=Tonnes/ n.years)
  
}
Species.list=data.frame(Common=c('Rocklobster','Prawn','Scallop','Abalone','Echinoderms',
                                 'Crab','Cephalopods', 'Scalefish','Shark'),
                        Scientific=c('Panulirus','Penaeus','Pecten','Haliotis','Holothuria',
                                     'portunus diacantha','Octopodinae','chrysophrys aculeata','Carcharhinus'),
                        Value=c(8964,3096,3276,263,91.7,
                                644,319,12956,1431))
Sp.shape=lapply(Species.list$Scientific, function(x) get_phylopic(get_uuid(name = x), format = "raster"))

Skaler=4
Species.list=Species.list%>%
  mutate(log.Value=log(Value),
         ValueRel=log.Value,
         ValueRel=ValueRel/max(ValueRel),
         Value.rel=ValueRel*Skaler)
write.csv(Species.list%>%rename(Tonnes=Value,
                                Tonnes.logged=log.Value,
                                Tonnes.logged.rel=ValueRel,
                                Value.figure=Value.rel)%>%
                        mutate(Scaler=Skaler,),paste0(hndl.out,"Species.list.csv"),row.names = F)


    #basemap
SP.kol='brown4'
Australia <- ne_states(country = "Australia", returnclass = "sf")
Limy=c(-44,-10)
Limx=c(108,154)
p_Map=ggplot(data = Australia) +
  geom_sf(color = "black", fill = "grey80") +
  xlab("") + ylab("")+
  coord_sf(xlim =Limx , ylim = Limy, expand = T)+
  theme_void()+labs(subtitle="Harvested resources")+
  theme(plot.subtitle = element_text(size=11,vjust = -3,color=SP.kol))

    #add ellipse
add.ellipse=FALSE
if(add.ellipse)
{
  p_Map=p_Map+
    stat_ellipse(data=data.frame(x=c(113,127,113,127), y=c(-34,-34,-18,-18)),
                 aes(x,y),level = 0.70,linewidth = 1.25,color=SP.kol)
}


    #add species
Species.list=Species.list%>%
  mutate(x=c(113,114,111,121,122,116.5,111.5,118,128),
         y=c(-32,-21,-24,-36,-13,-36,-28,-17,-34.5),
         x.end=c(114.5,116,114.25,121,122,117,113.5,118,128),
         y.end=c(-31,-23,-24,-34,-14,-34.5 ,-28,-18.5,-32.5))
p_inset=p_Map
for(i in 1:length(Sp.shape))
{
  p_inset=p_inset+add_phylopic(x = Species.list$x[i], y = Species.list$y[i],Sp.shape[[i]],
                               alpha = 1,fill=SP.kol, height = Species.list$Value.rel[i])
}

    #add arrows
p_inset=p_inset+
  geom_curve(data=Species.list,aes(x = 122, y = -25, xend = x.end, yend = y.end),size = .8,
             arrow = arrow(length = unit(0.3, "cm"), type = "open"),
             color = SP.kol, curvature = -0.2,show.legend = FALSE)

#Main plot
topic.kls=c("black","darkolivegreen4","darkorange3","brown4","darkslategray3","azure4",
            "chartreuse3","antiquewhite4","red")
names(topic.kls)=c("Recreational","Assessment","Commercial","Monitoring","Population","Indigenous",
                   "No topic","Management","Department")

   

p=function.chronology.pop.growth.timeline(d=WA.population,
                                          Labls=Chronology,
                                          lbl.width=45,
                                          start.year=Start.Fisheries.Department,
                                          end.year=2024,
                                          Nudge_y=5,
                                          Nudge_x = 5,
                                          Point.padding = 1,
                                          Box.padding = .5,
                                          Force=1.5,
                                          text.labelling='repel',
                                          lbl.size=2.8,
                                          ln.width=8,
                                          Arrow.width=.125,
                                          Kls=rev(colfunc(8)),
                                          Max.ovrlp=100, #Max.ovrlp=Inf
                                          add.arrow=FALSE,
                                          Topic.kls=topic.kls) 


#total catch pie chart
fun.pie=function(data,Xmax,Xmin,Fill.kl,ALfa)
{
  data$fraction = data$count / sum(data$count)
  
  # Compute the cumulative percentages (top of each rectangle)
  data$ymax = cumsum(data$fraction)
  
  # Compute the bottom of each rectangle
  data$ymin = c(0, head(data$ymax, n=-1))
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  
  # Compute a good label
  data$label <- paste0(data$category, "\n value: ", data$count)
  p=ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=Xmax, xmin=Xmin, fill=category)) +
    geom_rect(color=1,alpha=ALfa) +
    scale_fill_manual(values=Fill.kl) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "none")
  return(p)
  
}
XMAX=4
XMIN=3
Spec.kols=c('tomato3','sienna','thistle2','olivedrab','khaki3','peru','violetred4','lightcoral','grey60')
names(Spec.kols)=Species.list$Common 
p_pie_inset=fun.pie(data=Species.list%>%
                      rename(category=Common,
                             count=Value),
                    Xmax=XMAX,
                    Xmin=XMIN,
                    Fill.kl=Spec.kols,
                    ALfa=.8)

dumy.dat=Species.list%>%
  left_join(p_pie_inset$data%>%dplyr::select(category,labelPosition),
            by=c('Common'='category'))%>%
  mutate(x.pie=case_when(Common%in%c('Crab')~4,
                         Common%in%c('Cephalopods')~2.9,
                         Common%in%c('Echinoderms')~3.25,
                         Common%in%c('Abalone')~3.6,
                         TRUE~(XMAX+XMIN)/2),
         dumy.height=case_when(Common%in%c('Shark','Prawn','Scallop','Crab','Cephalopods')~.2,
                               Common%in%c('Rocklobster')~.4,
                               Common%in%c('Scalefish')~.3,
                               Common%in%c('Echinoderms','Abalone')~.1))
SP.kl='same' #'group'
for(i in 1:length(Sp.shape))
{
  if(SP.kl=='group') disKl=Spec.kols[i]
  if(SP.kl=='same') disKl='black'
  p_pie_inset=p_pie_inset+add_phylopic(x = dumy.dat$x.pie[i], y = dumy.dat$labelPosition[i],Sp.shape[[i]],
                                       alpha = 1,fill=disKl, height =dumy.dat$dumy.height[i])
}

#Combine all
ggdraw(p) +
  draw_plot(p_inset, x = 0.05, y = 0.4, width = 0.5, height = 0.68) 
ggsave(paste0(hndl.out,"Chronology_pop.growth_v1.jpg"),width = 8,height = 6)

ggdraw(p) +
  draw_plot(p_inset, x = 0, y = 0.4, width = 0.5, height = 0.68) + 
  draw_plot(p_pie_inset, x = 0.2, y = 0.66, width = 0.35, height = 0.35)
ggsave(paste0(hndl.out,"Chronology_pop.growth_v2.jpg"),width = 8,height = 6)

ggdraw(p) +
  draw_plot(p_Map, x = 0, y = 0.4, width = 0.5, height = 0.68) + 
  draw_plot(p_pie_inset, x = -0.01, y = 0.65, width = 0.325, height = 0.325)
ggsave(paste0(hndl.out,"Chronology_pop.growth_v3.jpg"),width = 8,height = 6)



# Chronology Andrew basic-----------------------------------------------------------
do.this=FALSE
if(do.this)
{
  function.chronology.basic.timeline(d=Chronology,
                                     Start.x=as.Date("1900-01-01"),End.x=as.Date("2025-01-01"))
  ggsave(paste0(hndl.out,"Chronology_basic.jpg"),width = 6,height = 8)
  
  
}


# Stock assessment evolution -----------------------------------------------------------
do.this=FALSE
if(do.this)
{
  stock.ass.evol <- read_csv(paste0(hndl.in,'assessment_timeline.csv'))
  
  function.sankey.timeline(d=stock.ass.evol%>%
                             rename(node=Ass.type,prop=Prop,
                                    decade=Decade,
                                    n=N.stock.ass)%>%
                             mutate(decade=factor(decade)),
                           pal=c(L1="#FDA638",L2="#EB7C69",L3="#866f85",L4="#459395",L5="forestgreen"),
                           Title="Stock assessment evolution",
                           Subtitle=str_wrap("The chart shows the evolution of stock assessment in WA by decade. Some other text.", 80),
                           Caption=str_wrap("Line thickness represents the proportion of assessment type occurrences within each decade. 
                                          The numbers at each end of a line show the stock assessment type count in the first and last decades, respectively.",120),
                           Marg1=10, Marg2=40, Marg3=10, Marg4=20)
  ggsave(paste0(hndl.out,"Evolution of stock assessments.tiff"),width = 8,height = 6,compression = "lzw")
  
}


# Department's title change, staff and papers ---------------------------------------------------------
#Sourced by Paul Orange
papers=read_excel(paste0(hndl.in,'Publications - journal articles, etc. - by Department of Fisheries staff.xlsx'), sheet = "Sheet1",skip = 0)
papers=papers%>%
  rename(n='No. of Publications')%>%
  mutate(type='Papers & conferences')%>%
  filter(Year<2017) #incomplete since 2017

# papers=data.frame(year=rep(c(1960,1970,1980,1990,2000,2010,2020),2),
#                   n=c(10,20,30,80,180,250,380,   1,2,10,25,29,48,100),
#                   type=c(rep('Paper',7),rep('Report',7)))
p_papers=papers%>%
        ggplot(aes(Year,n,color=type))+
        geom_point(size=2.5)+
        geom_line(linetype='dotted',linewidth=1.05,show.legend = FALSE)+theme_minimal() +
        theme(legend.position = c(.5, .95),
              legend.title = element_blank(),
              legend.text=element_text(size=10),
              axis.text=element_text(size=10),
              axis.title=element_text(size=11))+
        ylab('Number of publications')+  xlab('')+
        guides(colour = guide_legend(nrow = 1))

department.title=data.frame(Name=c('Fisheries Department','Department of Aborigines and Fisheries',
                                   'Fisheries Department','Department of Fisheries and Fauna',
                                   'Department of Fisheries and Wildlife','Fisheries Department',
                                   'Fisheries Western Australia','Department of Fisheries',
                                   'DPIRD'),
                            Commenced=as.Date(c("1893-01-01", "1909-01-01","1920-01-01",
                                                "1964-10-01","1974-01-01","1985-03-08",
                                                "1997-11-01","2001-07-01",
                                                "2017-06-30")),
                            Name.change=as.Date(c("1909-01-01","1920-01-01","1964-09-01",
                                                  "1974-01-01","1985-03-08","1997-11-01",
                                                  "2001-07-01","2017-06-30",
                                                  "2025-09-17")))
department.staff=data.frame(date.from=as.Date(c("1893-01-01","1898-02-01","1938-02-01", "1953-02-01","1968-02-01","1984-02-01","1990-03-01",  "2000-02-01","2017-02-01")),
                              date.to=as.Date(c("1898-01-01","1938-01-01","1953-01-01", "1968-01-01","1984-01-01","1990-02-01","2000-01-01",  "2017-01-01","2017-03-01")),
                            No.of.staff=c(4,8,10,35,100,200,207,388,466))
#fix DPIRD staff to actual fisheries staff (Jenny Moore)
do.this.fix=FALSE
if(do.this.fix)
{
  N.positions=c(Hillyars=147,Aquatic.Resource.management=44,Aquaculture.Management=10,
                Operations.Compliance.Education=346)
  department.staff=rbind(department.staff,
                        data.frame(date.from=as.Date("2017-04-01"),
                                   date.to=as.Date("2025-10-01"),
                                   No.of.staff=sum(N.positions)))
}


#create timeline
p=function.segment.timeline(d=department.title%>%
                              mutate(Topic=Name)%>%
                            rename(Year_from=Commenced,
                                   Year_to=Name.change,
                                   Label=Name),
                            Start.x="1890-01-01",End.x="2025-12-01",
                            Show.Leg=FALSE,repel.free=FALSE,
                            pt.size=4,add.point=FALSE,lbl.size=5,axis.leg.size=16,
                            x_axis.pos='top')
Name.cols=c('navyblue','slategray','skyblue3','royalblue','turquoise3','steelblue','tomato')
names(Name.cols)=c("Fisheries Department","Department of Aborigines and Fisheries",
                   "Department of Fisheries and Fauna","Department of Fisheries and Wildlife",
                   "Fisheries Western Australia","Department of Fisheries","DPIRD")
p=p+
  scale_color_manual(values = Name.cols)+
  labs(caption = 'DPIRD, Department of Primary Industries and Regional Development')

#create inset
p_inset=function.segment.timeline(d=department.staff%>%
                                    mutate(Label=No.of.staff,
                                           lbl.size=No.of.staff^0.1)%>%
                                    rename(Year_from=date.from,
                                           Year_to=date.to),
                                  Start.x="1890-01-01",End.x="2025-12-01",
                                  Show.Leg=FALSE,repel.free=FALSE,
                                  pt.size=3,add.point=FALSE,lbl.size=3.2,axis.leg.size=8,Hjust=0.2)+
  ggtitle("Number of fisheries research staff")+
  theme(plot.title = element_text(margin = margin(b = -15)))

#add staff
ggdraw(p) +
  draw_plot(p_inset+
              theme(panel.border = element_rect(colour = "grey90", fill = NA, size = 1.5),
                    panel.background = element_rect(fill = "grey90")),
            x = 0, y = 0.38, width = 0.5, height = 0.55)
ggsave(paste0(hndl.out,"Department's title change and staff_v1.jpg"),width = 10,height = 6.5)  

#add publications
ggdraw(p) +
  draw_plot(p_inset+
              theme(panel.border = element_rect(colour = "grey90", fill = NA, size = 1.5),
                    panel.background = element_rect(fill = "grey90")),
            x = 0, y = 0.38, width = 0.5, height = 0.55)+
  draw_plot(p_papers+
              theme(panel.border = element_rect(colour = "grey90", fill = NA, size = 1.5),
                    panel.background = element_rect(fill = "grey90")),
            x = 0.6, y = 0, width = 0.4, height = 0.4)
ggsave(paste0(hndl.out,"Department's title change and staff_v2.jpg"),width = 10,height = 6.5)  


# add logos
do.this=TRUE
if(do.this)
{
  img.list=list('Department of Aborigines and Fisheries' = readPNG(paste0(hndl.in,"Logos/1909.png")),
                'Fisheries Department' = readPNG(paste0(hndl.in,"Logos/1920.png")),
                'Department of Fisheries and Fauna' = readPNG(paste0(hndl.in,"Logos/1964.png")),
                'Department of Fisheries and Wildlife' = readPNG(paste0(hndl.in,"Logos/1974.png")),
                'Fisheries Department' = readPNG(paste0(hndl.in,"Logos/1985.png")),
                'Fisheries Western Australia' = readPNG(paste0(hndl.in,"Logos/1997.png")),
                'Department of Fisheries' = readPNG(paste0(hndl.in,"Logos/2001.png")))
  img.list.location=data.frame(Name=names(img.list),
                               xmin=c(as.Date("1963-01-01"),as.Date("1890-01-01"),as.Date("2010-10-01"),as.Date("1944-01-01"),as.Date("1965-01-01"),as.Date("1972-11-01"),as.Date("1953-01-01")),
                               xmax=c(as.Date("2000-01-01"),as.Date("1914-09-01"),as.Date("2024-01-01"),as.Date("1966-03-08"),as.Date("1980-01-01"),as.Date("1992-01-01"),as.Date("1997-01-30")))%>%
    mutate(ymin = (row_number()+1)*.85,
           ymax = ymin+.75,
           ymin=case_when(Name=='Department of Fisheries and Fauna'~4,
                          Name=='Department of Fisheries and Wildlife'~4.88,
                          Name=='Fisheries Department' & xmin==as.Date("1965-01-01")~5.5,
                          Name=='Fisheries Western Australia'~6.7,
                          Name=='Department of Fisheries'~7.9,
                          TRUE~ymin),
           ymax=case_when(Name=='Fisheries Department'~ymin+1,
                          Name%in%c('Department of Fisheries and Fauna',
                                    'Department of Fisheries and Wildlife')~ymin+1.1,
                          Name=='Fisheries Western Australia'~7.7,
                          Name=='Department of Fisheries'~8.6,
                          TRUE~ymax))
  p1=p+theme(panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank())  
  for(i in 2:length(img.list)) #drop 'Aborigines and Fisheries'
  {
    p1=p1 +
      annotation_raster(img.list[[i]],
                        xmin = img.list.location$xmin[i],
                        xmax = img.list.location$xmax[i],
                        ymin = img.list.location$ymin[i],
                        ymax = img.list.location$ymax[i])
  }
  ggdraw(p1) +
    draw_plot(p_inset, x = 0, y = 0.38, width = 0.5, height = 0.55)+
    draw_plot(p_papers+
                theme(panel.border = element_rect(colour = "grey90", fill = NA, size = 1.5),
                      panel.background = element_rect(fill = "grey90")),
              x = 0.6, y = -0.01, width = 0.4, height = 0.4)
  ggsave(paste0(hndl.out,"Department's title change and staff_v3.jpg"),width = 10,height = 6.5)  
  
}


# recreational sector ---------------------------------------------------------
do.this=FALSE
if(do.this)
{
  Rec=read_csv(paste0(hndl.in,'table - timeline of events for recreational sector.csv'))%>%
    dplyr::select(-Milestone)%>%
    mutate(Year_from=as.Date(paste0(Year_start,'-01-01')),
           Year_to=ifelse(is.na(Year_end),Year_start,Year_end),
           Year_to=as.Date(paste0(Year_to,'-01-01')))
  function.segment.timeline(d=Rec,Start.x="1825-01-01",End.x="2025-01-02")
  ggsave(paste0(hndl.out,"Recreational sector.jpg"),width = 8,height = 6)
  
}
