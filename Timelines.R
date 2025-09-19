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
                                   add.point=TRUE,lbl.size=9,axis.leg.size=16)
{
  if('Topic'%in%names(d))
  {
    p=d%>%
      mutate(y=row_number())%>%
      ggplot(aes(Year_from,y,color=Topic))+
      geom_segment(aes(xend=Year_to,yend=y,color=Topic),linewidth = 1.2*pt.size)
  }else
  {
    p=d%>%
      mutate(y=row_number())%>%
      ggplot(aes(Year_from,y))+
      geom_segment(aes(xend=Year_to,yend=y),linewidth = 1.2*pt.size)
  }
  if(add.point) p=p+geom_point(size=pt.size,shape=15)
  p=p+
    scale_x_date(name = "", date_breaks = "20 years", date_labels = "%Y",
                 limits=c(as.Date(Start.x),as.Date(End.x))) +
    scale_y_discrete(name = "") +
    theme_minimal() +
    theme(legend.position = "top")
  if(!is.null(lbl.size))
  {
    if(!repel.free)
    {
      p=p+
        geom_text(aes(x=Year_from,y=y,label=Label),size=lbl.size,show.legend = FALSE, vjust = -1,hjust=0.1)
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
                                                 Arrow.width,Kls=colfunc(8),Max.ovrlp)
{
  #base map
  p=d%>%
    ggplot(aes(Year,Total))+
    geom_line(linewidth=ln.width,color=Kls[7],arrow = arrow(type = "open",length=unit(Arrow.width, "npc")))+
    labs(caption = 'Source: ABS Historical Population Statistics')+
    scale_x_date(name = "", date_breaks = "20 years", date_labels = "%Y",
                 limits=c(as.Date(paste0(start.year-6,'-01-01')),as.Date(paste0(end.year,'-01-01')))) +
    scale_y_continuous(name = 'Population size of Western Australia',labels = label_number(scale = 1e-6, suffix = "M"))+
    theme_minimal() +
    theme(legend.position = "top",
          axis.title=element_text(size=14),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
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
              aes(Year,Total),linewidth=ln.width,color=Kls[7])
  
  #add labels
  timeline.labls=Labls%>%
    rename(x=Year_from)%>%
    dplyr::select(x,Label)
  timeline.labls$y=d$Total[match(timeline.labls$x,d$Year)]
  
  if(text.labelling=='repel')
  {
    timeline.labls=timeline.labls%>%mutate(Label=str_wrap(Label,width=lbl.width))
    p=p+
      geom_text_repel(data=timeline.labls,aes(x,y,label = Label),
                      show.legend = FALSE,max.overlaps = Max.ovrlp,min.segment.length = 0,
                      nudge_y=Nudge_y,nudge_x = Nudge_x, point.padding = Point.padding,
                      box.padding = Box.padding,force=Force,seed=666,size=lbl.size)
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
Chronology=read.csv(paste0(hndl.in,'table - timeline chronology.csv'))

Chronology=Chronology%>%
  mutate(Year_start1=ifelse(!is.na(Year_end),(Year_end+Year_start)/2,Year_start),
         Year_from=as.Date(paste0(round(Year_start1),'-01-01')))%>%
  arrange(Year_from)%>%
  dplyr::select(-c(Year_end,Missing))

# Chronology=Chronology%>%
#   mutate(type=ifelse(is.na(Year_end),'dot','bar'),
#          Year_start1=ifelse(!is.na(Year_end),(Year_end+Year_start)/2,Year_start),
#          Year_from=as.Date(paste0(Year_start1,'-01-01')),
#          Year_to=ifelse(is.na(Year_end), Year_start1,Year_end),
#          Year_to=as.Date(paste0(Year_to,'-01-01')))%>%
#   arrange(Year_from)


#Inset - Add shape of main commercial species proportional to catch
#note: relative importance from FISHCUBE query of total catch ever reported
#http://f01-fims-webp01/FishCubeWA/Query.aspx?CubeId=CommercialDPIRDOnly&QueryId=85c1860f-7a95-4d63-b195-7266b119c8a1
get.fishcube=FALSE
if(get.fishcube)
{
  FishCubeWA=read_excel(paste0(hndl.in,'Fish Cube WA - Data Extract -Catch totals all spp.xlsx'), sheet = "Data",skip = 0)
  FishCubeWA=FishCubeWA%>%
    rename(Group='Species Group',
           Species='Species CAAB Code',
           Name='Species Common Name',
           Tonnes='Weight (Tonnes)')%>%
    filter(!Name%in%c('Grand Total','Nil Fish Caught'))%>%
    filter(!Species%in%c('Grand Total','Nil Fish Caught'))%>%
    mutate(Group1=case_when(grepl('Lobster',Name)~'Lobster',
                            grepl('Crab',Name)~'Crab',
                            grepl('Abalone',Name)~'Abalone',
                            grepl('Scallop',Name)~'Scallop',
                            TRUE~Group))
  
  Top.catch=FishCubeWA%>%
    group_by(Group1)%>%
    summarise(Tonnes=sum(Tonnes))%>%
    arrange(-Tonnes)
  
}
Species.list=data.frame(Common=c('Rocklobster','Prawn','Scallop','Abalone','Echinoderms',
                                 'Crab','Cephalopods', 'Scalefish','Shark'),
                        Scientific=c('Panulirus','Penaeus','Pecten','Haliotis','Holothuria',
                                     'portunus diacantha','Octopodinae','chrysophrys aculeata','Carcharhinus'),
                        Value=c(453586,157682,149822,13253,3672,
                                29589,14588,625531,67536))
Sp.shape=lapply(Species.list$Scientific, function(x) get_phylopic(get_uuid(name = x), format = "raster"))

Skaler=4
Species.list=Species.list%>%
  mutate(Value.rel=log(Value),
         Value.rel=Value.rel/max(Value.rel),
         Value.rel=Value.rel*Skaler)

    #basemap
SP.kol='brown4'
Australia <- ne_states(country = "Australia", returnclass = "sf")
Limy=c(-44,-10)
Limx=c(108,154)
p_inset=ggplot(data = Australia) +
  geom_sf(color = "black", fill = "grey80") +
  xlab("") + ylab("")+
  coord_sf(xlim =Limx , ylim = Limy, expand = T)+
  theme_void()+labs(subtitle="Harvested resourced")+
  theme(plot.subtitle = element_text(size=11,vjust = -3,color=SP.kol))

    #add ellipse
p_inset=p_inset+
  stat_ellipse(data=data.frame(x=c(113,127,113,127), y=c(-34,-34,-18,-18)),
               aes(x,y),level = 0.70,linewidth = 1.25,color=SP.kol)

    #add species
Species.list=Species.list%>%
  mutate(x=c(113,114,111,121,122,116.5,111.5,118,128),
         y=c(-32,-21,-24,-36,-13,-36,-28,-17,-34.5),
         x.end=c(114.5,116,114.25,121,122,117,113.5,118,128),
         y.end=c(-31,-23,-24,-34,-14,-34.5 ,-28,-18.5,-32.5))
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
p_inset
#Main plot
p=function.chronology.pop.growth.timeline(d=WA.population,
                                          Labls=Chronology,
                                          lbl.width=60,
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
                                          Kls=colfunc(8),
                                          Max.ovrlp=100) #Max.ovrlp=Inf

# #ADD logos to bottom
use.this=FALSE
if(use.this)
{
  img.list=list('1920' = readPNG("Logos/1920.png"),
                '1964' = readPNG("Logos/1964.png"),
                '1974' = readPNG("Logos/1974.png"),
                '1997' = readPNG("Logos/1997.png"),
                '2001' = readPNG("Logos/2001.png"))
  p_inset2=ggplot(data=data.frame(y=0:8,x=0:8),aes(x,y))+
    geom_point(size=3,color='transparent')+
    xlim(1,8)+ylim(0,1.15)+theme_void()
  img.loc=data.frame(Lbl=names(img.list))%>%
    mutate(xmin=row_number(),
           xmax=ifelse(Lbl==2001,xmin+3,xmin+1))
  for(i in 1:length(img.list))
  {
    p_inset2=p_inset2+
      annotation_raster(img.list[[i]], xmin = img.loc$xmin[i]*.9, xmax = img.loc$xmax[i], ymin = 0, ymax = 1)+
      geom_text(x=mean(c(img.loc$xmin[i],img.loc$xmax[i]*.9)),y=1,label=names(img.list)[i],size=3)
  }
 # +    draw_plot(p_inset2, x = 1.1, y = 0, width = 0.6, height = 0.05)
}

#Combine all
ggdraw(p) + draw_plot(p_inset, x = 0.1, y = 0.525, width = 0.4, height = 0.5)
ggsave(paste0(hndl.out,"Chronology_pop.growth.jpg"),width = 8,height = 6)




# Chronology Andrew basic-----------------------------------------------------------
function.chronology.basic.timeline(d=Chronology,
                             Start.x=as.Date("1900-01-01"),End.x=as.Date("2025-01-01"))
ggsave(paste0(hndl.out,"Chronology_basic.jpg"),width = 6,height = 8)



# Stock assessment evolution -----------------------------------------------------------
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


# Department's title change and staff ---------------------------------------------------------
#Sourced by Paul Orange
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
department.staff=data.frame(date.from=as.Date(c("1893-01-01","1898-01-01","1938-01-01",
                                                "1953-01-01","1968-01-01","1984-01-01",
                                                "1990-01-01","2000-01-01","2017-01-01")),
                            date.to=as.Date(c("1898-01-01","1938-01-01","1953-01-01",
                                               "1968-01-01","1984-01-01","1990-01-01",
                                               "2000-01-01","2017-01-01","2025-09-17")),
                            No.of.staff=c(4,8,10,35,
                                          100,200,207,
                                          388,466))
#create timeline
p=function.segment.timeline(d=department.title%>%
                              mutate(Topic=Name)%>%
                            rename(Year_from=Commenced,
                                   Year_to=Name.change,
                                   Label=Name),
                            Start.x="1890-01-01",End.x="2025-12-01",
                            Show.Leg=FALSE,repel.free=FALSE,
                            pt.size=4,add.point=FALSE,lbl.size=5,axis.leg.size=16)
Name.cols=c('navyblue','slategray','skyblue3','royalblue','turquoise3','steelblue','tomato')
names(Name.cols)=c("Fisheries Department","Department of Aborigines and Fisheries",
                   "Department of Fisheries and Fauna","Department of Fisheries and Wildlife",
                   "Fisheries Western Australia","Department of Fisheries","DPIRD")
p=p+
  scale_color_manual(values = Name.cols)+
  labs(caption = 'DPIRD, Department of Primary Industries and Regional Development')

#create inset
p_inset=function.segment.timeline(d=department.staff%>%
                                    mutate(Label=paste(No.of.staff,'staff'),
                                           lbl.size=No.of.staff^0.1)%>%
                                    rename(Year_from=date.from,
                                           Year_to=date.to),
                                  Start.x="1890-01-01",End.x="2025-12-01",
                                  Show.Leg=FALSE,repel.free=FALSE,
                                  pt.size=3,add.point=FALSE,lbl.size=3,axis.leg.size=8)+
    theme(panel.border = element_rect(colour = "grey90", fill = NA, size = 1.5),
          panel.background = element_rect(fill = "grey90"))

# add logos
do.this=FALSE
if(do.this)
{
  img <- readPNG("Logos/1909.png")
  p +
    annotation_raster(img, xmin = as.Date("1903-01-01"), xmax = as.Date("1905-01-01"), ymin = 1, ymax = 1.2)
  
}

ggdraw(p) +
  draw_plot(p_inset, x = 0, y = 0.45, width = 0.55, height = 0.55)

ggsave(paste0(hndl.out,"Department's title change and staff.jpg"),width = 10,height = 6)  


# recreational sector ---------------------------------------------------------
Rec=read_csv(paste0(hndl.in,'table - timeline of events for recreational sector.csv'))%>%
  dplyr::select(-Milestone)%>%
  mutate(Year_from=as.Date(paste0(Year_start,'-01-01')),
         Year_to=ifelse(is.na(Year_end),Year_start,Year_end),
         Year_to=as.Date(paste0(Year_to,'-01-01')))
function.segment.timeline(d=Rec,Start.x="1825-01-01",End.x="2025-01-02")
ggsave(paste0(hndl.out,"Recreational sector.jpg"),width = 8,height = 6)
