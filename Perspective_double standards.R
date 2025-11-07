library(tidyverse)
library(cowplot)
library(readxl)
library(data.table)

if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
hndl.out=handl_OneDrive("Scientific manuscripts/Perspective_Double standards/6. Outputs/")
source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/ggplot.themes.R'))

# Infographic - Production, exports and imports --------------------------------------------------------------------

#Bring in data
  #source https://www.agriculture.gov.au/abares/research-topics/fisheries/fisheries-data#australian-fisheries-and-aquaculture-statistics-2022
  #these tables have production, exports and imports but by either commodity or country
  #Tables 13 & 14 have exports by commodity and country, respectively
  #Tables 15 & 16 have imports by commodity and country, respectively
hndl.in=handl_OneDrive("Scientific manuscripts/Perspective_Double standards/1. Data sets/")

  #wild caught fisheries and aquaculture production
Production_table2=read_excel(paste0(hndl.in,'AustFishAquacStats_2022_Tables_v1.0.0.xlsx'), sheet = "Table 2",skip = 7)%>%
                    rename(Commodity="Commodity a")%>%
                    filter(!grepl('State totals include aquaculture but exclude hatchery production',Commodity))%>%
                    filter(!Commodity%in%c("Value","Quantity",
                                           "Finfish, Sharks and Rays","Crustaceans","Molluscs",
                                           "Total","Total valuee","Total quantity e"))%>%
                    mutate(unit=case_when(unit=="$’000"~'thousands AUD',
                                          unit=="t"~'tonnes'),
                           Commodity=case_when(Commodity=="Tunas b"~"Tunas",
                                               Commodity=="Salmonidsc"~"Salmonids",
                                               Commodity=="Other Finfish, Sharks and Rays d"~"Other Finfish, Sharks and Rays",
                                               Commodity=="Oysters g"~"Oysters",
                                               TRUE~Commodity))


  #wild caught fisheries only production
Production_table3=read_excel(paste0(hndl.in,'AustFishAquacStats_2022_Tables_v1.0.0.xlsx'), sheet = "Table 3",skip = 7)%>%
          rename(Commodity="Commodity a")%>%
          filter(!grepl('State and Commonwealth wild-catch production',Commodity))%>%
          filter(!Commodity%in%c("Value","Quantity",
                                 "Finfish","Crustaceans","Molluscs","Total",
                                 "Total wild-caught"))%>%
          mutate(unit=case_when(unit=="$’000"~'thousands AUD',
                                unit=="t"~'tonnes'),
                 Commodity=case_when(Commodity=="Sharks & Rays b"~"Sharks & Rays",
                                     TRUE~Commodity))


  #exports by commodity
Production_table13=read_excel(paste0(hndl.in,'AustFishAquacStats_2022_Tables_v1.0.0.xlsx'), sheet = "Table 13",skip = 7)%>%
        filter(!grepl('Predominantly salmon. Includes trout and',Commodity))%>%
        filter(!Commodity%in%c("Value","Quantity","Edible c","Edible","Non-edible","Edible c",
                               "Finfish, Sharks and Rays","Crustaceans and Molluscs",
                               "Total","Total Finfish, Sharks and Rays","Total Crustaceans and Molluscs",
                               "Total edible fisheries products",
                               'Marine fats and oils','Fish meal','Pearl Oysters b','Ornamental Finfish','Other non–edible',
                               'Total non–edible fisheries products','Total fisheries products',
                               "Other non-edible","Total non-edible fisheries products","Pearl Oysters"))%>%
        mutate(unit=case_when(unit=="$m"~'M AUD',
                              unit=="t"~'tonnes'),
               Commodity=case_when(Commodity%in%c("Salmonids a","Salmonids d")~"Salmonids",
                                   TRUE~Commodity))




  #source https://www.agriculture.gov.au/abares/research-topics/trade/dashboard
hndl.in=handl_OneDrive("Data/Seafood imports and exports/") 
ABARES_trade_data=fread(paste0(hndl.in,'ABARES_trade_data.csv'))%>%
  data.frame

#Plots
fun1=function(d,Grups)
{
  d=d%>%
    gather(Year,Value,-c(Commodity,unit))%>%
    mutate(Commodity=factor(Commodity,levels=unique(d$Commodity)))%>%
    spread(Commodity,Value)%>%
    mutate(Year=as.numeric(substr(Year,1,4)))%>%
    rename(Units=unit)%>%
    data.frame()%>%
    gather(Commodity,Quantity,-c(Year,Units))%>%
    mutate(Quantity=as.character(Quantity),
           Quantity=ifelse(Quantity=="na",NA,Quantity),
           Quantity=as.numeric(Quantity),
           Commodity=tolower(Commodity),
           Group=case_when(grepl(paste(c("tunas","salmonids","swordfish","finfish","sharks","rays",
                                         "fish","salmons","sardine","barramundi","breams","trouts",
                                         "dories","flathead","gemfishes","ling","mullets","roughy",
                                         "mackerel","whitings"),collapse='|'),Commodity)~'Finfish',
                           grepl(paste(c("lobsters","prawn"),collapse='|'),Commodity)~'Rock lobsters & Prawns',
                           grepl(paste(c("abalone","scallops"),collapse='|'),Commodity)~'Abalone & Scallops',
                           grepl(paste(c("crabs","crustaceans","molluscs","octopus",
                                         "pipis","squids","oysters"),collapse='|'),Commodity)~'Other crustaceans & molluscs',
                           TRUE~Commodity),
           Group=factor(Group,levels=Grups))
  return(d)
}
fun2=function(d,KPTN,KLS)
{
  p=d%>%
    ggplot(aes(Year,Quantity,color=Group))+
    geom_line(linewidth=1.5)+facet_wrap(~Units,nrow=1,scales='free')+
    theme_PA()+
    theme(legend.title = element_blank(),
          legend.position = 'top')+
    labs(caption = KPTN)+
    scale_color_manual(values = KLs)+ 
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
  return(p)
}



  #wild caught fisheries and aquaculture
a=fun1(d=Production_table2,
       Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))
p_Production=a%>%
              filter(!Group=="Other")%>%
              group_by(Year,Units,Group)%>%
              summarise(Quantity=sum(Quantity,na.rm=T))%>%
              ungroup()%>%
              mutate(Quantity=ifelse(grepl('thousands',Units),Quantity/1000,Quantity),
                     Units=ifelse(grepl('AUD',Units),'Value (M AUD)',Units),
                     Quantity=ifelse(grepl('tonnes',Units),Quantity/1000,Quantity),
                     Units=ifelse(grepl('tonnes',Units),'Volume (1000s tonnes)',Units))%>%
              ggplot(aes(Year,Quantity,color=Units))+
              geom_line()+facet_wrap(~Group,nrow=1)+
              theme_minimal()+
              theme(legend.position = 'top')+
              labs(caption = 'Fisheries and aquaculture production (source:ABARES)')


  #wild caught fisheries only
a=fun1(d=Production_table3,
       Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))
KLs=c('blue3','chocolate3','cadetblue4','cornsilk3')
names(KLs)=levels(a$Group)
p_Production_wild.caught.only=fun2(d=a%>%
                                     filter(!Group=="Other nei")%>%
                                     group_by(Year,Units,Group)%>%
                                     summarise(Quantity=sum(Quantity,na.rm=T))%>%
                                     ungroup()%>%
                                     mutate(Quantity=ifelse(grepl('thousands',Units),Quantity/1000,Quantity),
                                            Units=ifelse(grepl('AUD',Units),'Value (M AUD)',Units),
                                            Quantity=ifelse(grepl('tonnes',Units),Quantity/1000,Quantity),
                                            Units=ifelse(grepl('tonnes',Units),'Volume (1000s tonnes)',Units)),
                                   KPTN='Wild-caught fisheries production (source:ABARES)',
                                   KLS=KLS)

  #exports by commodity  
a=fun1(d=Production_table13,
       Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))%>%
  filter(!Group=="Other")
p_exports_commodity=fun2(d=a%>%
                           group_by(Year,Units,Group)%>%
                           summarise(Quantity=sum(Quantity,na.rm=T))%>%
                           ungroup()%>%
                           mutate(Quantity=ifelse(grepl('thousands',Units),Quantity/1000,Quantity),
                                  Units=ifelse(grepl('AUD',Units),'Value (M AUD)',Units),
                                  Quantity=ifelse(grepl('tonnes',Units),Quantity/1000,Quantity),
                                  Units=ifelse(grepl('tonnes',Units),'Volume (1000s tonnes)',Units)),
                         KPTN='Edible exports (source:ABARES)',
                         KLS=KLS)


  #combine relevant plots
  ggsave(paste0(hndl.out,"Infographic_Production.jpg"),width = 10,height = 6.5) 
  
  
  
# Infographic - Shark production, exports and imports --------------------------------------------------------------------

  
# Infographic - Regulations --------------------------------------------------------------------
  