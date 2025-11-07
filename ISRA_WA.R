if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
do.observer=TRUE
if(do.observer)
{
  source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/Source_Shark_bio.R"))
}
library(tidyverse)
library(ggpubr)
library(rnaturalearth)
library(rgdal)
library(sf)
library(readxl)
library(ggrepel)
library(flextable)
library(officer)
library(sf)
library(mapview)
library(Hmisc)

LH.data=read.csv(handl_OneDrive('Data/Life history parameters/Life_History.csv'))

source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/ggplot.themes.R'))  #my themes
hndl.out=handl_OneDrive("ISRA/Australia/WA/")

do.preliminary=FALSE  #do preliminary stuff
do.observer=FALSE
# Data-------------------------------------------------------------------------
grids=read.csv(paste0(hndl.out,"WA_10nm.all.grids_all.spp.csv"))
grids_24_spp=read.csv(paste0(hndl.out,"WA_10nm.grid_all.spp.24.csv"))
all_spp=read.csv(handl_OneDrive("ISRA/Australia/all_spp.csv"))%>%
                  rename(Depth_min=Emiliano_Min_depth,
                         Depth_max=Emiliano_Max_depth)%>%
                  mutate(Common.name=case_when(Common.name=='Purple eagle eay'~'Purple eagle ray',
                                               TRUE~Common.name))
Temporal.WA=read.csv(handl_OneDrive("ISRA/Australia/WA/temporal data/All_grids_WA.csv"))%>%
  rename(Scientific.name=Scientific.name.cleaned)%>%
  mutate(Buffer=ifelse(grepl('buffer',ISRA),'buffer',ISRA),
         ISRA=str_remove(ISRA,'_buffer'),
         ISRA=ifelse(ISRA=="Walcott Inlet","Walcott Inlet and Secure Bay",ISRA))

Temporal.WA_all=Temporal.WA%>%
                  mutate(lat=as.numeric(sub(".*?_", "", str_remove(grid_id,'grid_'))),
                         lon=as.numeric(sub("_.*", "",str_remove(grid_id,'grid_'))))%>%
  dplyr::select(-grid_id)

Temporal.WA_Karissa=Temporal.WA%>%
                      filter(Buffer%in%c('Ningaloo','Shark Bay'))%>%  
  left_join(all_spp%>%dplyr::select(Scientific.name,Common.name),
            by='Scientific.name')%>%
  dplyr::select(-Buffer)%>%
  relocate(Common.name)
  write.csv(Temporal.WA_Karissa,paste0(hndl.out,'Outputs/Temporal.WA_Karissa.csv'),row.names = F)


squares_vertices=read.csv(paste0(hndl.out,'Outputs/Maps/squares_vertices.csv'))  

Effort_WA=read.csv(paste0(hndl.out,'Effort_WA.csv'))
  
#Al's Walcott
if.check.sites=FALSE
if(if.check.sites)
{
  biol.map=st_read(paste0(hndl.out,'Walcott_Al/biol.gpkg'))
  mapview(biol.map)
  
  isra_walcott_secure.map=st_read(paste0(hndl.out,'Walcott_Al/isra-walcott-secure.gpkg'))
  mapview(isra_walcott_secure.map)
  
}
isra_walcott_secure=read.csv(paste0(hndl.out,'Walcott_Al/isra-walcott-secure.csv'))%>%
  dplyr::select(common,species_long,start_date,lat,lon,FL,TL,sex)%>%
  mutate(TL=TL/10,FL=FL/10)
biol=readRDS(paste0(hndl.out,'Walcott_Al/biol.rds'))%>%
  data.frame%>%
  mutate(lat=-abs(lat))%>%
  dplyr::select(common,species_long,start_date,lat,lon,FL,TL,sex)%>%
  mutate(TL=TL/10,FL=FL/10)

Al_all=rbind(biol,isra_walcott_secure)

isra_walcott_secure=isra_walcott_secure%>%
  filter(lat>=-16.65 & lat<=-16.293)%>%
  filter(lon>=124.16 & lon<=125.04)
biol=biol%>%
  filter(lat>=-16.65 & lat<=-16.293)%>%
  filter(lon>=124.16 & lon<=125.04)
Walcott.Al=rbind(biol,isra_walcott_secure)   
if(if.check.sites)
{
  Walcott.Al%>%
    ggplot(aes(lon,lat))+
    geom_point()+ylim(-17.17,-15.44)+xlim(123.72,125.72)
}

#NSW
hndl.out.NSW=handl_OneDrive("ISRA/Australia/NSW/")
All_grids_NSW=read.csv(paste0(hndl.out.NSW,"All_grids_NSW.csv")) 

#Gulf of Carpentaria
hndl.out.Carp=handl_OneDrive("ISRA/Australia/Gulf Carpentaria/")
All_grids_Carpentaria=read.csv(paste0(hndl.out.Carp,"All_grids_Carpentaria.csv")) 

#cISRAs
cISRAs=read_excel(paste0(hndl.out,"Outputs/Criteria matrix.xlsx"),sheet = "Matrix")%>%
          filter(Status=='Active')

Greynurse.caves=read_excel(paste0(hndl.out,"Outputs/Criteria matrix.xlsx"),sheet = "Greynurse aggregations")%>%
  dplyr::select(ISRA,Actual_lat.dec,Actual_lon.dec)%>%
  rename(lat=Actual_lat.dec,
         lon=Actual_lon.dec)%>%
  mutate(ISRA=str_remove(ISRA,"Grey Nurse aggregation "))


Temporal.WA=Temporal.WA%>%
            filter(ISRA%in%unique(cISRAs$ISRA))


#WA Bathymetry
Map.hndl=handl_OneDrive("Data/Mapping/")
Bathymetry_120=read.table(paste0(Map.hndl,"get_data112_120.cgi"))
Bathymetry_138=read.table(paste0(Map.hndl,"get_data120.05_138.cgi")) 
Bathymetry=rbind(Bathymetry_120,Bathymetry_138)


# Update Walcott.Al  ------------------------------------------
Temporal.WA.Walcott=Walcott.Al%>%
  mutate(Year=year(start_date),
         Scientific.name=species_long)%>%
  group_by(Scientific.name,Year)%>%
  tally()%>%
  ungroup()%>%
  rename(n_obs=n)%>%
  mutate(grid_id='grid_124.3333366_-15.5000011',
         ISRA='Walcott Inlet and Secure Bay',
         Buffer='isra',
         Data.type='Surveys')%>%
  relocate(Scientific.name,Year,grid_id,ISRA,Data.type,n_obs,Buffer)

Temporal.WA=Temporal.WA%>%
                filter(!ISRA=='Walcott Inlet and Secure Bay')
  
Temporal.WA=rbind(Temporal.WA,Temporal.WA.Walcott)

# Filter out occurrence outliers  ------------------------------------------

#Not in WA
Not.in.WA=c('Cirrhigaleus australis','Urolophus bucculentus','Urolophus viridis',
            'Dentiraja confusa','Etmopterus granulosus','Dentiraja lemprieri',
            'Trygonoptera testacea','Trygonorrhina fasciata','Orectolobus ornatus',
            'Neotrygon trigonoides','Asymbolus analis','Asymbolus rubiginosus',
            'Centrophorus squamosus','Urolophus cruciatus','Brachaelurus waddi')

grids_24_spp=grids_24_spp%>%
              mutate(drop=case_when(Scientific.name=='Galeorhinus galeus' & max_lat>(-30)~'Yes',
                                    Scientific.name=='Mustelus antarcticus' & max_lat>(-27)~'Yes',
                                    Scientific.name%in% Not.in.WA ~'Yes',
                                    Scientific.name=='Mobula birostris' & max_lat<=(-26)~'Yes',
                                    Scientific.name=='Sphyrna lewini' & max_lat<=(-32)~'Yes',
                                    
                                    TRUE~'No'))%>%
              filter(drop=='No')%>%
              dplyr::select(-drop)

grids=grids%>%
      mutate(drop=case_when(Scientific.name=='Galeorhinus galeus' & max_lat>(-30)~'Yes',
                            Scientific.name=='Mustelus antarcticus' & max_lat>(-27)~'Yes',
                            Scientific.name%in% Not.in.WA ~'Yes',
                            Scientific.name=='Mobula birostris' & max_lat<=(-26)~'Yes',
                            Scientific.name=='Sphyrna lewini' & max_lat<=(-32)~'Yes',
                            TRUE~'No'))%>%
      filter(drop=='No')%>%
      dplyr::select(-drop)



# Compare Global and Australian IUCN for WA-------------------------------------------------------------------------
IUCN.colors=c("DD"="grey85","LC"="#60C659","NT"="#CCE226",
              "VU"="#F9E814","EN"="#FC7F3F","CR"="#D81E05")
IUCN.Endangered.categories=c("Vulnerable","Endangered","Critically endangered")

Different.global.Australia.IUCN=all_spp%>%
  distinct(Common.name, IUCN.global.status, IUCN.aus.status,in_WA,Cri_A_aus, Cri_A_global)%>%
  filter(!IUCN.global.status==IUCN.aus.status)


Different.global.Australia.IUCN.WA=Different.global.Australia.IUCN%>%
  filter(in_WA=='yes')%>%
  arrange(Cri_A_aus)

d=Different.global.Australia.IUCN.WA%>%
  dplyr::select(Common.name,IUCN.global.status,IUCN.aus.status)%>%
  gather(Assessment,Status,-Common.name)%>%
  mutate(Assessment=case_when(Assessment=="IUCN.global.status"~'Global',
                              Assessment=="IUCN.aus.status"~'Australia'))%>%
  filter(!Status=="")%>%
  mutate(Status=case_when(
                          Status=="Critically Endangered"~"CR",
                          Status=="Endangered"~"EN",
                          Status=="Vulnerable"~"VU",
                          Status=="Near Threatened"~"NT",
                          Status=="Least Concern"~"LC",
                          Status=="Data Deficient"~"DD"),
          Status=factor(Status,levels=names(IUCN.colors)))


d%>%
  ggplot(aes(Common.name,fill=Status))+
  geom_bar()+
  facet_wrap(~Assessment,ncol=2,nrow=1)+
  theme_PA(leg.siz=9)+
  theme(legend.position = 'top',
        legend.title=element_blank(),
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  coord_flip() +
  scale_fill_manual(values=IUCN.colors,drop=FALSE)+
  guides(fill = guide_legend(nrow = 1))
ggsave(paste0(hndl.out,'Outputs/IUCN differences_Global vs Australia.tiff'), width = 6,height = 10, dpi = 300, compression = "lzw")


# Criteria D.2 - Diversity-------------------------------------------------------------------------
#Extract 10 nm grids meeting criteria D.2, & A/B
Criteria.d2=function(Grids,jurisdiction,keep.vars,SIZE=3)
{
  d=Grids%>%
    left_join(all_spp%>%dplyr::select(!!c(paste0('in_',jurisdiction),keep.vars)),
              by='Scientific.name')%>%
    mutate(mid.lat=(min_lat+max_lat)/2,
           mid.long=(min_lon+max_lon)/2)
  
  dummy=d%>%distinct(grid_id,mid.long,mid.lat)
  
  #plot grid locations
  Total.species=d%>%
    group_by(grid_id,mid.long,mid.lat)%>%
    tally()%>%
    rename(N.sp=n)%>%
    ungroup()
  
  pD.2=Total.species%>%
    ggplot(aes(mid.long,mid.lat,color=N.sp))+
    geom_point(size=SIZE)+
    theme_PA(Ttl.siz=12)+
    ggtitle('All grids with >=24 species')

  
  #calculate number of Criteria A species per grid
  Criteria.A=d%>%
              filter(Cri_A_aus=='yes')%>%
              dplyr::select(grid_id,Common.name)%>%
              mutate(N=1)%>% 
              arrange(grid_id, Common.name) %>% 
              group_by(grid_id) %>%
              summarise(N.sp_A=sum(N),
                        Common.name_A=str_c(Common.name, collapse=", "))

  pA=Criteria.A%>%
    left_join(dummy,by='grid_id')%>%
    mutate(N.sp=N.sp_A)%>%
    ggplot(aes(mid.long,mid.lat,color=N.sp))+
    geom_point(data=dummy%>%mutate(N.sp=min(Criteria.A$N.sp_A)),
               aes(mid.long,mid.lat,color=N.sp),size=SIZE,color='grey60',shape=21)+
    geom_point(size=SIZE)+
    theme_PA(Ttl.siz=12)+
    ggtitle('Grids with >=24 species & species meeting criteria A')+
    ylim(range(d$mid.lat))+xlim(range(d$mid.long))
  
  
  #calculate number of Criteria B species per grid
  Criteria.B=d%>%
    filter(Cri_B=='yes')%>%
    dplyr::select(grid_id,Common.name)%>%
    mutate(N=1)%>% 
    arrange(grid_id, Common.name) %>% 
    group_by(grid_id) %>%
    summarise(N.sp_B=sum(N),
              Common.name_B=str_c(Common.name, collapse=", ")) 
  
  pB=Criteria.B%>%
    left_join(dummy,by='grid_id')%>%
    rename(N.sp=N.sp_B)%>%
    ggplot(aes(mid.long,mid.lat,color=N.sp))+
    geom_point(data=dummy%>%mutate(N.sp=min(Criteria.B$N.sp_B)),
               aes(mid.long,mid.lat,color=N.sp),size=SIZE,color='grey60',shape=21)+
    geom_point(size=SIZE)+
    theme_PA(Ttl.siz=12)+
    ggtitle('Grids with >=24 species & species meeting criteria B')+
    ylim(range(d$mid.lat))+xlim(range(d$mid.long))
  
  #Combine Criteria A & B 
  Criteria.A_B=full_join(Criteria.A,Criteria.B,by='grid_id')%>%
                  mutate(N.sp_A=ifelse(is.na(N.sp_A),0,N.sp_A),
                         Common.name_A=ifelse(is.na(Common.name_A),0,Common.name_A),
                         N.sp_B=ifelse(is.na(N.sp_B),0,N.sp_B),
                         Common.name_B=ifelse(is.na(Common.name_B),0,Common.name_B))
  Criteria.A_B=Criteria.A_B%>%
                left_join(Total.species%>%dplyr::select(grid_id,N.sp)%>%rename(Total.sp=N.sp),
                          by='grid_id')%>%
    relocate(Total.sp,.before=N.sp_A)
  
  return(list(d=d,Criteria.A=Criteria.A,Criteria.B=Criteria.B,Criteria.A_B=Criteria.A_B,
              pD.2=pD.2,pA=pA,pB=pB))
}
Out=Criteria.d2(Grids=grids_24_spp,
            jurisdiction='WA',
            keep.vars=c('CAAB.code','Scientific.name','Common.name','Order','Family','Genus','Group','Endemic','Habitat',
                        'IUCN.aus.status','Cri_A_aus','Cri_B','Cri_B_LMEs','Depth_min','Depth_max'))

ggarrange(Out$pD.2,Out$pA,Out$pB,ncol=1,nrow=3)
ggsave(paste0(hndl.out,'Outputs/Criteria D.2 & A & B.tiff'), width = 6,height = 10, dpi = 300, compression = "lzw")

write.csv(Out$Criteria.A_B,paste0(hndl.out,'Outputs/Criteria D.2 & A & B.csv'),row.names = F)

#Consolidate grids into cISRAS
Consolidated.grids=Out$Criteria.A_B%>%
  mutate(Long.centroid=as.numeric(sub("_.*", "",str_remove(grid_id,'grid_'))),
         Lat.centroid= as.numeric(sub(".*?_", "", str_remove(grid_id,'grid_'))),
         ISRA.name=case_when(Lat.centroid <(-32) & Long.centroid >125~ 'South.west.slope',
                             Lat.centroid>(-32.5) & Lat.centroid <(-31.75) & Long.centroid <116.5~ 'Perth.CS',
                             Lat.centroid>(-23.5) & Lat.centroid <(-21) & Long.centroid <116.5~ 'Ningaloo.Sandbar.Dusky',
                             .default = ''))
Consolidated.grids%>%
  ggplot(aes(Long.centroid,Lat.centroid,color=ISRA.name))+
  geom_point()


Criteria.d2.consolidate=function(d1,Grids,jurisdiction,keep.vars,SIZE=3)
{
  d=Grids%>%
    left_join(all_spp%>%dplyr::select(!!c(paste0('in_',jurisdiction),keep.vars)),
              by='Scientific.name')%>%
    mutate(mid.lat=(min_lat+max_lat)/2,
           mid.long=(min_lon+max_lon)/2)
  d=d%>%
    filter(grid_id%in%unique(d1$grid_id))%>%
    left_join(d1,by='grid_id')
  d_range=d%>%
    group_by(ISRA.name)%>%
    summarise(min_lat=min(min_lat),
              max_lat=max(max_lat),
              min_lon=min(min_lon),
              max_lon=max(max_lon))
  
  d_sp=d%>%
    distinct(ISRA.name,Scientific.name,CAAB.code,Common.name,
             Group,Endemic,Habitat,IUCN.aus.status,Cri_A_aus,Cri_B,Cri_B_LMEs,Depth_min,
             Depth_max)
  Species.by.ISRA=d_sp%>%
                    group_by(ISRA.name)%>%
                    distinct(Common.name)
  Total.number.species.by.ISRA=Species.by.ISRA%>%
                                group_by(ISRA.name)%>%
                                tally()%>%
                                rename(N.sp_total=n)
  
  Criteria.A.species.by.ISRA=d_sp%>%
                                filter(Cri_A_aus=='yes')%>%
                                group_by(ISRA.name)%>%
                                distinct(Common.name)
  Total.number.criteria.A.species.by.ISRA=Criteria.A.species.by.ISRA%>%
                                group_by(ISRA.name)%>%
                                tally()%>%
                                rename(N.sp_A=n)
  
  Criteria.B.species.by.ISRA=d_sp%>%
                              filter(Cri_B=='yes')%>%
                              group_by(ISRA.name)%>%
                              distinct(Common.name)
  Total.number.criteria.B.species.by.ISRA=Criteria.B.species.by.ISRA%>%
                              group_by(ISRA.name)%>%
                              tally()%>%
                              rename(N.sp_B=n)

  
  #Combine all info
  cISRAs=d_range%>%
          full_join(Total.number.species.by.ISRA,by='ISRA.name')%>%
          full_join(Criteria.A.species.by.ISRA%>%
                      group_by(ISRA.name)%>%
                      summarise(Common.name_A=str_c(Common.name, collapse=", ")),by='ISRA.name')%>%
          full_join(Total.number.criteria.A.species.by.ISRA,by='ISRA.name')%>%
          full_join(Criteria.B.species.by.ISRA%>%
                      group_by(ISRA.name)%>%
                      summarise(Common.name_B=str_c(Common.name, collapse=", ")),by='ISRA.name')%>%
          full_join(Total.number.criteria.B.species.by.ISRA,by='ISRA.name')
  
  
  cISRAs.SP=d_sp%>%
                distinct(Scientific.name,Common.name,CAAB.code,Group,Endemic,Habitat,
                         Cri_A_aus,Cri_B,Cri_B_LMEs,Depth_min,Depth_max)%>%
    filter(Common.name%in% unique(c(Criteria.A.species.by.ISRA$Common.name,Criteria.B.species.by.ISRA$Common.name)))
  
  
  
  
  return(list(cISRAs=cISRAs,cISRAs.SP=cISRAs.SP))
}
out.cons=Criteria.d2.consolidate(d1=Consolidated.grids%>%filter(!ISRA.name=='')%>%dplyr::select(grid_id,ISRA.name),
                                 Grids=grids_24_spp,
                                 jurisdiction='WA',
                                 keep.vars=c('CAAB.code','Scientific.name','Common.name','Order','Family','Genus','Group','Endemic','Habitat',
                                              'IUCN.aus.status','Cri_A_aus','Cri_B','Cri_B_LMEs','Depth_min','Depth_max'))
write.csv(out.cons$cISRAs,paste0(hndl.out,'Outputs/Criteria D.2 & A & B_consolidated.csv'),row.names = F)
write.csv(out.cons$cISRAs.SP,paste0(hndl.out,'Outputs/Criteria D.2 & A & B_consolidated_species.csv'),row.names = F)



# WA map with cISRAs-------------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
fn.map=function(Limx, Limy, Depth.data,add.depth,add.parks,FishClose.col,ASL.col,Comm.col,State.col,alpha.parks)
{
  p=ggplot(data = world) +
    geom_sf(color = "black", fill = "grey60",alpha=0.4) +
    xlab("") + ylab("")+
    scale_x_continuous(breaks=seq(round(Limx)[1],round(Limx)[2]))+
    scale_y_continuous(breaks=seq(round(Limy)[1],round(Limy)[2]))+
    ylab('Latitude')+xlab('Longitude')+
    theme_PA(leg.siz=10)+
    theme(axis.title = element_text(size=15),
          axis.text = element_text(size=11),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  if(add.depth)
  {
    p=p+
      geom_contour(data = Depth.data%>%filter(V1>=Limx[1] & V1<=Limx[2] & V2>=Limy[1] & V2<=Limy[2]), 
                   aes(x=V1, y=V2, z=V3),
                   breaks=c(-50,-100,-200,-500),linetype="solid",colour="grey70")
  }
  
  if(add.parks)
  {
    p=p+
      geom_sf(data = Shark_Fishery_Closures,fill=FishClose.col,alpha=alpha.parks)+
      geom_sf(data = ASL_Closures,fill=ASL.col,alpha=alpha.parks)+
      geom_sf(data = ASL_Closures2,fill=ASL.col,alpha=alpha.parks)+
      geom_sf(data = Park_WA_Commonwealth_Marine_Parks,fill=Comm.col,alpha=alpha.parks)+
      geom_sf(data = Park_DBCA_SCMP,fill=State.col,alpha=alpha.parks)+
      geom_sf(data = Park_Eighty.Mile,fill=State.col,alpha=alpha.parks)+
      geom_sf(data = Park_Horizontal.Falls.and.North.Lalang,fill=State.col,alpha=alpha.parks)+
      geom_sf(data = Park_Lalang.Garram.Camden.Sound,fill=State.col,alpha=alpha.parks)+
      geom_sf(data = Park_Northern.Kimberley,fill=State.col,alpha=alpha.parks)+
      geom_sf(data = Park_Yawuru.Roebuck,fill=State.col,alpha=alpha.parks)+
      coord_sf(xlim =Limx , ylim = Limy, expand = T)
    leg.dat=data.frame(x=seq(Limx[2]*.9,Limx[2],length.out=4),
                       y=seq(Limy[2]*.9,Limy[2],length.out=4),
                       Name=c('Fishery closure','ASL closure','Commonwealth park','State park'))
    leg.col=c(FishClose.col,ASL.col,Comm.col,State.col)
    names(leg.col)=c('Fishery closure','ASL closure','Commonwealth park','State park')
    p=p+
      geom_point(data=leg.dat,aes(x=x,y=y,color=Name))+
      theme(legend.title = element_blank(),
            legend.position = 'bottom')+
      scale_color_manual(values=leg.col)+
      guides(color = guide_legend(override.aes = list(alpha = alpha.parks,size = 5)))
    
    
  }
  p=p+coord_sf(xlim =Limx , ylim = Limy, expand = T)
  return(p)
}

if(do.preliminary)
{
  #Parks and closures 
  Shark_Fishery_Closures=st_read(paste0(Map.hndl,"Closures/Shark_Fishery_Closures/Shark_Fishery_Closures.shp"))
  #ASL_Closures=readOGR(paste0(Map.hndl,"Closures/ASL_closures/ASL_Closures.shp"), layer="ASL_Closures")
  ASL_Closures = st_read(paste0(Map.hndl,"Closures/ASL_closures/ASL_Closures.shp"))
  ASL_Closures2 = st_read(paste0(Map.hndl,"Closures/ASL_closures/Prohibition_on_Fishing_WCDGDLIMF_Order_2018_DPIRD_088.shp"))
  #Australian.marine.parks=st_read(paste0(Map.hndl,"Australian marine parks/Australian marine parks.shp"))
  Park_DBCA_SCMP=st_read(paste0(Map.hndl,"park boundaries/DBCA_SCMP/pmcr-scmp-sanctuary-hwm-proposed-sco-20240111.shp"))
  Park_Eighty.Mile=st_read(paste0(Map.hndl,"park boundaries/Eighty Mile/pmcr-embmp-zoning-hwm_kim_20140404.shp"))
  Park_Horizontal.Falls.and.North.Lalang=st_read(paste0(Map.hndl,"park boundaries/Horizontal Falls and North Lalang/pmcr-lhfmp+nlmp-zoning-hwm_kim_20161014.shp"))
  Park_Lalang.Garram.Camden.Sound=st_read(paste0(Map.hndl,"park boundaries/Lalang-Garram Camden Sound/pmcr-csmp-zoning-hwm_kim_20130626.shp"))
  Park_Northern.Kimberley=st_read(paste0(Map.hndl,"park boundaries/Northern Kimberley/pmcr-nkmp-zoning-hwm_kim_20160913.shp"))
  Park_Northwest.AMP.Network=st_read(paste0(Map.hndl,"park boundaries/Northwest AMP Network/NorthWestAMPNetwork.shp"))
  Park_WA_Commonwealth_Marine_Parks=st_read(paste0(Map.hndl,"park boundaries/WA_Commonwealth_Marine_Parks/WA_Commonwealth_Marine_Parks.shp"))
  Park_Yawuru.Roebuck=st_read(paste0(Map.hndl,"park boundaries/Yawuru Roebuck/pmcr-rbmp-zoning-hwm_kim_20160830.shp"))
  
  #plot map with bathymetry and marine parks
  WA.map=fn.map(Limx=c(112,130), Limy=c(-36,-13), Depth.data=Bathymetry,add.depth=TRUE,add.parks=TRUE,
                FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
  print(WA.map)
  ggsave(paste0(hndl.out,"Outputs/Maps/WA_parks & closures.tiff"),width = 6,height = 8,compression = "lzw")
  
  #add cISRAs layer
  what.polygon='GoogleEarth'
  ISRA.col='chocolate'
    #dummy polygons
  if(what.polygon=='dummy')
  {
    for (i in 1:nrow(cISRAs)) {
      a <-cISRAs[i,c("Lat_SE","Long_SE","Lat_NE","Long_NE","Lat_NW","Long_NW","Lat_SW","Long_SW","ISRA_pol.name")]
      a=data.frame(x=unlist(a[,grep('Long',colnames(a))]),
                   y=unlist(a[,grep('Lat',colnames(a))]),
                   Name=a$ISRA_pol.name)
      a.mid=data.frame(x=mean(a$x),y=mean(a$y),Name=unique(a$Name))
      WA.map <- WA.map +
        geom_polygon(data = a, aes(x = x, y = y), fill = ISRA.col,alpha=.5,col='transparent')+
        geom_text_repel(data=a.mid,aes(x=x,y=y,label = Name),box.padding=1.5)
    }
  }
    #Google earth polygons
  if(what.polygon=='GoogleEarth')
  {
    library(maptools)
    
    #Get GoogleEarth coordinates
    PAZ=paste0(hndl.out,'/Proposals/Google earth polygons/')
    KMZs <- list.files(path=PAZ, pattern="*.kmz", full.names=FALSE)
    sapply(KMZs,function(x)unzip(zipfile = paste0(PAZ, x),exdir =paste0(PAZ,"/KML/",x)))
    LonLat<-sapply(KMZs,function(x)getKMLcoordinates(kmlfile=paste0(PAZ,"/KML/",x,"/doc.kml"),ignoreAltitude = TRUE)[[1]])
    names(LonLat)=str_remove(str_remove(KMZs,'pAoI_'),'.kmz')
      
    #Add to map
    for (i in 1:length(LonLat)) {
      a=data.frame(x=LonLat[[i]][,1],
                   y=LonLat[[i]][,2],
                   Names=names(LonLat)[i])
      a.mid=data.frame(x=mean(a$x),y=mean(a$y),Name=unique(a$Name))
      WA.map <- WA.map +
        geom_polygon(data = a, aes(x = x, y = y), fill = ISRA.col,alpha=.5,col='transparent')+
        geom_text_repel(data=a.mid,aes(x=x,y=y,label = Name),box.padding=1.5)
    }
    
  }

  print(WA.map)
  ggsave(paste0(hndl.out,"Outputs/Maps/WA_parks & closures & ISRAs.tiff"),width = 6,height = 8,compression = "lzw")
  
  #show only ISRAs
  WA.map=fn.map(Limx=c(112,130), Limy=c(-36,-13), Depth.data=Bathymetry,add.depth=TRUE,add.parks=FALSE,
                FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
    #dummy polygons
  if(what.polygon=='dummy')
  {
    for (i in 1:nrow(cISRAs)) {
      a <-cISRAs[i,c("Lat_SE","Long_SE","Lat_NE","Long_NE","Lat_NW","Long_NW","Lat_SW","Long_SW","ISRA_pol.name")]
      a=data.frame(x=unlist(a[,grep('Long',colnames(a))]),
                   y=unlist(a[,grep('Lat',colnames(a))]),
                   Name=a$ISRA_pol.name)
      a.mid=data.frame(x=mean(a$x),y=mean(a$y),Name=unique(a$Name))
      WA.map <- WA.map +
        geom_polygon(data = a, aes(x = x, y = y), fill = ISRA.col,alpha=.5,col='transparent')+
        geom_text_repel(data=a.mid,aes(x=x,y=y,label = Name),box.padding=1.5)
    }
  }
    #Google earth polygons
  if(what.polygon=='GoogleEarth')
  {
    for (i in 1:length(LonLat)) {
      a=data.frame(x=LonLat[[i]][,1],
                   y=LonLat[[i]][,2],
                   Names=names(LonLat)[i])
      a.mid=data.frame(x=mean(a$x),y=mean(a$y),Name=unique(a$Name))
      WA.map <- WA.map +
        geom_polygon(data = a, aes(x = x, y = y), fill = ISRA.col,alpha=.5,col='transparent')+
        geom_text_repel(data=a.mid,aes(x=x,y=y,label = Name),box.padding=1.5)
    }
  }

  print(WA.map)
  ggsave(paste0(hndl.out,"Outputs/Maps/WA_ISRAs.tiff"),width = 6,height = 8,compression = "lzw")
  
  #Create individual map by ISRA
  LonLat1=LonLat
  names(LonLat1)[match("Walcott_Secure",names(LonLat1))]="Walcott Inlet and Secure Bay"
  for(i in 1:nrow(cISRAs))
  {
    xx=cISRAs[i,]
    nm=xx$ISRA
    print(paste('print individual ISRA for ------',nm))
    Long.lim=c(min(with(xx,c(Long_SW,Long_NW)))-1,max(with(xx,c(Long_NE,Long_NW)))+1)
    Lat.lim=c(min(with(xx,c(Lat_SE,Lat_SW)))-1,max(with(xx,c(Lat_NE,Lat_NW)))+1)
    WA.map=fn.map(Limx=Long.lim, Limy=Lat.lim, Depth.data=Bathymetry,add.depth=TRUE,add.parks=TRUE,
                  FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
    if(what.polygon=='GoogleEarth')
    {
      poly=LonLat1[[match(nm,names(LonLat1))]]
      
      a=data.frame(x=poly[,1],
                   y=poly[,2],
                   Names=nm)
      #a.mid=data.frame(x=mean(a$x),y=mean(a$y),Names=unique(a$Name))
      a.mid=a%>%filter(x==min(a$x))
      WA.map <- WA.map +
        geom_polygon(data = a, aes(x = x, y = y), fill = ISRA.col,alpha=.5,col='transparent')+
        geom_text_repel(data=a.mid[1,],aes(x=x,y=y,label = Names),box.padding=1.5)
      print(WA.map)
      delta.lat=abs(Lat.lim[2]-Lat.lim[1])
      delta.lon=abs(Long.lim[2]-Long.lim[1])
      WID=6
      HEI=6
      if(delta.lat-delta.lon>0.5)
      {
        WID=6
        HEI=8
      }
      if(delta.lon-delta.lat>0.5)
      {
        HEI=6
        WID=8
      }
      ggsave(paste0(hndl.out,"Outputs/Maps/By ISRA/",nm,".tiff"),width = WID,height = HEI,compression = "lzw")
    }
  }
}


# Populate proposals-------------------------------------------------------------------------
Proposal_qualifying.sp=vector('list',nrow(cISRAs))
names(Proposal_qualifying.sp)=cISRAs$ISRA
Total_sp=Proposal_supporting.sp=Proposal_qualifying.sp

pop.prop=function(sp)
{
  all_spp%>%
    filter(Common.name%in%sp)%>%
    dplyr::select(Group,Scientific.name,Common.name,IUCN.aus.status,Depth_min,Depth_max)%>%
    mutate(Group=factor(Group,levels=c('shark','ray','chimaera')))%>%
    arrange(Group,Scientific.name)
}

#1. Qualifying species 
  #1.1. create lists
for(i in 1:nrow(cISRAs))
{
  sp=unique(unlist(c(str_split(cISRAs[i,]$A.Vulnerability,', '),  
                    str_split(cISRAs[i,]$B.Range.Restricted,', '),
                    str_split(cISRAs[i,]$C1.Reproductive.Areas_species,', '),
                    str_split(cISRAs[i,]$C2.Feeding.Areas_species,', '),
                    str_split(cISRAs[i,]$C5.Undefined.Aggregations_species,', '))))
  Proposal_qualifying.sp[[i]]=pop.prop(sp=sp)
}

  #1.2. Export proposal table
change.status.to.global=TRUE
sect_properties <- prop_section(page_size = page_size(orient = "landscape",width = 11,height = 8.5),
                                type = "continuous", page_margins = page_mar(bottom=.5, top=.5, right=.5, left=.5))

for(i in 1:nrow(cISRAs))
{
  print(paste('Creating qualyfing species table for ------------',cISRAs[i,]$ISRA))
  
  D1=cISRAs[i,]$D1.Distinctiveness
  D2.total=str_extract(cISRAs[i,]$D2.Diversity, ".*?(?= in total)")
  if(!is.na(D2.total)) D2.total=paste(D2.total, 'in total')
  if(is.na(D2.total)) D2.total=''
  Sp_Crit.A=unlist(str_split(cISRAs[i,]$A.Vulnerability,', '))
  Sp_Crit.B=unlist(str_split(cISRAs[i,]$B.Range.Restricted,', '))
  Sp_Crit.C1=unlist(str_split(cISRAs[i,]$C1.Reproductive.Areas_species,', '))
  Sp_Crit.C2=unlist(str_split(cISRAs[i,]$C2.Feeding.Areas_species,', '))
  Sp_Crit.C3=unlist(str_split(cISRAs[i,]$C3.Resting.Areas_species,', '))
  Sp_Crit.C4=unlist(str_split(cISRAs[i,]$C4.Movement_species,', '))
  Sp_Crit.C5=unlist(str_split(cISRAs[i,]$C5.Undefined.Aggregations_species,', '))
  
  p=Proposal_qualifying.sp[[i]]
  
  #change status to global
  if(change.status.to.global)
  {
    p=p%>%
      left_join(all_spp%>%dplyr::select(Scientific.name,IUCN.global.status),by='Scientific.name')%>%
      mutate(IUCN.aus.status=IUCN.global.status)%>%
      dplyr::select(-IUCN.global.status)
  }
  
  p=p%>%
    mutate(IUCN.aus.status=case_when(IUCN.aus.status=="Critically Endangered"~"CR",
                                     IUCN.aus.status=="Endangered"~"EN",
                                     IUCN.aus.status=="Vulnerable"~"VU",
                                     IUCN.aus.status=="Near Threatened"~"NT",
                                     IUCN.aus.status=="Least Concern"~"LC",
                                     IUCN.aus.status=="Data Deficient"~"DD",
                                     TRUE~IUCN.aus.status),
           Group=case_when(Group=='shark'~'Sharks',
                           Group=='ray'~'Rays',
                           Group=='chimaera'~'Chimaeras'),
           Depth.range=paste(Depth_min,Depth_max,sep='-'),
           A=ifelse(Common.name%in%Sp_Crit.A,"X",''),
           B=ifelse(Common.name%in%Sp_Crit.B,"X",''),
           C1=ifelse(Common.name%in%Sp_Crit.C1,"X",''),
           C2=ifelse(Common.name%in%Sp_Crit.C2,"X",''),
           C3=ifelse(Common.name%in%Sp_Crit.C3,"X",''),
           C4=ifelse(Common.name%in%Sp_Crit.C4,"X",''),
           C5=ifelse(Common.name%in%Sp_Crit.C5,"X",''),
           D1=ifelse(Common.name%in%D1,"X",''),
           D2='')%>%
    dplyr::select(-c(Depth_min,Depth_max))%>%
    rename('Scientific Name'=Scientific.name,
           'Common Name'=Common.name,
           'IUCN Red List Category'=IUCN.aus.status,
           'Global Depth Range (m)'=Depth.range)
  p$D2[1]=D2.total
  ID=c(match("Sharks",p$Group),match("Rays",p$Group),match("Chimaeras",p$Group))
  names(ID)=c("Sharks","Rays","Chimaeras")
  is.na.id=is.na(ID)
  ID=ID[!is.na.id]
  add.dummy=p[1:length(ID),]
  add.dummy[,]=''
  add.dummy=add.dummy%>%mutate('Scientific Name'=names(ID),ID=ID)
  p=p%>%
    mutate(ID=case_when(Group=='Sharks'~ID[1]+1,
                        Group=='Rays'~ID[2]+1,
                        Group=='Chimaeras'~ID[3]+1))
  p=rbind(p,add.dummy)%>%arrange(ID)%>%dplyr::select(-c(Group,ID))
  ID.shift=c(0,1,1)
  ID.shift=ID.shift[!is.na.id]
  ID.shift=ID+ID.shift
  p%>%
    flextable%>%
    bg(bg = 'grey85', part = "header")%>%
    bold(part = "header")%>%
    bold(i=ID.shift,j=1)%>%
    italic(j='Scientific Name')%>%
    italic(i=ID.shift,j=1, italic = FALSE)%>%
    width(j=c('Scientific Name','Common Name'),width=4,unit='cm')%>%
    width(j=c('IUCN Red List Category'),width=2.5,unit='cm')%>%
    width(j=c('A','B','C1','C2','C3','C4','C5','D1'),width=1,unit='cm')%>%
    valign(valign = "top", part = "all")%>%
    set_caption(caption = "2.1 ISRA Qualifying Species and Qualifying Criteria",autonum = NULL)%>%
    save_as_docx( path = paste0(hndl.out,"Outputs/Tables_Qualifying species/",paste0(i,'_',names(Proposal_qualifying.sp)[i],".docx")),
                  align='left',pr_section = sect_properties)
  
}



#2. Supporting species table
  #2.1. create lists
#note: polygon must be big enough to encompass more than 1 grid 
for(i in 1:nrow(cISRAs))
{
  rango=cISRAs[i,c("Lat_SE","Long_SE","Lat_NE","Long_NE","Lat_NW","Long_NW","Lat_SW","Long_SW")]%>%
          data.frame
  All.sp.in.isra=grids%>%
    filter(min_lat>=rango$Lat_SE)%>%
    filter(max_lat<=rango$Lat_NE)%>%
    filter(max_lon<=rango$Long_SE)%>%
    filter(min_lon>=rango$Long_SW)%>%
          distinct(Scientific.name)%>%
    pull(Scientific.name)
  
  Total_sp[[i]]=length(All.sp.in.isra)
  
  #keep only supporting species
  All.sp.in.isra=subset(All.sp.in.isra,!All.sp.in.isra%in%Proposal_qualifying.sp[[i]]$Scientific.name)
  sp=all_spp%>%filter(Scientific.name%in%All.sp.in.isra)%>%pull(Common.name)
  #remove nonsense species
  n.sp=length(sp)
  
  if(grepl('Ashburton',cISRAs$ISRA[i])) sp=subset(sp,!sp%in%c('Oceanic whitetip shark','Greynurse shark'))
  
  if(grepl('West Kimberley',cISRAs$ISRA[i]))   
  {
    sp=subset(sp,!sp%in%c('Bronze whaler shark','Brown Stingray','Silky shark','Shortfin mako shark'))
    too.deep=c('Speckled catshark','Goldeneye shovelnose ray','Greynurse shark','Scalloped hammerhead',
               'Australian butterfly ray','Australian sharpnose shark','Bluespotted maskray',
               'Bull shark','Creek whaler shark','Dusky whaler shark','Fossil shark','Grey carpetshark',
               'Grey reef shark','Painted maskray','Pigeye shark','Reticulate whipray','Sandbar shark',
               'Silvertip shark','Sliteye shark','Spinner shark','Spot-tail shark','Tawny shark',
               'Tiger shark','Weasel shark','Whitecheek shark','Zebra shark')
    sp=subset(sp,!sp%in%too.deep)
  }
    

  if(grepl('Perth',cISRAs$ISRA[i])) sp=subset(sp,!sp%in%c("Ogilby's ghostshark","Variegated catshark",
                                                          "Silky shark","Australian blacktip shark",
                                                          "Draughtboard shark","Smooth lanternshark",
                                                          "Lemon shark","Rusty carpetshark","Edmunds' spurdog",
                                                          "Philippine spurdog","Western longnose spurdog",
                                                          "Zebra shark","Pink whipray","Jenkins' whipray",
                                                          "Shark ray","Eyebrow wedgefish","Spotted stingaree"))
  
  if(grepl('Shark Bay',cISRAs$ISRA[i])) sp=subset(sp,!sp%in%c('Oceanic whitetip shark','Bronze whaler shark',
                                                              'Australian blacktip shark','Southern eagle ray'))
  
  if(grepl('Ningaloo',cISRAs$ISRA[i])) sp=subset(sp,!sp%in%c('Western highfin spurdog',"Ogilby's ghostshark",
                                                             "Pacific spookfish","Broadnose sevengill shark",
                                                             "Gulf catshark","Oceanic whitetip shark",
                                                             "Rusty carpetshark","Whitespotted dogfish",
                                                             "Greeneye spurdog","Spikey spurdog",
                                                             "Ornate angelshark","Sandy skate"))
  
  if(grepl('South west slope',cISRAs$ISRA[i]))
  {
    sp=subset(sp,!sp%in%c('Western shovelnose stingaree','Blotched skate','Spinner shark','Tiger shark',
                          'Slender sawtail catshark','Tawny shark','Shark ray'))
    too.shallow.sp=c('Port Jackson shark','Bronze whaler shark','Draughtboard shark','Rusty carpetshark',
                     'Varied carpetshark','Southern sawshark','Smooth hammerhead','Australian angelshark',
                     'Western shovelnose ray','Southern fiddler ray','Sparsely-spotted stingaree',
                     'Elephantfish','Greynurse shark','School shark', 'White shark',
                     'Shortfin mako shark', 'Longfin mako shark','Spotted wobbegong','Whiskery shark',
                     'Gummy shark','Southern eagle ray','Smooth stingray','Dusky whaler shark','Coffin ray',
                     'Broadnose sevengill shark','Thresher shark','Blue shark','Southern round skate')
    sp=subset(sp,!sp%in%too.shallow.sp)
    #check.depth=all_spp%>%filter(Common.name%in%sp)%>%dplyr::select(Common.name,Depth_min,Depth_max)
  }
    
  n.sp1=length(sp)
  Total_sp[[i]]=Total_sp[[i]]-(n.sp-n.sp1)
  
  #extract species, status and range
  xx=pop.prop(sp=sp)%>%
              dplyr::select(-c(Depth_min,Depth_max))
  #add range restricted
  xx=xx%>%
      left_join(all_spp%>%
                filter(Common.name%in%xx$Common.name)%>%
                dplyr::select(Common.name,Cri_B),
      by='Common.name')
  
  Proposal_supporting.sp[[i]]=xx
}

  #2.2. Update Walcott river based on Al's data
sp=unique(Walcott.Al%>%
            mutate(common=case_when(common=="Largetooth Sawfish"~"Freshwater sawfish",
                                    TRUE~common),
                   common=capitalize(tolower(common)))%>%
            pull(common))
sp=subset(sp,!sp%in%Proposal_qualifying.sp$`Walcott Inlet and Secure Bay`$Common.name)
xx=pop.prop(sp=sp)%>%
  dplyr::select(-c(Depth_min,Depth_max))
xx=xx%>%
  left_join(all_spp%>%
              filter(Common.name%in%xx$Common.name)%>%
              dplyr::select(Common.name,Cri_B),
            by='Common.name')
Proposal_supporting.sp$`Walcott Inlet and Secure Bay`=xx


  #2.3. Export range restricted csv
for(i in 1:nrow(cISRAs))
{
  dummy=Proposal_supporting.sp[[i]]%>%filter(Cri_B=='yes')%>%dplyr::select(Common.name,Cri_B)
  if(nrow(dummy)>0)
  {
    write.csv(dummy,
              paste0(hndl.out,"Outputs/Add range restricted/",paste0(i,'_',names(Proposal_supporting.sp)[i],".csv")),row.names=F)
    
  }
  Proposal_supporting.sp[[i]]=Proposal_supporting.sp[[i]]%>%
    dplyr::select(-Cri_B)
}
  #2.4. Export proposal table
for(i in 1:nrow(cISRAs))
{
  print(paste('Creating supporting species table for ------------',cISRAs[i,]$ISRA))
  
  p=Proposal_supporting.sp[[i]]
  #change status to global
  if(change.status.to.global)
  {
    p=p%>%
      left_join(all_spp%>%dplyr::select(Scientific.name,IUCN.global.status),by='Scientific.name')%>%
      mutate(IUCN.aus.status=IUCN.global.status)%>%
      dplyr::select(-IUCN.global.status)
  }
  
  p=p%>%
    mutate(IUCN.aus.status=case_when(IUCN.aus.status=="Critically Endangered"~"CR",
                                     IUCN.aus.status=="Endangered"~"EN",
                                     IUCN.aus.status=="Vulnerable"~"VU",
                                     IUCN.aus.status=="Near Threatened"~"NT",
                                     IUCN.aus.status=="Least Concern"~"LC",
                                     IUCN.aus.status=="Data Deficient"~"DD",
                                     TRUE~IUCN.aus.status),
           Group=case_when(Group=='shark'~'Sharks',
                           Group=='ray'~'Rays',
                           Group=='chimaera'~'Chimaeras'))%>%
    rename('Scientific Name'=Scientific.name,
           'Common Name'=Common.name,
           'IUCN Red List Category'=IUCN.aus.status)
  ID=c(match("Sharks",p$Group),match("Rays",p$Group),match("Chimaeras",p$Group))
  names(ID)=c("Sharks","Rays","Chimaeras")
  is.na.id=is.na(ID)
  ID=ID[!is.na.id]
  add.dummy=p[1:length(ID),]
  add.dummy[,]=''
  add.dummy=add.dummy%>%mutate('Scientific Name'=names(ID),ID=ID)
  p=p%>%
    mutate(ID=case_when(Group=='Sharks'~ID[1]+1,
                        Group=='Rays'~ID[2]+1,
                        Group=='Chimaeras'~ID[3]+1))
  p=rbind(p,add.dummy)%>%arrange(ID)%>%dplyr::select(-c(Group,ID))
  ID.shift=c(0,1,2)
  ID.shift=ID.shift[!is.na.id]
  ID.shift=ID+ID.shift
  p%>%
    flextable%>%
    bg(bg = 'grey85', part = "header")%>%
    bold(part = "header")%>%
    bold(i=ID.shift,j=1)%>%
    italic(j='Scientific Name')%>%
    italic(i=ID.shift,j=1, italic = FALSE)%>%
    width(j=c('Scientific Name','Common Name'),width=4,unit='cm')%>%
    width(j=c('IUCN Red List Category'),width=4,unit='cm')%>%
    valign(valign = "top", part = "all")%>%
    set_caption(caption = "2.2 Supporting Species",autonum = NULL)%>%
    save_as_docx( path = paste0(hndl.out,"Outputs/Tables_Supporting species/",paste0(i,'_',names(Proposal_qualifying.sp)[i],".docx")),
                  align='left')
  
  write.csv(Total_sp[[i]],paste0(hndl.out,"Outputs/Tables_Total number of species/",paste0(i,'_',names(Total_sp)[i],".csv")),row.names=F)
}




# Kernel density by ISRAs -------------------------------------------------------------------------
if(do.preliminary)
{
  kernel.density=function(IsRa,Bins=6,jit.factor=2,Poly)
  {
    a=data.frame(x=Poly[,1], y=Poly[,2])
    
    qualif.sp=Proposal_qualifying.sp[[match(IsRa,names(Proposal_qualifying.sp))]]$Scientific.name 
    supp.sp=Proposal_supporting.sp[[match(IsRa,names(Proposal_supporting.sp))]]$Scientific.name  
    
    d=Temporal.WA%>%    
      filter(ISRA==IsRa)%>%
      mutate(dummy=str_remove(grid_id,'grid_'),
             Lat=-abs(as.numeric(str_extract(dummy, "(?<=-).*"))),
             Long=as.numeric(str_extract(dummy, "[^_]+")),
             Group=ifelse(Scientific.name%in%qualif.sp,'Qualifying sp.',
                          ifelse(Scientific.name%in%supp.sp,'Supporting sp.',NA)))%>%
      filter(!is.na(Group))
    
    #by species group
    set.seed(666)
    d1=d%>%
      group_by(Group,Lat,Long)%>%
      summarise(N=sum(n_obs),.groups = "drop")%>%
      ungroup()
    d1=with(d1,d1[rep(1:nrow(d1),N),])%>%
      mutate(Lat=jitter(Lat,jit.factor),
             Long=jitter(Long,jit.factor))
    p_group=d1%>%
      ggplot(aes(Long,Lat))+
      geom_point(aes(colour=Group),alpha=0.3)+
      stat_density2d_filled(aes(alpha=..level..,fill=..level..),size=2,bins=Bins, show.legend = F)+
      geom_density2d(colour='black',bins=Bins)+
      theme_PA(Sbt.siz=12,cap.siz=9)+
      theme(legend.position = 'bottom',
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0))+
      labs(title='Kernel density',
           subtitle='(Sourced from the national SDMP database)',
           caption=paste('SDMP, Species Distribution Modelling Project.',
                         'Grid observations were added a jitter factor of',jit.factor))+
      ylim(range(d1$Lat)*c(1.001,0.999)) + xlim(range(d1$Long)*c(0.999,1.001))+
      geom_polygon(data = a, aes(x = x, y = y), fill = ISRA.col,alpha=.5,col='transparent')
    print(p_group)
    ggsave(paste0(hndl.out,"Outputs/z_Kernel density/",i,'_',IsRa,"_by sp group.tiff"),width = 6,height = 6,compression = "lzw")
    
    #all species combined
    set.seed(666)
    d1=d%>%
      group_by(Lat,Long)%>%
      summarise(N=sum(n_obs),.groups = "drop")%>%
      ungroup()
    d1=with(d1,d1[rep(1:nrow(d1),N),])%>%
      mutate(Lat=jitter(Lat,jit.factor),
             Long=jitter(Long,jit.factor))
    p_all=d1%>%
      ggplot(aes(Long,Lat))+
      geom_point(alpha=0.3)+
      stat_density2d_filled(aes(alpha=..level..,fill=..level..),size=2,bins=Bins, show.legend = F)+
      geom_density2d(colour='black',bins=Bins)+
      theme_PA(Sbt.siz=12,cap.siz=9)+
      theme(legend.position = 'bottom',
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0))+
      labs(title='Kernel density',
           subtitle='(Sourced from the national SDMP database)',
           caption='SDMP, Species Distribution Modelling Project')+
      ylim(range(d1$Lat)*c(1.001,0.999)) + xlim(range(d1$Long)*c(0.999,1.001))+
      geom_polygon(data = a, aes(x = x, y = y), fill = ISRA.col,alpha=.5,col='transparent') 
    print(p_all)
    ggsave(paste0(hndl.out,"Outputs/z_Kernel density/",i,'_',IsRa,"_sp combined.tiff"),width = 6,height = 6,compression = "lzw")
    
  }
  for(i in 1:nrow(cISRAs))
  {
    print(paste('Kernel density for ------------',cISRAs[i,]$ISRA))
    kernel.density(IsRa=cISRAs$ISRA[i],Poly=LonLat1[[match(cISRAs[i,]$ISRA,names(LonLat1))]])
  }
}

  

# Observations only by ISRAs -------------------------------------------------------------------------
if(do.preliminary)
{
  Obs.only=function(IsRa,jit.factor=2,Poly)
  {
    a=data.frame(x=Poly[,1], y=Poly[,2])
    qualif.sp=Proposal_qualifying.sp[[match(IsRa,names(Proposal_qualifying.sp))]]$Scientific.name 
    supp.sp=Proposal_supporting.sp[[match(IsRa,names(Proposal_supporting.sp))]]$Scientific.name  
    
    d=Temporal.WA%>%    
      filter(ISRA==IsRa)%>%
      mutate(dummy=str_remove(grid_id,'grid_'),
             Lat=-abs(as.numeric(str_extract(dummy, "(?<=-).*"))),
             Long=as.numeric(str_extract(dummy, "[^_]+")),
             Group=ifelse(Scientific.name%in%qualif.sp,'Qualifying sp.',
                          ifelse(Scientific.name%in%supp.sp,'Supporting sp.',NA)))%>%
      filter(!is.na(Group))
    
    #by species group
    set.seed(666)
    d1=d%>%
      group_by(Group,Lat,Long)%>%
      summarise(N=sum(n_obs),.groups = "drop")%>%
      ungroup()%>%
      mutate(Lat=jitter(Lat,jit.factor),
             Long=jitter(Long,jit.factor))
    p_group=d1%>%
      ggplot(aes(Long,Lat))+
      geom_point(aes(colour=Group,size=N),alpha=0.3)+
      theme_PA(Sbt.siz=12,cap.siz=9,leg.siz=9)+
      theme(legend.title=element_blank(),
            legend.position = 'bottom',
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0))+
      labs(title='Number of observations',
           subtitle='(Sourced from the national SDMP database)',
           caption=paste('SDMP, Species Distribution Modelling Project.',
                         'Grid observations were added a jitter factor of',jit.factor))+
      ylim(range(d1$Lat)*c(1.001,0.999)) + xlim(range(d1$Long)*c(0.999,1.001))+
      geom_polygon(data = a, aes(x = x, y = y), fill = ISRA.col,alpha=.5,col='transparent') 
    print(p_group)
    ggsave(paste0(hndl.out,"Outputs/z_Observations only/",i,'_',IsRa,"_by sp group.tiff"),width = 6,height = 6,compression = "lzw")
    
    #all species combined
    set.seed(666)
    d1=d%>%
      group_by(Lat,Long)%>%
      summarise(N=sum(n_obs),.groups = "drop")%>%
      ungroup()%>%
      mutate(Lat=jitter(Lat,jit.factor),
             Long=jitter(Long,jit.factor))
    p_all=d1%>%
      ggplot(aes(Long,Lat))+
      geom_point(aes(size=N),alpha=0.3)+
      theme_PA(Sbt.siz=12,cap.siz=9,leg.siz=9)+
      theme(legend.title=element_blank(),
            legend.position = 'bottom',
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0))+
      labs(title='Number of observations',
           subtitle='(Sourced from the national SDMP database)',
           caption=paste('SDMP, Species Distribution Modelling Project.',
                         'Grid observations were added a jitter factor of',jit.factor))+
      ylim(range(d1$Lat)*c(1.001,0.999)) + xlim(range(d1$Long)*c(0.999,1.001))+
      geom_polygon(data = a, aes(x = x, y = y), fill = ISRA.col,alpha=.5,col='transparent')
    print(p_all)
    ggsave(paste0(hndl.out,"Outputs/z_Observations only/",i,'_',IsRa,"_sp combined.tiff"),width = 6,height = 6,compression = "lzw")
    
  }
  for(i in 1:nrow(cISRAs))
  {
    print(paste('Observations only for ------------',cISRAs[i,]$ISRA))
    Obs.only(IsRa=cISRAs$ISRA[i],Poly=LonLat1[[match(cISRAs[i,]$ISRA,names(LonLat1))]])
  }
}

  
# Regular occurrence in ISRAs -------------------------------------------------------------------------
occurrence.thru.time=function(IsRa,point.size=FALSE,use.buffer=FALSE)
{
  set.seed(666)
  d=Temporal.WA%>%    
    filter(ISRA==IsRa)%>%
    mutate(lat=as.numeric(sub(".*?_", "", str_remove(grid_id,'grid_'))),
           lon=as.numeric(sub("_.*", "",str_remove(grid_id,'grid_'))))
  if(!use.buffer) d=d%>%filter(!Buffer=='buffer')
  yr.rand=round(runif(nrow(d),1975,max(d$Year,na.rm=T)))
  qualif.sp=Proposal_qualifying.sp[[match(IsRa,names(Proposal_qualifying.sp))]]$Scientific.name 
  supp.sp=Proposal_supporting.sp[[match(IsRa,names(Proposal_supporting.sp))]]$Scientific.name 
  
  if(IsRa=="Perth metro") 
  {
    d.CS=d%>%
      filter(lat>=(-32.273) & lat<=(-32.05))%>%
      filter(lon>=115.632 & lon<=115.82)%>%
      mutate(Year=ifelse(is.na(Year),yr.rand,Year))%>%
      group_by(Scientific.name,Year,Data.type)%>%
      summarise(N=sum(n_obs),.groups = "drop")%>%
      ungroup()%>%
      mutate(Group=ifelse(Scientific.name%in%qualif.sp,'Qualifying sp.',
                          ifelse(Scientific.name%in%supp.sp,'Supporting sp.',NA)))%>%
      filter(!is.na(Group))
    write.csv(d.CS,paste0(hndl.out,"Outputs/z_Temporal/",i,'_',IsRa,"_Cockburn Sound.csv"),row.names = F)
    
    d.data.type=d%>%
      filter(lat>=(-32.273) & lat<=(-32.05))%>%
      filter(lon>=115.632 & lon<=115.82)%>%
      mutate(Year=ifelse(is.na(Year),yr.rand,Year))%>%
      group_by(Scientific.name,Year,Data.type)%>%
      summarise(N=sum(n_obs),.groups = "drop")%>%
      ungroup()%>%
      mutate(Group=ifelse(Scientific.name%in%qualif.sp,'Qualifying sp.',
                          ifelse(Scientific.name%in%supp.sp,'Supporting sp.',NA)))%>%
      filter(!is.na(Group))
    
    d1=d%>%
      filter(lat>=(-32.273) & lat<=(-32.05))%>%
      filter(lon>=115.632 & lon<=115.82)%>%
      mutate(Year=ifelse(is.na(Year),yr.rand,Year))%>%
      group_by(Scientific.name,Year)%>%
      summarise(N=sum(n_obs),.groups = "drop")%>%
      ungroup()%>%
      mutate(Group=ifelse(Scientific.name%in%qualif.sp,'Qualifying sp.',
                          ifelse(Scientific.name%in%supp.sp,'Supporting sp.',NA)))%>%
      filter(!is.na(Group))
    
    
    
    p=d1%>%
      ggplot(aes(Year,Scientific.name,color=Group))+
      geom_point(size=2)+geom_line(linetype='dotted')+
      if(point.size) p=p+geom_point(aes(size=N))
    p=p+
      theme_PA(Sbt.siz=12,cap.siz=9)+
      theme(legend.position = 'bottom',
            axis.text.y = element_text(face='italic'),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0))+
      ylab('Scientific name')+
      labs(title='Reported occurrence',
           subtitle='(Sourced from the national SDMP database)',
           caption='SDMP, Species Distribution Modelling Project')+
      geom_vline(xintercept = 2009)
    print(p)
    Height=6
    if(length(unique(d$Scientific.name))>35) Height=9 
    Width=8
    ggsave(paste0(hndl.out,"Outputs/z_Temporal/",i,'_',IsRa,"_Cockburn Sound.tiff"),width = Width,height = Height,compression = "lzw")
    write.csv(d.data.type,paste0(hndl.out,"Outputs/z_Temporal/",i,'_',IsRa,"_Cockburn Sound.csv"),row.names = F)
    
  }
  d.data.type=d%>%
    mutate(Year=ifelse(is.na(Year),yr.rand,Year))%>%
    group_by(Scientific.name,Year,Data.type)%>%
    summarise(N=sum(n_obs),.groups = "drop")%>%
    ungroup()%>%
    mutate(Group=ifelse(Scientific.name%in%qualif.sp,'Qualifying sp.',
                        ifelse(Scientific.name%in%supp.sp,'Supporting sp.',NA)))%>%
    filter(!is.na(Group))
  
  d=d%>%
    mutate(Year=ifelse(is.na(Year),yr.rand,Year))%>%
    group_by(Scientific.name,Year)%>%
    summarise(N=sum(n_obs),.groups = "drop")%>%
    ungroup()%>%
    mutate(Group=ifelse(Scientific.name%in%qualif.sp,'Qualifying sp.',
                        ifelse(Scientific.name%in%supp.sp,'Supporting sp.',NA)))%>%
    filter(!is.na(Group))
  
  
  
  p=d%>%
    ggplot(aes(Year,Scientific.name,color=Group))+
    geom_point(size=2)+geom_line(linetype='dotted')+
    if(point.size) p=p+geom_point(aes(size=N))
  p=p+
    theme_PA(Sbt.siz=12,cap.siz=9)+
    theme(legend.position = 'bottom',
          axis.text.y = element_text(face='italic'),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))+
    ylab('Scientific name')+
    labs(title='Reported occurrence',
         subtitle='(Sourced from the national SDMP database)',
         caption='SDMP, Species Distribution Modelling Project')+
    geom_vline(xintercept = 2009)
  print(p)
  Height=6
  if(length(unique(d$Scientific.name))>35) Height=9 
  Width=8
  ggsave(paste0(hndl.out,"Outputs/z_Temporal/",i,'_',IsRa,".tiff"),width = Width,height = Height,compression = "lzw")
  write.csv(d.data.type,paste0(hndl.out,"Outputs/z_Temporal/",i,'_',IsRa,".csv"),row.names = F)
}
for(i in 1:nrow(cISRAs))
{
  print(paste('occurrence thrutime for ------------',cISRAs[i,]$ISRA))
  occurrence.thru.time(IsRa=cISRAs$ISRA[i])
}

# Map observations per ISRA-------------------------------------------------------------------------
map.obs=function(d,isra)
{
  d=Temporal.WA%>%    
    filter(ISRA==isra)%>%
    mutate(lat=as.numeric(sub(".*?_", "", str_remove(grid_id,'grid_'))),
           lon=as.numeric(sub("_.*", "",str_remove(grid_id,'grid_'))))
  if(grepl('Walcott',isra))
  {
    d=Al_all%>%rename(Scientific.name=species_long)
  }
  
  LIMY=c(min(d$lat),max(d$lat))
  LIMX=c(min(d$lon),max(d$lon))
  Base.map=fn.map(Limx=LIMX, Limy=LIMY, Depth.data=Bathymetry,add.depth=FALSE,add.parks=FALSE,
                  FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
  
  d=d%>%
    group_by(lon,lat,Scientific.name)%>%
    tally()%>%
    ungroup()
  
  Base.map+
    geom_point(data=d,aes(lon,lat,color=Scientific.name,size=n))+
    theme_PA()
  ggsave(paste0(hndl.out,"Outputs/Maps/",isra,"_observations.tiff"),width = 8,height = 8,compression = "lzw")
  
}
for(i in 1:nrow(cISRAs))
{
  map.obs(d=D,isra=cISRAs$ISRA[i])
}

# Range restricted overall distribution -------------------------------------------------------------------------
range.restricted.dist=function(SP,dd)
{
  dd=dd%>%
    filter(Common.name==SP)
  if(SP=="Western wobbegong") dd=dd%>%filter(lat<=(-21))
  
  LIMX=c(min(dd$lon),max(dd$lon))
  LIMY=c(min(dd$lat),max(dd$lat))
  Base.map=fn.map(Limx=LIMX, Limy=LIMY, Depth.data=Bathymetry,add.depth=TRUE,add.parks=FALSE,
                  FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
  
  
  #overall spatial dist
  d1=dd%>%
    group_by(lat,lon)%>%
    summarise(n=sum(n_obs))
  
  p1=Base.map+
    geom_point(data=d1,aes(lon,lat,size=n),color='black',shape = 21,fill='steelblue',alpha=.3)+
    theme(legend.position = 'top')+ggtitle(SP)
  
  print(p1)
  Height=Width=6
  if((LIMY[2]-LIMY[1]) >= 2*(LIMX[2]-LIMX[1] ))
  {
    Height=8
    Width=5
  }
  ggsave(paste0(hndl.out,"Outputs/z_range.restricted/",IsRa,'_',SP,".tiff"),width = Width,height = Height,compression = "lzw")
  
}
for(i in 1:nrow(cISRAs))
{
  print(paste('range restricted analysis for ------------',cISRAs[i,]$ISRA))
  IsRa=cISRAs$ISRA[i]
  if(!is.na(cISRAs$B.Range.Restricted[i]))
  {
    Unik.sp=unlist(str_split(cISRAs$B.Range.Restricted[i],', '))
    DAT=Temporal.WA_all%>%
      left_join(all_spp%>%dplyr::select(Scientific.name,Common.name),by='Scientific.name')%>%
      filter(Common.name%in%Unik.sp)
    if(IsRa=="Walcott Inlet and Secure Bay")
    {
      DAT=Al_all%>%
        rename(Scientific.name=species_long)%>%
        left_join(all_spp%>%dplyr::select(Scientific.name,Common.name),by='Scientific.name')%>%
        filter(Common.name%in%Unik.sp)%>%
        mutate(Year=year(start_date))%>%
        group_by(Scientific.name,Year,lat,lon,Common.name)%>%
        tally()%>%
        ungroup()%>%
        rename(n_obs=n)
    }
    for(s in 1:length(Unik.sp))   range.restricted.dist(SP=Unik.sp[s],dd=DAT)
  }

}

# Find neonates and YOY  -------------------------------------------------------------------------
Size.colors=c(Neonate="navyblue", YOY="skyblue1", Other="azure2")
fn.plot.juveniles=function(sp,min.smap.size=25,siz.kol=Size.colors,TL.species,dat.name)
{
  d=Da%>%
    filter(COMMON_NAME==sp)%>%
    filter(!is.na(Mid.Lat))
  if(sum(is.na(d$Class))==nrow(d))
  {
    d=d%>%
      mutate(Length=ifelse(grepl(paste(TL.species,collapse = '|'),COMMON_NAME),TL,FL))%>%
      filter(!is.na(Length))%>%
      mutate(Class=cut(Length,3))
    names(siz.kol)=levels(d$Class)
  }
  d=d%>%
    group_by(COMMON_NAME,SCIENTIFIC_NAME,Mid.Lat, Mid.Long,Class)%>%
    tally()%>%
    ungroup()
  
  if(nrow(d)>min.smap.size)
  {
    LIMY=c(min(d$Mid.Lat),max(d$Mid.Lat))
    LIMX=c(min(d$Mid.Long),max(d$Mid.Long))
    if(LIMY[2]<=.95*LIMY[1] | LIMX[2]<=1.005*LIMX[1])
    {
      LIMY[1]=LIMY[1]-1
      LIMY[2]=LIMY[2]+1
      
      LIMX[1]=LIMX[1]-1
      LIMX[2]=LIMX[2]+1
    }
    
    Base.map=fn.map(Limx=LIMX, Limy=LIMY, Depth.data=Bathymetry,add.depth=FALSE,add.parks=FALSE,
                    FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
    
    p1=Base.map+
      geom_point(data=d,aes(Mid.Long,Mid.Lat, size=n,fill=Class),
                 alpha=.5,color='black',shape = 21)+
      facet_wrap(~Class,nrow=1)+
      theme_PA()+
      theme(legend.position = 'top',
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      scale_fill_manual(values=siz.kol,drop=FALSE)+
      guides(fill = "none")+
      ggtitle(unique(d$SCIENTIFIC_NAME))
    
    print(p1)
    Height=Width=6
    if((LIMY[2]-LIMY[1]) >= 2*(LIMX[2]-LIMX[1] ))
    {
      Height=8
      Width=5
    }
    Width=8
    ggsave(paste0(hndl.out,"Outputs/z_neonates/",paste(dat.name,sp,sep='_'),".tiff"),width = Width,height = Height,compression = "lzw")
    
  }
  
}

#Al's data Walcott only
Al_neonate=Walcott.Al%>%
  mutate(CAAB_code=case_when(common=="Northern River Shark"~18042,
                             common=="Green Sawfish"~25001,
                             common=="Dwarf Sawfish"~25004,
                             common=="Narrow Sawfish"~25002,
                             common=="Largetooth Sawfish"~25003,
                             common=="Speartooth Shark"~18041,
                             common=="Australian Sharpnose Shark"~18024,
                             common=="Bull Shark"~18021,
                             common=="Graceful Shark"~18033,
                             common=="Great Hammerhead"~19002,
                             common=="Lemon Shark"~18029,
                             common=="Nervous Shark"~18034,
                             common=="Pigeye Shark"~18026 ))

Al_neonate=Al_neonate%>%
  left_join(LH.data%>%
              dplyr::select(SPECIES,LF_o,a_FL.to.TL,b_FL.to.TL,K,FL_inf,male_K,male_FL_inf)%>%
              filter(SPECIES%in%unique(Al_neonate$CAAB_code)),
            by=c('CAAB_code'='SPECIES'))%>%
  mutate(male_K=ifelse(is.na(male_K),K,male_K),
         male_FL_inf=ifelse(is.na(male_FL_inf),K,male_FL_inf))

Da=Al_neonate%>%
  mutate(TL=ifelse(is.na(TL) & !is.na(FL) & !is.na(a_FL.to.TL),FL*a_FL.to.TL+b_FL.to.TL,TL),
         FL=ifelse(is.na(FL) & !is.na(TL) & !is.na(a_FL.to.TL),(FL-b_FL.to.TL)/a_FL.to.TL,FL))%>%
  mutate(dummy=ifelse(!is.na(TL)| !is.na(FL),'keep','drop'))%>%
  filter(dummy=='keep')%>%
  dplyr::select(-dummy)%>%
  rename(SEX=sex)%>%
  mutate(SEX=capitalize(SEX))%>%
  mutate(SEX=ifelse(!SEX=='M','F',SEX),
         SEX=ifelse(is.na(SEX),'F',SEX),
         FL.neonate=ifelse(!is.na(K) & SEX=='M',male_FL_inf-(male_FL_inf-LF_o)*exp(-male_K*(1/12)),
                           ifelse(!is.na(K) & !SEX=='M',FL_inf-(FL_inf-LF_o)*exp(-K*(1/12)),
                                  NA)),
         FL.YOY=ifelse(!is.na(K) & SEX=='M',male_FL_inf-(male_FL_inf-LF_o)*exp(-male_K*(1)),
                       ifelse(!is.na(K) & !SEX=='M',FL_inf-(FL_inf-LF_o)*exp(-K*(1)),
                              NA)),
         TL.neonate=FL.neonate*a_FL.to.TL+b_FL.to.TL,
         TL.YOY=FL.YOY*a_FL.to.TL+b_FL.to.TL)

#determine class
Da=Da%>%
  mutate(Class=ifelse((!is.na(TL) & TL<=TL.YOY) | (!is.na(FL) & FL<=FL.YOY),'YOY','Other'),
         Class=ifelse((!is.na(TL) & TL<=TL.neonate) | (!is.na(FL) & FL<=FL.neonate),'Neonate',Class),
         Class=factor(Class,levels=c('Neonate','YOY','Other')))%>%
  mutate(COMMON_NAME=common,
         SCIENTIFIC_NAME=species_long,
         Mid.Lat=lat,
         Mid.Long=lon)%>%
  mutate(Year=year(start_date))
Unik.Sp=sort(unique(Da$COMMON_NAME))
Unik.Sp=subset(Unik.Sp,!Unik.Sp=='')

for(s in 1:length(Unik.Sp))
{
  print(paste('--------------plot neonates and YOY for -----',Unik.Sp[s]))
  fn.plot.juveniles(sp=Unik.Sp[s],
                    min.smap.size=2,
                    TL.species=Unik.Sp,
                    dat.name="Al data/")
}

#table of species by year and class
Table.numbers.by.class=Da%>%
  group_by(COMMON_NAME,SCIENTIFIC_NAME,Class,Year)%>%
  tally()
write.csv(Table.numbers.by.class,paste0(hndl.out,"Outputs/z_neonates/Al data/Table.numbers.by.class.csv"),row.names = F)

#Size classes
for(s in 1:length(Unik.Sp))
{
  p=Da%>%
    filter(COMMON_NAME==Unik.Sp[s])%>%
    filter(!is.na(TL))%>%
    mutate(bin=10*floor(TL/10))%>%
    ggplot(aes(bin,fill=Class))+
    geom_bar()+
    guides(fill=guide_legend(nrow=1,byrow=TRUE))+
    xlab('TL (cm)')+theme_PA()+
    theme(legend.position = 'top',
          legend.title = element_blank())+
    scale_fill_manual(values=Size.colors,drop=FALSE)+xlim(0,NA)
  print(p)
  ggsave(paste0(hndl.out,"Outputs/z_neonates/Al data/Length comps_",Unik.Sp[s],".tiff"),width = 6,height = 6,compression = "lzw")
  
  
}

#Neonates thru time
Table.numbers.by.class%>%
  ggplot(aes(Year,n,color=SCIENTIFIC_NAME))+
  geom_point()+
  geom_line(linetype='dashed')+
  facet_wrap(~Class,nrow=3,ncol=1)+
  theme_PA(leg.siz=10)+
  theme(legend.position = 'top',legend.title = element_blank())+
  guides(colour = guide_legend(nrow = 4))
ggsave(paste0(hndl.out,"Outputs/z_neonates/Al data/Classes thru time_all species.tiff"),
       width = 6.25,height = 7,compression = "lzw")

#Table of used length neonate and yoy
write.csv(Da%>%
            distinct(COMMON_NAME,SCIENTIFIC_NAME,SEX,TL.neonate,TL.YOY)%>%
            arrange(SCIENTIFIC_NAME),
          paste0(hndl.out,"Outputs/z_neonates/Al data/Table.length.neonate.YOY.species.csv"),row.names = F)


#WA's Observer data
if(do.observer)
{
  Da=DATA%>%
    filter(TYPE=='Elasmo')%>%
    dplyr::select(SHEET_NO,LINE_NO,BOTDEPTH,BOAT,SOAK.TIME,Method,NET_LENGTH,N.hooks,date,Day,Month,year,
                  Mid.Lat,Mid.Long,SPECIES,CAES_Code, CAAB_code,COMMON_NAME,SCIENTIFIC_NAME,TL,FL,PL,SEX,Numbers)%>%
    mutate(Method=ifelse(Method=='LL; DL','LL',Method))
  Da=Da%>%
    left_join(LH.data%>%
                dplyr::select(SPECIES,a_w8t,b_w8t,male_a_w8t,male_b_w8t,w8t_length.units,
                              LF_o,a_FL.to.TL,b_FL.to.TL,TL.50.mat,TL.95.mat,Max.TL,
                              K,FL_inf,male_K,male_FL_inf)%>%
                filter(SPECIES%in%unique(DATA$CAAB_code)),
              by=c('CAAB_code'='SPECIES'))
  #fixed NA lengths
  Da=Da%>%
    mutate(TL=ifelse(is.na(TL) & !is.na(FL) & !is.na(a_FL.to.TL),FL*a_FL.to.TL+b_FL.to.TL,TL),
           FL=ifelse(is.na(FL) & !is.na(TL) & !is.na(a_FL.to.TL),(FL-b_FL.to.TL)/a_FL.to.TL,FL))
  
  #remove records with no TL or no FL
  Da=Da%>%
    mutate(dummy=ifelse(!is.na(TL)| !is.na(FL),'keep','drop'))%>%
    filter(dummy=='keep')%>%
    dplyr::select(-dummy)
  
  #add length of neonate and YOY
  Da=Da%>%
    mutate(SEX=ifelse(!SEX=='M','F',SEX),
           SEX=ifelse(is.na(SEX),'F',SEX),
           FL.neonate=ifelse(!is.na(K) & SEX=='M',male_FL_inf-(male_FL_inf-LF_o)*exp(-male_K*(1/12)),
                      ifelse(!is.na(K) & !SEX=='M',FL_inf-(FL_inf-LF_o)*exp(-K*(1/12)),
                             NA)),
           FL.YOY=ifelse(!is.na(K) & SEX=='M',male_FL_inf-(male_FL_inf-LF_o)*exp(-male_K*(1)),
                         ifelse(!is.na(K) & !SEX=='M',FL_inf-(FL_inf-LF_o)*exp(-K*(1)),
                                NA)),
           TL.neonate=FL.neonate*a_FL.to.TL+b_FL.to.TL,
           TL.YOY=FL.YOY*a_FL.to.TL+b_FL.to.TL)
  
  #class observations as neonate, YOY, other
  Da=Da%>%
    mutate(Class=ifelse((!is.na(TL) & TL<=TL.YOY) | (!is.na(FL) & FL<=FL.YOY),'YOY','Other'),
           Class=ifelse((!is.na(TL) & TL<=TL.neonate) | (!is.na(FL) & FL<=FL.neonate),'Neonate',Class),
           Class=factor(Class,levels=c('Neonate','YOY','Other')))
  Unik.Sp=sort(unique(Da$COMMON_NAME))
  Unik.Sp=subset(Unik.Sp,!Unik.Sp=='')
  for(s in 1:length(Unik.Sp))
  {
    print(paste('--------------plot neonates and YOY for -----',Unik.Sp[s]))
    fn.plot.juveniles(sp=Unik.Sp[s],
                      TL.species=c('wobbegong','catshark','sawfish','carpet sharks',
                                   'stingray','eagle ray','Wobbegong'),
                      dat.name='')
  }
  
  # 6 and 9 months olds sandbar sharks
  do.dis=FALSE
  if(do.dis)
  {
    Da=DATA%>%
      filter(TYPE=='Elasmo')%>%
      dplyr::select(SHEET_NO,LINE_NO,BOTDEPTH,BOAT,SOAK.TIME,Method,NET_LENGTH,N.hooks,date,Day,Month,year,
                    Mid.Lat,Mid.Long,SPECIES,CAES_Code, CAAB_code,COMMON_NAME,SCIENTIFIC_NAME,TL,FL,PL,SEX,Numbers)%>%
      mutate(Method=ifelse(Method=='LL; DL','LL',Method))%>%filter(SPECIES=='TK')
    Da=Da%>%
      left_join(LH.data%>%
                  dplyr::select(SPECIES,a_w8t,b_w8t,male_a_w8t,male_b_w8t,w8t_length.units,
                                LF_o,a_FL.to.TL,b_FL.to.TL,TL.50.mat,TL.95.mat,Max.TL,
                                K,FL_inf,male_K,male_FL_inf)%>%
                  filter(SPECIES%in%unique(DATA$CAAB_code)),
                by=c('CAAB_code'='SPECIES'))
    #fixed NA lengths
    Da=Da%>%
      mutate(TL=ifelse(is.na(TL) & !is.na(FL) & !is.na(a_FL.to.TL),FL*a_FL.to.TL+b_FL.to.TL,TL),
             FL=ifelse(is.na(FL) & !is.na(TL) & !is.na(a_FL.to.TL),(FL-b_FL.to.TL)/a_FL.to.TL,FL))
    
    #remove records with no TL or no FL
    Da=Da%>%
      mutate(dummy=ifelse(!is.na(TL)| !is.na(FL),'keep','drop'))%>%
      filter(dummy=='keep')%>%
      dplyr::select(-dummy)
    
    #add length of neonate and YOY
    Da=Da%>%
      mutate(SEX=ifelse(!SEX=='M','F',SEX),
             SEX=ifelse(is.na(SEX),'F',SEX),
             FL.neonate=ifelse(!is.na(K) & SEX=='M',male_FL_inf-(male_FL_inf-LF_o)*exp(-male_K*(6/12)),
                               ifelse(!is.na(K) & !SEX=='M',FL_inf-(FL_inf-LF_o)*exp(-K*(6/12)),
                                      NA)),
             FL.YOY=ifelse(!is.na(K) & SEX=='M',male_FL_inf-(male_FL_inf-LF_o)*exp(-male_K*(9.5/12)),
                           ifelse(!is.na(K) & !SEX=='M',FL_inf-(FL_inf-LF_o)*exp(-K*(9.5/12)),
                                  NA)),
             TL.neonate=FL.neonate*a_FL.to.TL+b_FL.to.TL,
             TL.YOY=FL.YOY*a_FL.to.TL+b_FL.to.TL)
    
    #class observations as neonate, YOY, other
    Da=Da%>%
      mutate(Class=ifelse((!is.na(TL) & TL<=TL.YOY) | (!is.na(FL) & FL<=FL.YOY),'YOY','Other'),
             Class=ifelse((!is.na(TL) & TL<=TL.neonate) | (!is.na(FL) & FL<=FL.neonate),'Neonate',Class),
             Class=ifelse(Class=='Neonate','<=6 months',ifelse(Class=='YOY','6-9 months',Class)),
             Class=factor(Class,levels=c('<=6 months','6-9 months','Other')))
    
    
    min.smap.size=25
    siz.kol=Size.colors
    TL.species="Green Sawfish"
    dat.name=''
    sp="Sandbar shark"
    d=Da%>%
      filter(!is.na(Mid.Lat))%>%
      group_by(COMMON_NAME,SCIENTIFIC_NAME,Mid.Lat, Mid.Long,Class)%>%
      tally()%>%
      ungroup()
    
    if(nrow(d)>min.smap.size)
    {
      LIMY=c(min(d$Mid.Lat),max(d$Mid.Lat))
      LIMX=c(min(d$Mid.Long),max(d$Mid.Long))
      Base.map=fn.map(Limx=LIMX, Limy=LIMY, Depth.data=Bathymetry,add.depth=FALSE,add.parks=FALSE,
                      FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
      
      p1=Base.map+
        geom_point(data=d,aes(Mid.Long,Mid.Lat, size=n,fill=Class),
                   alpha=.5,color='black',shape = 21)+
        facet_wrap(~Class,nrow=1)+
        theme_PA()+
        theme(legend.position = 'top',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
        scale_fill_manual(values=siz.kol,drop=FALSE)+
        guides(fill = "none")+
        ggtitle(unique(d$SCIENTIFIC_NAME))
      
      print(p1)
      Height=Width=6
      if((LIMY[2]-LIMY[1]) >= 2*(LIMX[2]-LIMX[1] ))
      {
        Height=8
        Width=5
      }
      Width=8
      ggsave(paste0(hndl.out,"Outputs/z_neonates/",paste(dat.name,sp,sep=''),"_6 and 9 mmonths.tiff"),width = Width,height = Height,compression = "lzw")
      
    }
    
  }  
  
  #Table of used length neonate and yoy
  write.csv(Da%>%
              distinct(COMMON_NAME,SCIENTIFIC_NAME,SEX,TL.neonate,TL.YOY)%>%
              arrange(SCIENTIFIC_NAME),
            paste0(hndl.out,"Outputs/z_neonates/Table.length.neonate.YOY.species.csv"),row.names = F)
  
  
  #Plot by small, medium or large
  Size.colors.size=c(Small="navyblue", Medium="skyblue1", Large="azure2")
  
  #1. cut lengths into 3 categories
  TL.SPECIES=c('wobbegong','catshark','sawfish','carpet sharks',
               'stingray','eagle ray','Wobbegong')
  Size.classes=data.frame(Sp=Unik.Sp)
  fun.cut=function(x)
  {
    d=Da%>%
      filter(COMMON_NAME==x)%>%
      filter(!is.na(Mid.Lat))%>%
      mutate(Length=ifelse(grepl(paste(TL.SPECIES,collapse = '|'),COMMON_NAME),TL,FL))%>%
      filter(!is.na(Length))%>%
      mutate(Class=cut(Length,3))
    Lvl=levels(d$Class)
    aa=str_remove_all(Lvl[1], "[()]")
    return(data.frame(Sp=x,
                      Small=as.numeric(sub(",.*", "", str_remove_all(Lvl[1], "[()]"))),
                      Medium=as.numeric(sub(",.*", "", str_remove_all(Lvl[2], "[()]")))))
  }
  Length.classes=lapply(Size.classes$Sp,fun.cut)
  Size.classes=do.call(rbind,Length.classes)%>%
                          mutate(Small=case_when(Sp=="White shark"~200,
                                                 TRUE~Small),
                                 Medium=case_when(Sp=="White shark"~300,
                                                 TRUE~Medium))
  
  fn.plot.size.classes=function(sp,min.smap.size=25,siz.kol=Size.colors.size,dat.name,Klass)
  {
    d=Da%>%
      filter(COMMON_NAME==sp)%>%
      filter(!is.na(Mid.Lat))%>%
      left_join(Klass,by=c('COMMON_NAME'='Sp'))
    
    d=d%>%
      mutate(Class=case_when(TL<Small~paste("<",Small,'cm TL'),
                             TL>=Small & TL<= Medium ~paste(Small,'-',Medium,'cm TL'),
                             TL>Medium~paste(">",Medium,'cm TL')))
    Class.level=unique(d$Class)
    Class.level=c(Class.level[grep("<",Class.level)],Class.level[grep("-",Class.level)],Class.level[grep(">",Class.level)])
    d$Class=factor(d$Class,levels=Class.level)
    names(siz.kol)=levels(d$Class)
    if(sum(is.na(d$Class))==nrow(d))
    {
      d=d%>%
        mutate(Length=ifelse(grepl(paste(TL.species,collapse = '|'),COMMON_NAME),TL,FL))%>%
        filter(!is.na(Length))%>%
        mutate(Class=cut(Length,3))
      names(siz.kol)=levels(d$Class)
    }
    d=d%>%
      group_by(COMMON_NAME,SCIENTIFIC_NAME,Mid.Lat, Mid.Long,Class)%>%
      tally()%>%
      ungroup()
    
    if(nrow(d)>min.smap.size)
    {
      LIMY=c(min(d$Mid.Lat),max(d$Mid.Lat))
      LIMX=c(min(d$Mid.Long),max(d$Mid.Long))
      if(LIMY[2]<=.95*LIMY[1] | LIMX[2]<=1.005*LIMX[1])
      {
        LIMY[1]=LIMY[1]-1
        LIMY[2]=LIMY[2]+1
        
        LIMX[1]=LIMX[1]-1
        LIMX[2]=LIMX[2]+1
      }
      
      Base.map=fn.map(Limx=LIMX, Limy=LIMY, Depth.data=Bathymetry,add.depth=FALSE,add.parks=FALSE,
                      FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
      
      p1=Base.map+
        geom_point(data=d,aes(Mid.Long,Mid.Lat, size=n,fill=Class),
                   alpha=.5,color='black',shape = 21)+
        facet_wrap(~Class,nrow=1)+
        theme_PA()+
        theme(legend.position = 'top',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
        scale_fill_manual(values=siz.kol,drop=FALSE)+
        guides(fill = "none")+
        ggtitle(unique(d$SCIENTIFIC_NAME))
      
      print(p1)
      Height=Width=6
      if((LIMY[2]-LIMY[1]) >= 2*(LIMX[2]-LIMX[1] ))
      {
        Height=8
        Width=5
      }
      Width=8
      ggsave(paste0(hndl.out,"Outputs/z_neonates/",paste(dat.name,sp,sep='_'),".tiff"),width = Width,height = Height,compression = "lzw")
      
    }
    
  }
  for(s in 1:length(Unik.Sp))
  {
    print(paste('--------------plot Size classes for -----',Unik.Sp[s]))
    fn.plot.size.classes(sp=Unik.Sp[s],
                         dat.name='Size classes/',
                         Klass=Size.classes[s,])
  }
}




# Extract species and effort for ISRA crew specified polygons -----------------------------------------------------
squares_vertices=squares_vertices%>% 
                    filter(!vertex_index==0)
n.square.vec=unique(squares_vertices$id)
squares_vertices.list=vector('list',length(n.square.vec))  
names(squares_vertices.list)=n.square.vec

for(i in 1:length(n.square.vec))
{
  dd=squares_vertices%>%filter(id==n.square.vec[i])
  
  squares_vertices.list[[i]]=data.frame(id=unique(dd$id),
                                        Lat.min=min(dd$y),
                                        Lat.max=max(dd$y),
                                          Lon.min=min(dd$x),
                                          Lon.max=max(dd$x))
}

#Range restricted thru time
for(i in 1:length(n.square.vec))
{
  Poly=squares_vertices.list[[i]]
  DAT=Temporal.WA_all%>%
    filter(lat>=Poly$Lat.min & lat<=Poly$Lat.max)%>%
    filter(lon>=Poly$Lon.min & lon<=Poly$Lon.max)%>%
    left_join(all_spp%>%dplyr::select(Scientific.name,Common.name,Cri_B),by='Scientific.name')%>%
    mutate(Data.type=ifelse(Data.type=="Fisheries coarse","Fisheries",Data.type))
  
  DAT=DAT%>%
    group_by(Year,Scientific.name,Cri_B,Data.type)%>%
    summarise(N=sum(n_obs))%>%
    ungroup()
  write.csv(DAT%>%rename('Number of individuals'=N),
            paste0(hndl.out,"Outputs/Species and effort for ISRA crew specified polygons/Observations_id_",Poly$id,".csv"),
            row.names = F)
  
  p=DAT%>%
    ggplot(aes(Year,Scientific.name,color=Cri_B))+
    geom_point(size=2)+geom_line(linetype='dotted')+
    geom_point(aes(size=N))+
    facet_wrap(~Data.type)+
    theme_PA(Sbt.siz=12,cap.siz=9)+
    theme(legend.position = 'bottom',
          axis.text.y = element_text(face='italic'),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))+
    ylab('Scientific name')+
    labs(title='Reported occurrence',
         subtitle='(Sourced from the national SDMP database)',
         caption='SDMP, Species Distribution Modelling Project')+
    geom_vline(xintercept = 2009)
  print(p)
  Height=6
  if(length(unique(DAT$Scientific.name))>50) Height=12 
  Width=8
  ggsave(paste0(hndl.out,"Outputs/Species and effort for ISRA crew specified polygons/Polygon ",Poly$id,"_occurrence_range restricted",
                ".tiff"),width = Width,height = Height,compression = "lzw")
}

#Neonates thru time
if(do.observer)
{
  for(i in 1:length(n.square.vec))
  {
    Poly=squares_vertices.list[[i]]
    DAT=Da%>%
      mutate(lat=Mid.Lat,
             lon=Mid.Long)%>%
      filter(lat>=Poly$Lat.min & lat<=Poly$Lat.max)%>%
      filter(lon>=Poly$Lon.min & lon<=Poly$Lon.max)%>%
      rename(Scientific.name=SCIENTIFIC_NAME)
    if(nrow(DAT))
    {
      DAT=DAT%>%
        left_join(all_spp%>%dplyr::select(Scientific.name,Common.name,Cri_B),by='Scientific.name')%>%
        mutate(Class=as.character(Class),
               Class=ifelse(is.na(Class),'No growth pars',Class))%>%
        filter(!Class=='Other')%>%
        rename(Year=year)%>%
        mutate(Class=factor(Class,levels=c("Neonate","YOY","No growth pars")))
      
      p=DAT%>%
        group_by(Year,Scientific.name,Class)%>%
        summarise(N=sum(Numbers))%>%
        ungroup()%>%
        ggplot(aes(Year,Scientific.name))+
        geom_point(size=2)+geom_line(linetype='dotted')+
        geom_point(aes(size=N))+
        facet_wrap(~Class)+
        theme_PA(Sbt.siz=12,cap.siz=9)+
        theme(legend.position = 'bottom',
              axis.text.y = element_text(face='italic'),
              axis.text.x = element_text(angle = 90),
              plot.title.position = "plot",
              plot.caption.position = "plot",
              plot.caption = element_text(hjust = 0))+
        ylab('Scientific name')+
        geom_vline(xintercept = 2009)
      print(p)
      Height=6
      if(length(unique(DAT$Scientific.name))>50) Height=12 
      Width=8
      ggsave(paste0(hndl.out,"Outputs/Species and effort for ISRA crew specified polygons/Polygon ",Poly$id,"_neonates",
                    ".tiff"),width = Width,height = Height,compression = "lzw")
      
    }
    
  }
}

#Effort  

#map it
p=fn.map(Limx=c(112,130), Limy=c(-36,-13), Depth.data=Bathymetry,add.depth=TRUE,add.parks=FALSE,
         FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
for(i in 1:length(n.square.vec))
{
  d=with(squares_vertices.list[[i]],data.frame(y=c(Lat.min,Lat.min,Lat.max,Lat.max),
                                               x=c(Lon.min,Lon.max,Lon.max,Lon.min)))%>%
    mutate(Label=squares_vertices.list[[i]]$id)
  p=p+geom_polygon(data=d,aes(x,y),color='red',fill='red',alpha=.1)+
    geom_text_repel(data=d[2,],aes(x=x,y=y,label =Label),box.padding=1.5)
}
print(p)

#Export effort
for(i in 1:length(n.square.vec))
{
  Poly=squares_vertices.list[[i]]
  DAT.eff=Effort_WA%>%filter(id==Poly$id)%>%
    mutate(Data.type=ifelse(Data.type=="Fisheries coarse","Fisheries",Data.type))%>%
    mutate(Gear=case_when(grepl(paste(c('ft','pt'),collapse='|'),tolower(Gear))~'Trawl',
                          grepl(paste(c('trawl','tw'),collapse='|'),tolower(Gear))~'Trap',
                          grepl(paste(c('net','gn'),collapse='|'),tolower(Gear))~'Gillnet',
                          grepl(paste(c('line','ll','tl','dl','gl','hl','pl'),collapse='|'),tolower(Gear))~'Longline',
                          TRUE~Gear))%>%
    filter(!is.na(Shot.id))
  if(nrow(DAT.eff)>0)
  {
    Eff=DAT.eff%>%
      group_by(Year,Data.type,Fishery,Gear)%>%
      tally()%>%
      ungroup()%>%
      rename(N.shots=n)%>%
      filter(!Gear%in%c('','BH','FG','HR','Unknown fishing gear from Biodata','Detections','Telemetry'))%>%
      filter(!is.na(Gear))%>%
      dplyr::select(-Fishery)%>%
      mutate(id=Poly$id)
    write.csv(Eff,paste0(hndl.out,"Outputs/Species and effort for ISRA crew specified polygons/Effort_id_",Poly$id,".csv"),
              row.names = F)
    
    
  }
}
  

# 80 mile beach - Al-------------------------------------------------------------------------
if(do.observer)
{
  eighty.mile.beach=DATA%>%
    filter(Mid.Lat>=(-20) & Mid.Lat<=(-17.5))%>%
    filter(Mid.Long>=120 & Mid.Long<=123)%>%
    filter(Method=='GN')%>%
    filter(Taxa=='Elasmobranch')%>%
    dplyr::select(SHEET_NO,LINE_NO,date,Mid.Lat,Mid.Long,Method,MESH_SIZE,SOAK.TIME,NET_LENGTH,
                  COMMON_NAME,SCIENTIFIC_NAME,CAAB_code,TL,FL,SEX)
    write.csv(eighty.mile.beach,paste0(hndl.out,"Outputs/eighty.mile.beach.csv"),row.names = F)
}
# Gulf of Carpentaria -----------------------------------------------------
do.this=FALSE
if(do.this)
{
  All_grids_Carpentaria=All_grids_Carpentaria%>%
    rename(Scientific.name=Scientific.name.cleaned)%>%
    left_join(all_spp%>%dplyr::select(Scientific.name,Common.name,IUCN.global.status,Cri_B),
              by='Scientific.name')%>%
    mutate(lat=as.numeric(sub(".*?_", "", str_remove(grid_id,'grid_'))),
           lon=as.numeric(sub("_.*", "",str_remove(grid_id,'grid_'))))
  
  #Map glyphis and pristis
  d=All_grids_Carpentaria%>%
    filter(grepl(paste(c('Pristis','Glyphis','Anoxypristis','Sphyrna','Eusphyra'),collapse='|'),Scientific.name))
  
  LIMY=c(min(d$lat),max(d$lat))
  LIMX=c(min(d$lon),max(d$lon))
  Base.map=fn.map(Limx=LIMX, Limy=LIMY, Depth.data=Bathymetry,add.depth=FALSE,add.parks=FALSE,
                  FishClose.col='cyan2',ASL.col='cadetblue4',Comm.col='chartreuse4',State.col='aquamarine3',alpha.parks=.5)
  
  d1=d%>%
    group_by(lon,lat,Scientific.name)%>%
    summarise(n=sum(n_obs))%>%
    ungroup()
  dis.shrk=unique(d1$Scientific.name)
  for(i in 1:length(dis.shrk))
  {
    NM=dis.shrk[i]
    Tab=d%>%filter(Scientific.name==NM)%>%group_by(Year)%>%summarise(n=sum(n_obs))
    p=Base.map+
      geom_point(data=d1%>%filter(Scientific.name==NM),aes(lon,lat,size=n))+
      theme_PA()+ggtitle(NM)+
      theme(legend.position = 'bottom',
            plot.title = element_text(face = "italic"))+
      guides(size = guide_legend(title = '',nrow = 1)) #paste0('n=',sum(Tab$n),' records')
    
    print(p)
    ggsave(paste0(hndl.out.Carp,"Outputs/",NM,".tiff"),width = Width,height = Height,compression = "lzw")
    write.csv(Tab,paste0(hndl.out.Carp,"Outputs/table_",NM,".csv"),row.names = F)
    
  }
  
  #for Marta
  # Northwest: 140.8731925°E 12.2404036°S 
  # Northeast:  141.9804993°E 12.2725198°S 
  # Southwest:  140.8479115°E 13.0396485°S
  # Southeast:  141.9628026°E 13.0741268°S 
  Marta=All_grids_Carpentaria%>%
    filter(lat>=(-13.08) & lat<=(-12.2))%>%
    filter(lon>=(140.8) & lon<=(142))%>%
    group_by(Scientific.name,Year)%>%
    summarise(n.individuals=sum(n_obs))
  write.csv(Marta,paste0(hndl.out.Carp,"Outputs/Tabla_Marta.csv"),row.names = F)
  
}

# NSW -----------------------------------------------------
do.this=FALSE
if(do.this)
{
  All_grids_NSW=All_grids_NSW%>%
    mutate(lat=as.numeric(sub(".*?_", "", str_remove(grid_id,'grid_'))),
           lon=as.numeric(sub("_.*", "",str_remove(grid_id,'grid_'))))
  
  # Chris Rohner <chrisrohner@gmail.com>
  Chris.Rohner.in=list(Richmond.River=data.frame(lat.min=-28.913403,lat.max=-28.838166,lon.min=153.508421,lon.max=153.585692),
                       Hastings.River=data.frame(lat.min=-31.444368,lat.max=-31.390751,lon.min=152.845986,lon.max=152.947953),
                       Manning.River=data.frame(lat.min=-31.986770,lat.max=-31.854163,lon.min=152.536655,lon.max=152.680808))
  Chris.Rohner.out=Chris.Rohner.in
  
  delta1=1.001
  delta2=0.999
  for(i in 1:length(Chris.Rohner.in))
  {
    dd=Chris.Rohner.in[[i]]
    Chris.Rohner.out[[i]]=All_grids_NSW%>%
      filter(lat>=dd$lat.min*delta1 & lat<=dd$lat.max*delta2)%>%
      filter(lon>=dd$lon.min*delta2 & lon<=dd$lon.max*delta1)
  }
  
  p=All_grids_NSW%>%
    distinct(lon,lat)%>%
    ggplot(aes(lon,lat))+
    geom_point(x=Chris.Rohner.in$Richmond.River$lon.min,y=Chris.Rohner.in$Richmond.River$lat.min,size=4,color='orange')+
    geom_point(x=Chris.Rohner.in$Hastings.River$lon.min,y=Chris.Rohner.in$Hastings.River$lat.min,size=4,color='orange')+
    geom_point(x=Chris.Rohner.in$Manning.River$lon.min,y=Chris.Rohner.in$Manning.River$lat.min,size=4,color='orange')+
    geom_text(x=Chris.Rohner.in$Richmond.River$lon.min,y=Chris.Rohner.in$Richmond.River$lat.min,label='Richmond.River',size=4)+
    geom_text(x=Chris.Rohner.in$Hastings.River$lon.min,y=Chris.Rohner.in$Hastings.River$lat.min,label='Hastings.River',size=4)+
    geom_text(x=Chris.Rohner.in$Manning.River$lon.min,y=Chris.Rohner.in$Manning.River$lat.min,label='Manning.River',size=4)+
    geom_point(alpha=.5)+
    geom_polygon(data=with(Chris.Rohner.in$Richmond.River,data.frame(y=c(lat.min,lat.min,lat.max,lat.max),x=lon.min,lon.max,lon.max,lon.min)),
                 aes(x,y),color='red')+
    geom_polygon(data=with(Chris.Rohner.in$Hastings.River,data.frame(y=c(lat.min,lat.min,lat.max,lat.max),x=lon.min,lon.max,lon.max,lon.min)),
                 aes(x,y),color='red')+
    geom_polygon(data=with(Chris.Rohner.in$Manning.River,data.frame(y=c(lat.min,lat.min,lat.max,lat.max),x=lon.min,lon.max,lon.max,lon.min)),
                 aes(x,y),color='red')+
    xlim(152,154)+ylim(-32.5,-28)
  p+
    geom_polygon(data=with(Chris.Rohner.in$Richmond.River,data.frame(y=c(lat.min*delta1,lat.min*delta1,lat.max*delta2,lat.max*delta2),
                                                                     x=c(lon.min*delta2,lon.max*delta1,lon.max*delta1,lon.min*delta2))),
                 aes(x,y),fill='blue',alpha=.1)+
    geom_polygon(data=with(Chris.Rohner.in$Hastings.River,data.frame(y=c(lat.min*delta1,lat.min*delta1,lat.max*delta2,lat.max*delta2),
                                                                     x=c(lon.min*delta2,lon.max*delta1,lon.max*delta1,lon.min*delta2))),
                 aes(x,y),fill='blue',alpha=.1)+
    geom_polygon(data=with(Chris.Rohner.in$Manning.River,data.frame(y=c(lat.min*delta1,lat.min*delta1,lat.max*delta2,lat.max*delta2),
                                                                    x=c(lon.min*delta2,lon.max*delta1,lon.max*delta1,lon.min*delta2))),
                 aes(x,y),fill='blue',alpha=.1)
  
}

#Range restricted species
Vic=All_grids_NSW%>%
  rename(Scientific.name=Scientific.name.cleaned)%>%
  filter(Scientific.name%in%c('Dipturus endeavouri','Dentiraja australis','Dipturus grahami',
                                      'Urolophus sufflavus','Urolophus kapalensis','Parascyllium collare'))

Tab.records=Vic%>%
  group_by(Scientific.name,Year)%>%
  summarise(N=sum(n_obs))
write.csv(Tab.records,paste0(hndl.out.NSW,'Total number of records.csv'),row.names=F)


Vic.poly=Vic%>%filter(lat<=(-32.5) &  lat>=(-34.5))

Tab.records.poly=Vic.poly%>%
  group_by(Scientific.name,Year)%>%
  summarise(N=sum(n_obs))
write.csv(Tab.records.poly,paste0(hndl.out.NSW,'Total number of records_polygon.csv'),row.names=F)


#All species
Vic.all.sp=All_grids_NSW%>%
  rename(Scientific.name=Scientific.name.cleaned)%>%
  filter(lat<=(-32.5) &  lat>=(-34.5))

Tab.records.poly.all.sp=Vic.all.sp%>%
  group_by(Scientific.name,Year)%>%
  summarise(N=sum(n_obs))
write.csv(Tab.records.poly.all.sp,paste0(hndl.out.NSW,'Total number of records_polygon_all species.csv'),row.names=F)

