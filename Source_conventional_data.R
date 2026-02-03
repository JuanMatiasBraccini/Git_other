                    #SOURCE CONVENTIONAL TAGGING DATA#

#new: add "FL" in full_join(Tagging,subset( to deal with released recaptures

library(RODBC)    		#library for importing excel data
library(lubridate)
library(dplyr)
library(Hmisc)
if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

# DATA SECTION -----------------------------------------------------------------------

#WA sharks

  #Sharks.mdb data base
setwd("M:/Production Databases/Shark") 
#setwd("//fish.wa.gov.au/Data/Production Databases/Shark") 
Dat.Beis<-'Sharks v20220906.mdb'
#Dat.Beis<-'Sharks v20240820 UAT.mdb' #new database updated by Vero's team.. Previous: 'Sharks v20220906.mdb'  'Sharks.mdb'
channel <- odbcConnectAccess2007(Dat.Beis)  
Tagging=sqlFetch(channel, "Tag data", colnames = F) 
Tag.recaptures=sqlFetch(channel, "Tag recaptures", colnames = F) 
Boat_hdr=sqlFetch(channel, "Boat_hdr", colnames = F) 
Boat_bio=sqlFetch(channel, "Boat_bio", colnames = F) 
Flinders_hdr=sqlFetch(channel, "FLINDERS HDR", colnames = F) 
close(channel)  

#standardise sheetNo
Tagging$SHEET_NO=tolower(Tagging$SHEET_NO)
Boat_hdr$SHEET_NO=tolower(Boat_hdr$SHEET_NO)
Boat_bio$SHEET_NO=tolower(Boat_bio$SHEET_NO)
Flinders_hdr$SHEET_NO=tolower(Flinders_hdr$SHEET_NO)


setwd(handl_OneDrive("Data/Tagging/Conventional_tagging"))
Tagging.check=read.csv(handl_OneDrive("Data/Tagging/Conventional_tagging/Boat_bio tag releases.csv"))

#Gummy shark
GummyWA=read.csv("Terry_data/InWA.csv")
GummySA=read.csv("Terry_data/OutWA.intoWA.csv")


#Species codes 
Species.Codes=read.csv(handl_OneDrive("Data/Species.code.csv"),stringsAsFactors=FALSE, fileEncoding="latin1")
Species.Size.Range=read.csv(handl_OneDrive("Data/Species.Size.Range.csv"))      

# PARAMETERS SECTION -----------------------------------------------------------------------
Min.biological.length=20

# PROCEDURE SECTION -----------------------------------------------------------------------

#Early manipulations
these.hdr=c('SHEET_NO','Method','DATE','BOTDEPTH','BOAT','MID.LAT','MID.LONG')
Flinders_hdr=Flinders_hdr%>%
              dplyr::rename(DATE='DATE SET',
                     BOTDEPTH='Avg DEPTH')%>%
              mutate(Method="DL",
                     BOAT='Flinders',
                     MID.LONG= END1LNGD+(END1LNGM/60),
                     MID.LAT= END1LATD+(END1LATM/60),
                     MID.LAT=-abs(MID.LAT))
Boat_hdr=Boat_hdr%>%
            dplyr::rename(MID.LONG='MID LONG',
                   MID.LAT='MID LAT')%>%
            mutate(MID.LAT=-abs(MID.LAT))

Flinders_hdr=subset(Flinders_hdr,select=these.hdr)
Boat_hdr=subset(Boat_hdr,select=these.hdr)
Boat_hdr$Method=as.character(Boat_hdr$Method)
Gear=rbind(Boat_hdr,Flinders_hdr)


#Make gummy variables compatible
b.w=8.891; a.w=1.046  #total to fork length (used the inverse of whiskery as no data available)
b.g=4.6424; a.g=1.08
Gummy=rbind(GummyWA,GummySA)%>%
        mutate(Sp="GM",
               SEX=ifelse(X==1,"M",ifelse(X==2,"F","U")),
               SPECIES="GM",
               SHEET_NO="Dummy",
               Yr.rel=YrRl,
               Mn.rel=MnRl,
               Day.rel=DyRl,
               Yr.rec=YrRc,
               Mn.rec=MnRc,
               Day.rec=DyRc,
               Recaptured=ifelse(is.na(YrRc),"NO","YES"),
               CONDITION=C,
               Tag.no=TAG1NO,
               FINTAGNO=TAG1NO,
               FL=round(((TLRl-b.g)/a.g)/10),
               CAP_FL=round(((TLRc-b.g)/a.g)/10),
               ATAG.NO=NA,
               Lat.rec=-abs(LatRc),
               Long.rec=LonRc,
               Lat.rels=-abs(LatRl),
               Long.rels=LonRl,
               Tag.type="conventional",
               BOTDEPTH=NA,
               BOAT=NA)
#Gummy$"RELEASE.DATE"=as.POSIXct(with(Gummy,paste(YrRl,"-",MnRl,"-",DyRl,sep="")))
#Gummy$DATE_CAPTR=with(Gummy,ifelse(!is.na(YrRc),paste(YrRc,"-",MnRc,"-",DyRc,sep=""),NA))
#Gummy$DATE_CAPTR=as.POSIXct(strptime(Gummy$DATE_CAPTR,format="%Y-%m-%d"))
Terry.gear.code=data.frame(GrRl=1:16,
                           GrRc=1:16,
                           Method=c("LL","GN","non-shark monfilament","GN",
                                    'Tassie graball net','OT','Danish seine',
                                    'PT','Snapper LL','Trotline','DL','Troll',
                                    'HL','Lobster pot','Fish trap','Unknown'))
Gummy=Gummy%>%
      left_join(Terry.gear.code%>%dplyr::select(GrRc,Method)%>%rename(CAPT_METHD=Method),
                by='GrRc')%>%
      left_join(Terry.gear.code%>%dplyr::select(GrRl,Method),
                by='GrRl')

#release and recapture methods 
Gummy=Gummy%>%
  mutate(DARTTAGNO=NA,
         Rec.method=ifelse(CAPT_METHD=="GN","Commercial gillnet",
                    ifelse(CAPT_METHD=="LL","Commercial longline",
                    ifelse(!CAPT_METHD%in%c("LL","GN"),"Other",
                    NA))),
         Rel.method=ifelse(Method=='LL',"Commercial longline",
                    ifelse(Method=='GN',"Commercial gillnet",
                    ifelse(!Method%in%c('LL','GN'),"Other",
                    NA))))

#add missing records
ind=which(is.na(match(Tagging.check$"FINTAG NO",Tagging$FINTAGNO))==T)
if(sum(ind)>0)
  {
    not.in.Tagging=Tagging.check[ind,]
    write.csv(not.in.Tagging,file="not_in_Tag_Data_data.csv")
  
    dummy=Tagging[1:nrow(not.in.Tagging),]
    dummy[,]=NA
    dummy$"SHEET_NO"=not.in.Tagging$Boat_hdr_SHEET_NO
    dummy$SPECIES=not.in.Tagging$SPECIES
    dummy$FINTAGNO=not.in.Tagging$"FINTAG NO"
    dummy$"FINTAG.2"=not.in.Tagging$"FINTAG 2"
    dummy$FL=not.in.Tagging$FL
    dummy$SEX=not.in.Tagging$SEX
    dummy$CONDITION=not.in.Tagging$"RELEASE CONDITION"
    dummy$REL_LATD=not.in.Tagging$END1LATD
    dummy$REL_LATM=not.in.Tagging$END1LATM
    dummy$REL_LNGD=not.in.Tagging$END1LNGD
    dummy$REL_LNGM=not.in.Tagging$END1LNGM
    dummy$"RELEASE.DATE"=not.in.Tagging$DATE
    dummy$"Tag.no"=paste("F",dummy$FINTAGNO,sep="")
    
    a=rep(NA,ncol(Tagging));for(i in 1:ncol(Tagging))a[i]=(class(Tagging[,i]))
    id=which(a=="factor")
    for (i in 1:length(id))Tagging[,id[i]]=as.character(Tagging[,id[i]])
    for (i in 1:length(id))dummy[,id[i]]=as.character(dummy[,id[i]])
    
    Tagging=rbind(Tagging,dummy)
  }

#convert all factors to character
Tagging %>% mutate(across(where(is.factor), as.character)) -> Tagging  
Boat_bio %>% mutate(across(where(is.factor), as.character)) -> Boat_bio
Gummy %>% mutate(across(where(is.factor), as.character)) -> Gummy
Species.Codes %>% mutate(across(where(is.factor), as.character)) -> Species.Codes

#Fix Tag.no if incomplete
Tagging=Tagging%>%
  dplyr::rename(Tag.no2='Tag no',
         ATAG.NO="ATAG NO",
         RELEASE.DATE="RELEASE DATE")%>%
  mutate(Tag.no=as.character(FINTAGNO))

#Tag type
Tagging=Tagging%>%
  dplyr::rename(Recaptured="Captured?")%>%
        mutate(Recaptured=ifelse(!is.na(CAPT_METHD),'Y',Recaptured),
               Recaptured=case_when(is.na(CAPT_METHD) & (!is.na(CAP_LATD) | !is.na(CAP_LNGD))~'Y',
                                    is.na(CAPT_METHD) & (!is.na(year(DATE_CAPTR)) | !is.na(month(DATE_CAPTR)))~'Y',  
                                    TRUE~Recaptured),
               CONDITION=ifelse(CONDITION=="?",NA,CONDITION),
               Tag.type=case_when(!is.na(ATAG.NO)~'acoustic',
                                  !is.na(DARTTAGNO) & is.na(Tag.no)~'conventional.dart',
                                  TRUE~'conventional'))%>%
        mutate(Tag.no=ifelse(is.na(Tag.no) & !is.na(Tag.no2),Tag.no2,
                      ifelse(is.na(Tag.no) & !is.na(DARTTAGNO),DARTTAGNO,
                      ifelse(is.na(Tag.no) & !is.na(ATAG.NO),ATAG.NO,
                      Tag.no))),
                Tag.no=tolower(Tag.no))%>%
      dplyr::select(-Tag.no2)


#remove duplicated tags
Tagging=Tagging%>%
          filter(!is.na(Tag.no))


#release and recapture methods 
Res.ves=c('HAM','HOU','NAT','RV BREAKSEA','RV Gannet','RV GANNET','RV SNIPE 2','FLIN','naturaliste')
Tagging=Tagging%>%
        mutate(CAPTVESS=tolower(CAPTVESS),
               Rec.method=case_when(CAPT_METHD%in%c('LL','HK') & CAPTVESS %in% tolower(Res.ves) ~"Research longline",
                                    CAPT_METHD=='LL' & !CAPTVESS %in% tolower(Res.ves) ~"Commercial longline",
                                    CAPT_METHD=='GN' & CAPTVESS %in% tolower(Res.ves) ~"Research gillnet",
                                    CAPT_METHD=='GN' & !CAPTVESS %in% tolower(Res.ves) ~"Commercial gillnet",
                                    !CAPT_METHD%in%c("LL","GN") & !is.na(CAPT_METHD)~"Other"),
               Rec.method=ifelse(is.na(Recaptured),NA,Rec.method))%>%
        left_join(Boat_hdr%>%dplyr::select(SHEET_NO,Method,BOAT),by='SHEET_NO')

  #fill in missing capture method and boat for tagging SHEET_NO not in Boat_hdr 
Tagging=Tagging%>%
  mutate(RELLATDECDEG=case_when(is.na(RELLATDECDEG) & SHEET_NO=='r01065' ~19.97,
                                is.na(RELLATDECDEG) & SHEET_NO=='t05015' ~32.015,
                                TRUE~RELLATDECDEG),
         RELLNGDECDEG=case_when(is.na(RELLNGDECDEG) & SHEET_NO=='r01065' ~119.79,
                                is.na(RELLNGDECDEG) & SHEET_NO=='t05015' ~128.63,
                                TRUE~RELLNGDECDEG),
         Method=case_when(is.na(Method) & SHEET_NO%in% c("r01066","r01067","r01068","r01070")~'GN',
                          is.na(Method) & grepl('t0',SHEET_NO)~'GN',
                          TRUE~Method),
         BOAT=case_when(is.na(BOAT) & SHEET_NO%in% c("r01066","r01067","r01068","r01070")~'some unkwn commercial',
                        is.na(BOAT) & grepl('t0',SHEET_NO)~'some unkwn commercial',
                        TRUE~BOAT),
         RELEASE.DATE=case_when(is.na(RELEASE.DATE) & SHEET_NO%in%c('t00434','t00435') ~ as.POSIXct('1999-02-25'),
                               is.na(RELEASE.DATE) & SHEET_NO%in%c('t03048') ~ as.POSIXct('1998-03-26'),
                               TRUE~RELEASE.DATE))

#Add TL to species where TL is measured but not FL and any tagging event not in Tagging
#note: The Tag data table in Shark.mdb got some what corrupted and is not capturing all tagging event post 2016
#      Hence, combine with Boat bio.....
TL.species=c('ZE','PC','FR','TN','PZ','PM','SR') 
Boat_bio=Boat_bio%>%
        dplyr::rename(Tag.no="FINTAG NO",
                   DARTTAGNO="DART TAG NO",
                   ATAG.NO="ATAG NO",
                   FINTAG.2="FINTAG 2",
                   CONDITION_Boat_bio="RELEASE CONDITION",
                   SEX_Boat_bio=SEX)%>%
        mutate(CONDITION_Boat_bio=ifelse(DeadFlag=='Yes',0,CONDITION_Boat_bio))%>%
        dplyr::select(SHEET_NO,SPECIES,FL,TL,Tag.no,DARTTAGNO,ATAG.NO,FINTAG.2,
                          CONDITION_Boat_bio,SEX_Boat_bio)%>%
        mutate(DARTTAGNO=ifelse(DARTTAGNO<100,NA,DARTTAGNO),
                   Tag.type2=case_when(!is.na(ATAG.NO)~'acoustic',
                                       !is.na(DARTTAGNO) & is.na(Tag.no)~'conventional.dart',
                                       TRUE~'conventional'),
                   Tag.no=ifelse(is.na(Tag.no) & !is.na(DARTTAGNO),paste("D",DARTTAGNO,sep=""),
                                ifelse(is.na(Tag.no) & !is.na(ATAG.NO),paste("A",ATAG.NO,sep=""),
                                Tag.no)))%>%
        filter(!is.na(Tag.no))%>%
        mutate(Tag.no=tolower(Tag.no))%>%
        dplyr::rename(SHEET_NO_Boat_bio=SHEET_NO)

#fix some duplicated tag numbers
Tagging=Tagging%>%
  mutate(Tag.no=case_when(Tag.no=='1284' & SPECIES=='MI'~'dart6078',
                          Tag.no=='a175a' & SPECIES=='GM'~'A29506',
                          Tag.no=='122' & SPECIES=='BW'~DARTTAGNO,
                          Tag.no=='1016' & SPECIES=='BW'~DARTTAGNO,
                          Tag.no=='1016' & SPECIES=='TK'~DARTTAGNO,
                          Tag.no=='2533' & SPECIES=='TK'~DARTTAGNO,
                          Tag.no=='d0203' & SHEET_NO=='w00124'~'D0203_1',
                          TRUE~Tag.no))
#remove duplicates
Tagging=Tagging%>%
          mutate(dupli=paste(RELEASE.DATE,SPECIES,Tag.no,FL,SEX,RELLATDECDEG,RELLNGDECDEG,Method,BOAT))%>%
          distinct(dupli,.keep_all = T)%>%
          dplyr::select(-dupli)

#combine tagging and boat_bio   
Tagging=Tagging%>%
            mutate(SEX=case_when(SEX%in%c('f')~'F',
                                 SEX%in%c('m')~'M',
                                 SEX%in%c('?','n','N','p')~NA_character_,
                                 TRUE~SEX))%>%
            left_join(Boat_bio%>%
                          mutate(SEX_Boat_bio=case_when(SEX_Boat_bio%in%c('f')~'F',
                                               SEX_Boat_bio%in%c('m')~'M',
                                               SEX_Boat_bio%in%c('?','n','N','p')~NA_character_,
                                               TRUE~SEX_Boat_bio),
                                 SP.tg=paste(SPECIES,Tag.no))%>%
                          mutate(dupli=paste(SHEET_NO_Boat_bio,SPECIES,FL,Tag.no,SEX_Boat_bio))%>%
                          distinct(dupli,.keep_all = T)%>%
                          dplyr::select(-dupli)%>%
                          filter(SP.tg%in%unique(paste(Tagging$SPECIES,Tagging$Tag.no)))%>%
                          dplyr:::select(SHEET_NO_Boat_bio,SPECIES,TL,FL,Tag.no,
                                         Tag.type2,CONDITION_Boat_bio,SEX_Boat_bio),
                              by=c("SHEET_NO"="SHEET_NO_Boat_bio","SPECIES","Tag.no","FL","SEX"="SEX_Boat_bio"))%>%
            mutate(FL=ifelse(is.na(FL) & !is.na(TL) & !SPECIES%in%TL.species,TL*.85,FL),
                   FL=ifelse(SPECIES%in%TL.species,TL,FL),
                   Tag.type=ifelse(is.na(Tag.type)&!is.na(Tag.type2),Tag.type2,Tag.type))%>%
            dplyr::select(-Tag.type2)%>%
            mutate(CONDITION=ifelse(is.na(CONDITION),CONDITION_Boat_bio,CONDITION))
dd=table(Tagging$Tag.no)
dd=subset(dd,dd>1)
Tagging=Tagging%>%
          mutate(drop.dup=ifelse(Tag.no %in% names(dd) & is.na(RELEASE.DATE),'yes','no'))%>%
          filter(drop.dup=='no')%>%
          dplyr::select(-drop.dup)

Tagging=Tagging%>%
      mutate(dupli=paste(RELEASE.DATE,SPECIES,Tag.no,FL,SEX,RELLATDECDEG,RELLNGDECDEG,Method,BOAT))%>%
      distinct(dupli,.keep_all = T)%>%
      dplyr::select(-dupli)


Gear1=Gear%>%
        distinct(SHEET_NO,.keep_all = T)%>%
        filter(SHEET_NO%in%unique(Tagging$SHEET_NO))%>%
  dplyr::rename(Method_hdr=Method,
               DATE_hdr=DATE,
               BOTDEPTH_hdr=BOTDEPTH,
               BOAT_hdr=BOAT,
               MID.LAT_hdr=MID.LAT,
               MID.LONG_hdr=MID.LONG)%>%  
        mutate(Day.rel_hdr=day(DATE_hdr),    
               Mn.rel_hdr=month(DATE_hdr),
               Yr.rel_hdr=year(DATE_hdr)) 
  
Tagging=Tagging%>%
            left_join(Gear1,by='SHEET_NO')%>%
            mutate(BOAT_hdr=ifelse(is.na(BOAT_hdr) & !is.na(BOAT), BOAT, BOAT_hdr),
                   Method_hdr=ifelse(is.na(Method_hdr) & !is.na(Method), Method, Method_hdr))


#Add Rel.method
Tagging=Tagging%>%
  mutate(Rel.method=case_when(Method=='LL' & !BOAT%in%Res.ves~"Commercial longline",
                              Method=='LL' &  BOAT%in%Res.ves~"Research longline",
                              Method=='GN' &  BOAT%in%Res.ves~"Research gillnet",
                              Method=='GN' & !BOAT%in%Res.ves~"Commercial gillnet",
                              TRUE~"Other"))

#check for duplicates  
Tagging$Unico=with(Tagging,paste(Tag.no,SPECIES,FL))
ind=which(duplicated(Tagging$Unico)==T)
Dup.Tags=Tagging$Unico[ind]
if(length(Dup.Tags)>0)Tagging=Tagging[!(duplicated(Tagging$Unico)),-match("Unico",names(Tagging))]


#select relevant tagging variables
Tagging=Tagging%>%
          mutate(Day.rel=day(RELEASE.DATE),   
                 Mn.rel=month(RELEASE.DATE),
                 Yr.rel=year(RELEASE.DATE),
                 Day.rec=day(DATE_CAPTR),
                 Mn.rec=month(DATE_CAPTR),
                 Yr.rec=year(DATE_CAPTR),
                 Day.rel=ifelse(is.na(Day.rel),Day.rel_hdr,Day.rel),
                 Mn.rel=ifelse(is.na(Mn.rel),Mn.rel_hdr,Mn.rel),
                 Yr.rel=ifelse(is.na(Yr.rel),Yr.rel_hdr,Yr.rel))

Tagging$CAP_FL=as.numeric(Tagging$CAP_FL)

these.vars=c("SHEET_NO","SPECIES","FINTAGNO","ATAG.NO","DARTTAGNO","Tag.no","Tag.type","FL","SEX",
             "CONDITION","REL_LATD","REL_LATM",
             "REL_LNGD","REL_LNGM","Day.rel","Mn.rel","Yr.rel",
             "CAPT_METHD","Recaptured","CAP_LATD","CAP_LATM","CAP_LNGD","CAP_LNGM","CAP_FL",
             "Day.rec","Mn.rec","Yr.rec",
             'RELLATDECDEG','RELLNGDECDEG','RECLATDECDEG','RECLNGDECDEG',
             'Method_hdr','BOTDEPTH_hdr','BOAT_hdr','MID.LAT_hdr','MID.LONG_hdr',
             'Rec.method','Rel.method')
Tagging=Tagging[,match(these.vars,names(Tagging))]%>%
              mutate(Recaptured=ifelse(Recaptured%in%c("Y","y"),"YES","NO"))


#combine degrees and minutes (already in decimales degrees)
Tagging$REL_LNGM=ifelse(Tagging$REL_LNGM>100,0,Tagging$REL_LNGM)  #remove nonsense
Tagging$Lat.rels=with(Tagging,-(REL_LATD+(REL_LATM)/100))
Tagging$Long.rels=with(Tagging,(REL_LNGD+(REL_LNGM)/100))
Tagging$Lat.rec=with(Tagging,-(CAP_LATD+(CAP_LATM)/100))
Tagging$Long.rec=with(Tagging,(CAP_LNGD+(CAP_LNGM)/100))

Tagging=Tagging%>%
          mutate(Long.rels=ifelse(is.na(Long.rels),RELLNGDECDEG,Long.rels),
                 Lat.rels=ifelse(is.na(Lat.rels),-abs(RELLATDECDEG),Lat.rels),
                 Long.rec=ifelse(is.na(Long.rec),RECLNGDECDEG,Long.rec),
                 Lat.rec=ifelse(is.na(Lat.rec),-abs(RECLATDECDEG),Lat.rec),
                 Long.rels=ifelse(is.na(Long.rels),MID.LONG_hdr,Long.rels),
                 Lat.rels=ifelse(is.na(Lat.rels),MID.LAT_hdr,Lat.rels))%>%
  dplyr::rename(Method=Method_hdr,
                 BOTDEPTH=BOTDEPTH_hdr,
                 BOAT=BOAT_hdr)

#drop redundant vars
Drop.this=c("REL_LNGD","REL_LNGM","CAP_LATD","CAP_LATM","CAP_LNGD","CAP_LNGM","REL_LATD","REL_LATM",
            'RELLATDECDEG','RELLNGDECDEG','RECLATDECDEG','RECLNGDECDEG',
            'MID.LAT_hdr','MID.LONG_hdr')
Tagging=Tagging[,-match(Drop.this,names(Tagging))]



#add gummy tagging data 
Tagging=rbind(Tagging,Gummy[,match(names(Tagging),names(Gummy))])


#add recaptures not entered in database  
Tagging=Tagging%>%
  mutate(Day.rec=case_when(Tag.no=='d3254' & Recaptured=='No'~28,
                           TRUE~Day.rec),
         Mn.rec=case_when(Tag.no=='d3254' & Recaptured=='No'~7,
                          TRUE~Mn.rec),
         Yr.rec=case_when(Tag.no=='d3254' & Recaptured=='No'~2024,
                          TRUE~Yr.rec),
         Lat.rec=case_when(Tag.no=='d3254' & Recaptured=='No'~-22.1783667,
                           TRUE~Lat.rec),
         Long.rec=case_when(Tag.no=='d3254' & Recaptured=='No'~113.7791167,
                            TRUE~Long.rec),
         CAPT_METHD=case_when(Tag.no=='d3254' & Recaptured=='No'~'rod and reel',
                              TRUE~CAPT_METHD),
         Rec.method=case_when(Tag.no=='d3254' & Recaptured=='No'~'Other',
                              TRUE~Rec.method),
         Recaptured=case_when(Tag.no=='d3254' & Recaptured=='No'~'Yes',
                              TRUE~Recaptured))


#Fix a few dodgy recapture locations and dates
ALL.These=c("R00255","R00256","R00257","R00258","R00259","R00260","R00261",
            "R00262","R00263","R00264","R00265","R00266","R00267","R00268")
Tagging$Yr.rel=with(Tagging,ifelse(SHEET_NO%in%c("R00502","R00504"),2002,
                                   ifelse(SHEET_NO%in%ALL.These,2000,
                                          ifelse(SHEET_NO%in%c("R00176","R00177","R00178",
                                                               "R00179","R00180","R00181","R00182","R00183","R00184"),1998,Yr.rel))))
Tagging$Mn.rel=with(Tagging,ifelse(SHEET_NO%in%ALL.These,5,
                                   ifelse(SHEET_NO%in%c("R00176","R00177","R00178","R00179","R00180","R00181"),9,
                                          ifelse(SHEET_NO%in%c("R00182","R00183","R00184"),10,Mn.rel))))

Tagging$Day.rel=with(Tagging,ifelse(SHEET_NO=="R00255",5,
                                    ifelse(SHEET_NO=="R00256",6,       
                                           ifelse(SHEET_NO=="R00257",7,
                                                  ifelse(SHEET_NO=="R00258",8,
                                                         ifelse(SHEET_NO=="R00259",9,
                                                                ifelse(SHEET_NO=="R00260",10,
                                                                       ifelse(SHEET_NO=="R00261",13,   
                                                                              ifelse(SHEET_NO=="R00262",14,   
                                                                                     ifelse(SHEET_NO=="R00263",15,                    
                                                                                            ifelse(SHEET_NO=="R00264",16,
                                                                                                   ifelse(SHEET_NO=="R00265",17,
                                                                                                          ifelse(SHEET_NO=="R00266",18,
                                                                                                                 ifelse(SHEET_NO=="R00267",19,
                                                                                                                        ifelse(SHEET_NO=="R00268",20,
                                                                                                                               ifelse(SHEET_NO=="R00176",25,      
                                                                                                                                      ifelse(SHEET_NO=="R00177",26,     
                                                                                                                                             ifelse(SHEET_NO=="R00178",27,   
                                                                                                                                                    ifelse(SHEET_NO=="R00179",28,        
                                                                                                                                                           ifelse(SHEET_NO=="R00180",29,    
                                                                                                                                                                  ifelse(SHEET_NO=="R00181",30,        
                                                                                                                                                                         ifelse(SHEET_NO=="R00182",2,        
                                                                                                                                                                                ifelse(SHEET_NO=="R00183",4,
                                                                                                                                                                                       ifelse(SHEET_NO=="R00184",5,Day.rel))))))))))))))))))))))))

#set 1900 year to NA
Tagging$Day.rel=with(Tagging,ifelse(Yr.rel==1900,NA,Day.rel))
Tagging$Mn.rel=with(Tagging,ifelse(Yr.rel==1900,NA,Mn.rel))
Tagging$Yr.rel=with(Tagging,ifelse(Yr.rel==1900,NA,Yr.rel))
Tagging=Tagging%>%
          mutate(Lat.rec=ifelse(Lat.rec==0,NA,Lat.rec),
                 Long.rec=ifelse(Long.rec==0,NA,Long.rec),
                 Day.rec=case_when(Recaptured=='YES' & is.na(Day.rec) & !is.na(Lat.rec) & !Lat.rec==0~1,
                                   Recaptured=='YES' & Tag.no=='2864' & is.na(Day.rec) & !is.na(CAP_FL)~1,
                            TRUE~Day.rec),
                 Mn.rec=case_when(Recaptured=='YES' & is.na(Mn.rec) & !is.na(Lat.rec) & !Lat.rec==0~Mn.rel,
                                  Recaptured=='YES' & Tag.no=='2864' & is.na(Mn.rec) & !is.na(CAP_FL)~Mn.rel,
                                   TRUE~Mn.rec),
                 Yr.rec=case_when(Yr.rec==1900~Yr.rel+1,
                                  Recaptured=='YES' & is.na(Yr.rec) & !is.na(Lat.rec) & !Lat.rec==0~Yr.rel+1,
                                  Recaptured=='YES' & Tag.no=='2864' & is.na(Yr.rec) & !is.na(CAP_FL)~Yr.rel+1,
                                  TRUE~Yr.rec))
ID=which(Tagging$Yr.rel==1900)
if(length(ID)>0) Tagging=Tagging[-ID,]
Tagging$Day.rec=with(Tagging,ifelse(Tag.no%in%c("239"),9,Day.rec))
Tagging$Mn.rec=with(Tagging,ifelse(Tag.no%in%c("239"),12,Mn.rec))
Tagging$Yr.rec=with(Tagging,ifelse(Tag.no%in%c("239"),1997,Yr.rec))

#Fix some locations
Tagging$Lat.rels=with(Tagging,ifelse(SHEET_NO=="J00746",-22.8,
                                     ifelse(SHEET_NO=="N00401",-20.5402,
                                            ifelse(SHEET_NO=="N00558",-24.417,
                                                   ifelse(SHEET_NO=="R00388" & Lat.rels<(-17),-16.211,Lat.rels)))))
Tagging$Long.rels=with(Tagging,ifelse(SHEET_NO=="N00558",113.21,
                                      ifelse(SHEET_NO=="N00597",113.1442,
                                             ifelse(SHEET_NO=="Q00053",115.742,
                                                    ifelse(SHEET_NO=="N00099",113.2563,
                                                           ifelse(SHEET_NO=="R00617",113.598,
                                                                  ifelse(SHEET_NO=="R00863",112.787,
                                                                         ifelse(SHEET_NO=="R00833",113.99,Long.rels))))))))

Tagging$Lat.rels=with(Tagging,ifelse(SPECIES=="BW" & Long.rels<114 & Lat.rels>-17.7,-21.765,Lat.rels))
Tagging=Tagging%>%
  mutate(Lat.rels=case_when(SHEET_NO=='m00087' & Lat.rels>(-22)~ -23.048,
                            SHEET_NO=='m00239' & Lat.rels<(-18)~ -18.45,
                            TRUE~Lat.rels),
         Long.rels=case_when(SHEET_NO=='r00863' & Long.rels>(112)~ 112.78,
                             TRUE~Long.rels),
         Long.rec=case_when(SHEET_NO=='t00207' & Long.rec<(100)~ 115.3,
                            SHEET_NO=='t00003' & Long.rec<(113)~ 115.35,
                            TRUE~Long.rec),
         Lat.rec=case_when(SHEET_NO=='n00409' & Lat.rec>(-22) & Lat.rec<(-20)~ -18.4,
                           TRUE~Lat.rec))


#change nonsense 'Recaptured?' column
#note: the database has hundreds of entries in the 'Recaptured?' column as 'Yes' but no recapture info.
#      so many additional recaptures is inconsistent with what was reported by McAuley et al, hence set to 'No'

# minimum data to accept a recapture: at least recapture date or location

Tagging=Tagging%>%
            mutate(CAP_FL=ifelse(CAP_FL<Min.biological.length,NA,CAP_FL),
                   dummy.CAP_FL=ifelse(is.na(CAP_FL),1,CAP_FL),
                   dummy.Day.rec=ifelse(is.na(Day.rec),1,Day.rec),
                   dummy.Mn.rec=ifelse(is.na(Mn.rec),1,Mn.rec),
                   dummy.Yr.rec=ifelse(is.na(Yr.rec),1,Yr.rec),
                   dummy.Lat.rec=ifelse(is.na(Lat.rec),1,abs(Lat.rec)),
                   dummy.Long.rec=ifelse(is.na(Long.rec),1,Long.rec),
                   dummy=dummy.CAP_FL*dummy.Day.rec*dummy.Mn.rec*dummy.Yr.rec*dummy.Lat.rec*dummy.Long.rec,
                   Recaptured=ifelse(Recaptured=="YES" & dummy==1,"NO",Recaptured),  
                   Lat.rec=ifelse(Recaptured=="NO",NA,Lat.rec),
                   Long.rec=ifelse(Recaptured=="NO",NA,Long.rec),
                   Long.rels=ifelse(SHEET_NO=='R00863',112.7868,
                             ifelse(SHEET_NO=='Q00241',122.6462,
                             ifelse(SHEET_NO=='Q00654',118.4068,
                             Long.rels))),
                   Lat.rels=ifelse(SHEET_NO=='M00087',-23.06767,
                            ifelse(SHEET_NO=='Q00241',-33.89615,
                            ifelse(SHEET_NO=='Q00654',-34.82442,
                            Lat.rels))),
                   Recaptured=capitalize(tolower(Recaptured)))%>%
            dplyr::select(-c(dummy,dummy.CAP_FL,dummy.Day.rec,dummy.Mn.rec,dummy.Yr.rec,dummy.Lat.rec,dummy.Long.rec))

Tagging=Tagging%>%
  mutate(CAPT_METHD=ifelse(!is.na(CAPT_METHD) & Recaptured=='No',NA,CAPT_METHD),
         Rec.method=ifelse(!is.na(Rec.method) & Recaptured=='No',NA,Rec.method))

#fix species names
Tagging$SPECIES=with(Tagging,ifelse(SPECIES=="LP","ZE",
                             ifelse(SPECIES=="DF","SD",
                                    SPECIES)))   #LP is zebra shark; DF is spurdog
Tagging$SPECIES=toupper(Tagging$SPECIES)
Tagging=subset(Tagging,!SPECIES=='SR') #one record of stingray with no other data, assumed to be typo

#change awkward names
colnames(Tagging)[match(c("SEX","SPECIES","FL"),names(Tagging))]=c("Sex","Species","Rel_FL")

#fix sex
Tagging$Sex=ifelse(Tagging$Sex%in%c("F","f"),"F",ifelse(Tagging$Sex%in%c("m","M"),"M","U"))


#Create useful variables
  #areas
eachArea=c("JASDGDLF.zone2","JASDGDLF.zone1","WCDGDLF","Closed","WANCSF","JANSF")
Tagging$Areas=with(Tagging,ifelse(Lat.rels<=(-26.5) & Lat.rels>=(-33) & Long.rels <=116,eachArea[3],
                    ifelse(Lat.rels<(-33) & Lat.rels>=(-41) & Long.rels <=116.5,eachArea[2],
                    ifelse(Lat.rels<(-31) & Lat.rels>=(-41) & Long.rels >116.5,eachArea[1],
                    ifelse(Long.rels>=114 & Long.rels<=123.75 & Lat.rels >=(-23),eachArea[5],
                    ifelse(Long.rels>123.75 & Lat.rels >=(-18),eachArea[6],
                    ifelse(Lat.rels>=(-26.5) & Long.rels <=114,eachArea[4],       
                    NA)))))))
Tagging$Areas=with(Tagging,ifelse(Lat.rels<=(-29) & Long.rels >129,"SA",Areas))
Tagging$Areas.rec=with(Tagging,ifelse(Lat.rec<=(-26.5) & Lat.rec>=(-33) & Long.rec <=116,eachArea[3],
                      ifelse(Lat.rec<(-33) & Lat.rec>=(-41) & Long.rec <=116.5,eachArea[2],
                      ifelse(Lat.rec<(-31) & Lat.rec>=(-41) & Long.rec >116.5,eachArea[1],
                      ifelse(Long.rec>=114 & Long.rec<=123.75 & Lat.rec >=(-23),eachArea[5],
                      ifelse(Long.rec>123.75 & Lat.rec >=(-18),eachArea[6],
                      ifelse(Lat.rec>=(-26.5) & Long.rec <=114,eachArea[4],       
                      NA)))))))
Tagging$Areas.rec=with(Tagging,ifelse(Lat.rec<=(-29) & Long.rec >129,"SA",Areas.rec))


#AREAS=unique(Tagging$Areas)
AREAS=eachArea
#ColorAreas=2:(length(AREAS)+1)
ColorAreas=c("red","green","blue","orange","brown","salmon") 
names(ColorAreas)=eachArea
LetrasAreas=LETTERS[1:length(ColorAreas)]
names(LetrasAreas)=names(ColorAreas)


#remove nonsene recapture and release FL
Tagging$Rel_FL=with(Tagging,ifelse(Rel_FL>600,Rel_FL/10,Rel_FL)) #FL entered in mm
Tagging$Rel_FL=ifelse(Tagging$Rel_FL>500,NA,Tagging$Rel_FL)
Tagging$CAP_FL=ifelse(Tagging$CAP_FL>500,NA,Tagging$CAP_FL)
Tagging$Rel_FL=ifelse(Tagging$Rel_FL==0,NA,Tagging$Rel_FL)
Tagging$Rel_FL=with(Tagging,ifelse(Rel_FL<Min.biological.length,NA,Rel_FL))   
Tagging$CAP_FL=with(Tagging,ifelse(CAP_FL<Min.biological.length,NA,CAP_FL))
Tagging$CAP_FL=with(Tagging,ifelse(CAP_FL<Rel_FL,NA,CAP_FL))



#Add species full name
Tagging=left_join(Tagging,Species.Codes%>%distinct(Species,.keep_all = T),by="Species")


#Remove unknown species
Tagging=Tagging%>%
          filter(!Species=="")%>%
          filter(!COMMON_NAME%in%c("","Unknown"))


#Check release and recapture size within species range                
Species.Size.Range=subset(Species.Size.Range,Species%in%Tagging$Species)

TG.sp=unique(Tagging$Species)
Wrong.size=vector('list',length(TG.sp))
names(Wrong.size)=TG.sp

for(x in 1:length(TG.sp))
{
  a=subset(Tagging,Species==TG.sp[x])
  RnG=subset(Species.Size.Range,Species==TG.sp[x])
  if(nrow(RnG)>0)
  {
    if(!is.na(RnG$FL_min)) {MIN=0.90*RnG$FL_min}else
      MIN=0.90*RnG$TL_min
    if(!is.na(RnG$FL_max)) {MX=1.1*RnG$FL_max}else
      MX=1.1*RnG$TL_max
    
    Wrong.Rel=NULL
    if(nrow(a)>0)
    {
      Min=subset(a,Rel_FL<MIN,select=c(SHEET_NO,Species,Rel_FL,CAP_FL))
      Max=subset(a,Rel_FL>MX,select=c(SHEET_NO,Species,Rel_FL,CAP_FL))
      if(nrow(Min)>0) Min$Error="Release_size_below_min_size"
      if(nrow(Max)>0) Max$Error="Release_size_above_max_size"
      if(nrow(Min)>0| nrow(Max)>0)Wrong.Rel=rbind(Min,Max)
    }
    
    Wrong.Rec=NULL
    a=subset(a,!is.na(CAP_FL))
    if(nrow(a)>0)
    {
      Min=subset(a,CAP_FL<MIN| CAP_FL<Rel_FL,select=c(SHEET_NO,Species,Rel_FL,CAP_FL))
      Max=subset(a,CAP_FL>MX ,select=c(SHEET_NO,Species,Rel_FL,CAP_FL))
      if(nrow(Min)>0)Min$Error="Capture_size_below_min_size_AND_OR_below_release_FL"
      if(nrow(Max)>0)Max$Error="Capture_size_above_max_size"
      if(nrow(Min)>0| nrow(Max)>0)Wrong.Rec=rbind(Min,Max)
    }

    
    Wrong.size[[x]]=rbind(Wrong.Rel,Wrong.Rec)
  }
}
a=do.call(rbind,Wrong.size)
a=a[!duplicated(paste(a$SHEET_NO,a$Species,a$Rel_FL)),]
#write.csv(a,"C:/Users/myb/Desktop/Fix.these.sizes.csv",row.names=F)   

rm(a,TG.sp)


#check for duplicates   
Tagging$Unico=with(Tagging,paste(Tag.no,Species,Rel_FL))
ind=which(duplicated(Tagging$Unico)==T)
Dup.Tags=Tagging$Unico[ind]
if(length(Dup.Tags)>0)Tagging=Tagging[!(duplicated(Tagging$Unico)),-match("Unico",names(Tagging))]




  

