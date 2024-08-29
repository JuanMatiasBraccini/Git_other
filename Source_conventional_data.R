                    #SOURCE CONVENTIONAL TAGGING DATA#

#new: add "FL" in full_join(Tagging,subset( to deal with released recaptures

library(RODBC)    		#library for importing excel data
library(lubridate)
library(dplyr)
library(Hmisc)

# DATA SECTION -----------------------------------------------------------------------

#WA sharks

  #Sharks.mdb data base
setwd("M:/Production Databases/Shark") 
#setwd("//fish.wa.gov.au/Data/Production Databases/Shark") 
Dat.Beis<-'Sharks v20240820 UAT.mdb' #new database updated by Vero's team.. Previous: 'Sharks v20220906.mdb'  'Sharks.mdb'
channel <- odbcConnectAccess2007(Dat.Beis)  
Tagging=sqlFetch(channel, "Tag data", colnames = F) 
Boat_hdr=sqlFetch(channel, "Boat_hdr", colnames = F) 
Boat_bio=sqlFetch(channel, "Boat_bio", colnames = F) 
Flinders_hdr=sqlFetch(channel, "FLINDERS HDR", colnames = F) 
close(channel)  

#standardise sheetNo
Tagging$SHEET_NO=tolower(Tagging$SHEET_NO)
Boat_hdr$SHEET_NO=tolower(Boat_hdr$SHEET_NO)
Boat_bio$SHEET_NO=tolower(Boat_bio$SHEET_NO)
Flinders_hdr$SHEET_NO=tolower(Flinders_hdr$SHEET_NO)

#remove duplicated tags
Tagging=Tagging[order(Tagging$"Tag no",Tagging$"RELEASE DATE"),]
Tagging=Tagging[!duplicated(Tagging$"Tag no"),]

if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

setwd(handl_OneDrive("Data/Tagging/Conventional_tagging"))


Tagging.check=read.csv(handl_OneDrive("Data/Tagging/Conventional_tagging/Boat_bio tag releases.csv"))

#Gummy shark
GummyWA=read.csv("Terry_data/InWA.csv")
GummySA=read.csv("Terry_data/OutWA.intoWA.csv")


#Species codes 
Species.Codes=read.csv(handl_OneDrive("Data/Species.code.csv"))     
Species.Size.Range=read.csv(handl_OneDrive("Data/Species.Size.Range.csv"))      




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
Gummy=rbind(GummyWA,GummySA)
Gummy$Sp="GM"
Gummy$SEX=with(Gummy,ifelse(X==1,"M",ifelse(X==2,"F","U")))
Gummy$SPECIES=Gummy$Sp
Gummy$SHEET_NO="Dummy"
#Gummy$"RELEASE.DATE"=as.POSIXct(with(Gummy,paste(YrRl,"-",MnRl,"-",DyRl,sep="")))
Gummy$Yr.rel=Gummy$YrRl
Gummy$Mn.rel=Gummy$MnRl
Gummy$Day.rel=Gummy$DyRl

Gummy$Yr.rec=Gummy$YrRc
Gummy$Mn.rec=Gummy$MnRc
Gummy$Day.rec=Gummy$DyRc

Gummy$Recaptured=with(Gummy, ifelse(is.na(YrRc),"NO","YES"))
#Gummy$DATE_CAPTR=with(Gummy,ifelse(!is.na(YrRc),paste(YrRc,"-",MnRc,"-",DyRc,sep=""),NA))
#Gummy$DATE_CAPTR=as.POSIXct(strptime(Gummy$DATE_CAPTR,format="%Y-%m-%d"))
Gummy$CONDITION=Gummy$C
Gummy$"Tag.no"=Gummy$TAG1NO
Gummy$FINTAGNO=Gummy$"Tag.no"

b.w=8.891; a.w=1.046  #total to fork length (used the inverse of whiskery as no data available)
b.g=4.6424; a.g=1.08
Gummy$FL=round(((Gummy$TLRl-b.g)/a.g)/10)
Gummy$CAP_FL=round(((Gummy$TLRc-b.g)/a.g)/10)

Gummy$CAPT_METHD=Gummy$GrRc
Gummy$CAPT_METHD=with(Gummy,ifelse(CAPT_METHD==1,"LL",ifelse(CAPT_METHD==2,"GN",
          ifelse(CAPT_METHD==6,"OT",ifelse(CAPT_METHD==13,"HL",ifelse(CAPT_METHD==16,"?","other"))))))

Gummy$"ATAG.NO"=NA

Gummy$Lat.rec=-Gummy$LatRc
Gummy$Long.rec=Gummy$LonRc
Gummy$Lat.rels=-Gummy$LatRl
Gummy$Long.rels=Gummy$LonRl

Gummy$Tag.type="conventional"

Gummy$Method=Gummy$CAPT_METHD
Gummy$BOTDEPTH=NA
Gummy$BOAT=NA


#release and recapture methods 
Gummy=Gummy%>%
  mutate(Rec.method=ifelse(CAPT_METHD=="GN","Commercial gillnet",
                    ifelse(CAPT_METHD=="LL","Commercial longline",
                    ifelse(!CAPT_METHD%in%c("LL","GN"),"Other",
                    NA))),
         Rel.method=ifelse(GrRl==2,"Commercial gillnet",
                    ifelse(GrRl==1,"Commercial longline",
                    ifelse(!GrRl%in%c(1,2),"Other",
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
        mutate(CONDITION=ifelse(CONDITION=="?",NA,CONDITION),
               Tag.type=case_when(!is.na(ATAG.NO)~'acoustic',
                                  !is.na(DARTTAGNO) & is.na(Tag.no)~'conventional.dart',
                                  TRUE~'conventional'))%>%
        mutate(Tag.no=ifelse(is.na(Tag.no) & !is.na(Tag.no2),Tag.no2,
                      ifelse(is.na(Tag.no) & !is.na(DARTTAGNO),DARTTAGNO,
                      ifelse(is.na(Tag.no) & !is.na(ATAG.NO),ATAG.NO,
                      Tag.no))),
                Tag.no=tolower(Tag.no))%>%
      dplyr::select(-Tag.no2)


#release and recapture methods 
Res.ves=c('HAM','HOU','NAT','RV BREAKSEA','RV Gannet','RV GANNET','RV SNIPE 2')
Tagging=Tagging%>%
  mutate(CAPTVESS=tolower(CAPTVESS),
         Rec.method=case_when(CAPT_METHD=='LL' & CAPTVESS%in%c('nat','naturaliste') ~"Research longline",
                              CAPT_METHD=='LL' & !CAPTVESS%in%c('nat','naturaliste') ~"Commercial longline",
                              CAPT_METHD=='GN' ~"Commercial gillnet",
                              !CAPT_METHD%in%c("LL","GN") ~"Other"),
         Rec.method=ifelse(is.na(Recaptured),NA,Rec.method))%>%
  left_join(Boat_hdr%>%dplyr::select(SHEET_NO,Method,BOAT),by='SHEET_NO')%>%
  mutate(Rel.method=case_when(Method=='LL' & !BOAT%in%Res.ves~"Commercial longline",
                              Method=='LL' &  BOAT%in%Res.ves~"Research longline",
                              Method=='GN' &  BOAT%in%Res.ves~"Research gillnet",
                              Method=='GN' & !BOAT%in%Res.ves~"Commercial gillnet",
                              TRUE~"Other"))


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

Tagging=full_join(Tagging,subset(Boat_bio,select=c(SHEET_NO_Boat_bio,SPECIES,TL,FL,Tag.no,
                                                   Tag.type2,CONDITION_Boat_bio,SEX_Boat_bio)),
                  by=c("SPECIES","Tag.no","FL"))%>%
            mutate(FL=ifelse(is.na(FL) & !is.na(TL) & !SPECIES%in%TL.species,TL*.85,FL),
                   FL=ifelse(SPECIES%in%TL.species,TL,FL),
                   Tag.type=ifelse(is.na(Tag.type)&!is.na(Tag.type2),Tag.type2,Tag.type))%>%
        dplyr::select(-Tag.type2)%>%
        mutate(SHEET_NO=ifelse(is.na(SHEET_NO),SHEET_NO_Boat_bio,SHEET_NO),
               CONDITION=ifelse(is.na(CONDITION),CONDITION_Boat_bio,CONDITION),
               SEX=ifelse(is.na(SEX),SEX_Boat_bio,SEX))
Tagging=Tagging[!duplicated(Tagging$Tag.no),]

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
  
Tagging=Tagging%>%left_join(Gear1,by='SHEET_NO') 



#check for duplicates   
Tagging$Unico=with(Tagging,paste(Tag.no,SPECIES,FL))
ind=which(duplicated(Tagging$Unico)==T)
Dup.Tags=Tagging$Unico[ind]
if(length(Dup.Tags)>0)Tagging=Tagging[!(duplicated(Tagging$Unico)),-match("Unico",names(Tagging))]


#subset tagging data
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
Gummy$DARTTAGNO=NA
Tagging=rbind(Tagging,Gummy[,match(names(Tagging),names(Gummy))])



#change nonsense recapture data
Tag.nonsense.rec=c(476,3021)
these.nonsense.rec=match(Tag.nonsense.rec,Tagging$FINTAGNO)
na.these.cols=match(c("Lat.rec","Long.rec"),names(Tagging))
Tagging[these.nonsense.rec,na.these.cols]=NA  #set to NA the recapture location

Tagging=Tagging%>%
            mutate(CAP_FL=ifelse(CAP_FL<20,NA,CAP_FL),
                   dummy.CAP_FL=ifelse(is.na(CAP_FL),1,CAP_FL),
                   dummy.Day.rec=ifelse(is.na(Day.rec),1,Day.rec),
                   dummy.Mn.rec=ifelse(is.na(Mn.rec),1,Mn.rec),
                   dummy.Yr.rec=ifelse(is.na(Yr.rec),1,Yr.rec),
                   dummy.Lat.rec=ifelse(is.na(Lat.rec),1,abs(Lat.rec)),
                   dummy.Long.rec=ifelse(is.na(Long.rec),1,Long.rec),
                   dummy=dummy.CAP_FL*dummy.Day.rec*dummy.Mn.rec*dummy.Yr.rec*dummy.Lat.rec*dummy.Long.rec,
                   Recaptured=ifelse(Recaptured=="YES" & dummy<=1,"NO",Recaptured),
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

#fix species names
#Tagging$SPECIES=with(Tagging,ifelse(SPECIES%in%c("WC","WD","WS","WW"),"WB",SPECIES)) #Identification of wobbegongs was found to be unreliable so set all wobbies to general
Tagging$SPECIES=with(Tagging,ifelse(SPECIES=="LP","ZE",
                             ifelse(SPECIES=="DF","SD",
                                    SPECIES)))   #LP is zebra shark; DF is spurdog

Tagging$SPECIES=toupper(Tagging$SPECIES)

Tagging=subset(Tagging,!SPECIES=='SR') #one record of stingray with no other data, assumed to be typo

#change awkward names
colnames(Tagging)[match(c("SEX","SPECIES","FL"),names(Tagging))]=
  c("Sex","Species","Rel_FL")

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
Tagging$Rel_FL=with(Tagging,ifelse(Rel_FL>600,Rel_FL/10,Rel_FL))
Tagging$Rel_FL=ifelse(Tagging$Rel_FL>500,NA,Tagging$Rel_FL)
Tagging$CAP_FL=ifelse(Tagging$CAP_FL>500,NA,Tagging$CAP_FL)
Tagging$Rel_FL=ifelse(Tagging$Rel_FL==0,NA,Tagging$Rel_FL)
Tagging$Rel_FL=with(Tagging,ifelse(Rel_FL<10,NA,Rel_FL))
Tagging$CAP_FL=with(Tagging,ifelse(CAP_FL<10,NA,CAP_FL))
Tagging$CAP_FL=with(Tagging,ifelse(CAP_FL<Rel_FL,NA,CAP_FL))



#Add species full name
Tagging=left_join(Tagging,Species.Codes%>%distinct(Species,.keep_all = T),by="Species")


#Remove unknown species
Tagging=Tagging%>%
          filter(!Species=="")%>%
          filter(!COMMON_NAME%in%c("","Unknown"))


#Fix some dates
Tagging$Day.rec=with(Tagging,ifelse(Tag.no=="a154a",10,Day.rec))
Tagging$Mn.rec=with(Tagging,ifelse(Tag.no=="a154a",3,Mn.rec))
Tagging$Yr.rec=with(Tagging,ifelse(Tag.no=="a154a",2013,Yr.rec))

Tagging$Day.rec=with(Tagging,ifelse(Tag.no%in%c("884","3326","3327"),NA,Day.rec))
Tagging$Mn.rec=with(Tagging,ifelse(Tag.no%in%c("884","3326","3327"),NA,Mn.rec))
Tagging$Yr.rec=with(Tagging,ifelse(Tag.no%in%c("884","3326","3327"),NA,Yr.rec))


ID=which(Tagging$Yr.rel==1900)
if(length(ID)>0) Tagging=Tagging[-ID,]
Tagging$Day.rec=with(Tagging,ifelse(Tag.no%in%c("239"),9,Day.rec))
Tagging$Mn.rec=with(Tagging,ifelse(Tag.no%in%c("239"),12,Mn.rec))
Tagging$Yr.rec=with(Tagging,ifelse(Tag.no%in%c("239"),1997,Yr.rec))


# #Checks
# #Future checks
# Future.rel=subset(Tagging,Yr.rel>2014)
# Future.rec=subset(Tagging,Yr.rec>2014)
# Check1=Future.rel[,match(c("SHEET_NO","Tag.no"),names(Future.rel))]
# 
# #Past checks
# Past.rel=subset(Tagging,Yr.rel<1980)
# Past.rec=subset(Tagging,Yr.rec<1980)
# Check2=Past.rel[!duplicated(Past.rel$SHEET_NO),match(c("SHEET_NO","Tag.no"),names(Past.rel))]
# 
# #Rec before release
# Rec.before.Rel=subset(Tagging,Day.rec<=Day.rel & Mn.rec<=Mn.rel &  Yr.rec<=Yr.rel)
# Check3=unique(Rec.before.Rel$Tag.no)
# Check3=Rec.before.Rel[!duplicated(Rec.before.Rel$SHEET_NO),match(c("SHEET_NO","Tag.no"),names(Rec.before.Rel))]

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


#Fix dodgy lats and longs
Tagging$Lat.rels=with(Tagging,ifelse(Species=="BW" & Long.rels<114 & Lat.rels>-17.7,-21.765,Lat.rels))
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
  

