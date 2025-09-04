# SCRIPT FOR SOURCING BIOLOGICAL DATA FROM SHARK DATABASE

#notes:
#       Every new year: update 'Manually add lost species and subtract lost hooks'

library(RODBC)
library(lunar)   #moon phases
library(lubridate)
library(tidyverse)
library(ggrepel)
options(stringsAsFactors = FALSE)


# DATA SECTION -----------------------------------------------------------------------

#Sharks data base
setwd("M:/Production Databases/Shark") #updated 16/10/2023
#setwd("//fish.wa.gov.au/Data/Production Databases/Shark") 
#setwd("U:/Shark")
#Dat.Beis<-'Sharks v20240917.mdb' #new database updated by Vero's team
#Dat.Beis<-'Sharks v20240820 UAT.mdb'
Dat.Beis<-'Sharks v20220906.mdb'
#Dat.Beis<-'Sharks.mdb'

channel <- odbcConnectAccess2007(Dat.Beis)  
Boat_bio=sqlFetch(channel, "Boat_bio", colnames = F) 
Scalefish=sqlFetch(channel, "Scalefish", colnames = F) 
Boat_hdr=sqlFetch(channel, "Boat_hdr", colnames = F)   
close(channel)

if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

#Species names
SPECIES.names=read.csv(handl_OneDrive("Data/Species.code.csv"),stringsAsFactors=FALSE, fileEncoding="latin1")


#Species historically recorded in Boat header comments
Boat_bio_header_sp=read.csv(handl_OneDrive("Data/Shark_bio/Missing_species_from_comments.csv"),
                            stringsAsFactors=F)


#TL-FL coefficients
TL_FL=read.csv(handl_OneDrive("Data/Naturaliste/FL_to_TL.csv"),
               stringsAsFactors=F)




# PROCEDURE SECTION -----------------------------------------------------------------------
              
#Fix soak time 
names(Boat_hdr)[match(c("SOAK TIME",'AVE SET TIME','AVE HAUL TIME'),names(Boat_hdr))]=
  c("SOAK.TIME",'AVE.SET.TIME','AVE.HAUL.TIME')  
Boat_hdr$SOAK.TIME=with(Boat_hdr,ifelse(SOAK.TIME<0,NA,SOAK.TIME))
Boat_hdr$SOAK.TIME=with(Boat_hdr,ifelse(is.na(SOAK.TIME),(AVE.HAUL.TIME-AVE.SET.TIME)/3600,SOAK.TIME))
Boat_hdr$SOAK.TIME=with(Boat_hdr,ifelse(is.na(SOAK.TIME),(END_HAUL-START_SET)/3600,SOAK.TIME))
Boat_hdr$SOAK.TIME=with(Boat_hdr,ifelse(is.na(SOAK.TIME),(START_HAUL-START_SET)/3600,SOAK.TIME))
Boat_hdr$SOAK.TIME=with(Boat_hdr,ifelse(is.na(SOAK.TIME),(START_HAUL-END_SET)/3600,SOAK.TIME))
Boat_hdr$SOAK.TIME=with(Boat_hdr,ifelse(SOAK.TIME<0,24+SOAK.TIME,SOAK.TIME))

#Extract month,year, day, hour
Boat_hdr=Boat_hdr%>%
  mutate(date=as.Date(format(DATE, "%Y-%m-%d")),
         date=case_when(as.character(date)%in%c('2032-07-04','2032-07-03') & SHEET_NO=='H00017'~as.Date('2023-07-04'),
                        as.character(date)%in%c('2021-07-08') & SHEET_NO=='I00803'~as.Date('2022-07-08'),
                        TRUE~date),
         Day=mday(date),
         Month=month(date),
         year=year(date),
         Set.time=strftime(START_SET, format='%H:%M'),
         Haul.time=strftime(START_HAUL, format='%H:%M'),
         Set.time.end=strftime(END_SET, format='%H:%M'),
         Haul.time.end=strftime(END_HAUL, format='%H:%M'),
         Set.time.avg=strftime(AVE.SET.TIME, format='%H:%M'),
         Haul.time.avg=strftime(AVE.HAUL.TIME, format='%H:%M'))%>%
  dplyr::select(-c(DATE))

#Patch UNIQUE_ID due to Access macro stuffing in  
Boat_bio=Boat_bio%>%
  mutate(UNIQUE_ID.dummy=LINE_NO,
         UNIQUE_ID.dummy=ifelse(is.na(UNIQUE_ID.dummy),'',UNIQUE_ID.dummy),
         UNIQUE_ID.dummy=ifelse(nchar(UNIQUE_ID.dummy)==2,paste('0',UNIQUE_ID.dummy,sep=''),
                         ifelse(nchar(UNIQUE_ID.dummy)==1,paste('00',UNIQUE_ID.dummy,sep=''),
                                UNIQUE_ID.dummy)),
         UNIQUE_ID.dummy=paste(SHEET_NO,UNIQUE_ID.dummy,sep=''),
         UNIQUE_ID=case_when(!UNIQUE_ID==UNIQUE_ID.dummy~UNIQUE_ID.dummy,
                             TRUE~UNIQUE_ID))%>%
  dplyr::select(-UNIQUE_ID.dummy)

#Remove columns occurring both in Boat_hdr and Boat_bio
a=names(Boat_hdr)
b=names(Boat_bio)
dumi.dupli=b[which(b%in%a)]
dumi.dupli=subset(dumi.dupli,!dumi.dupli%in%c("SHEET_NO","COMMENTS"))
if(length(dumi.dupli)>0) Boat_hdr=Boat_hdr[,-match(dumi.dupli,names(Boat_hdr))]  

#Merge biological and sampling information  
Use.less.boat.hdr=c("Count","season","ZONE","BOTTYPE","CTCHABLITY",
                    "SEA_CONDTN","MOON","MOON PHASE","SUBBLOCK",
                    "SEA/WEATHER CONDITIONS","Region","Buffer Zone")
Use.full.boat.hdr=Boat_hdr[,-match(Use.less.boat.hdr,names(Boat_hdr))]
names(Use.full.boat.hdr)[match("COMMENTS",names(Use.full.boat.hdr))]="COMMENTS.hdr"
DATA=merge(Boat_bio,Use.full.boat.hdr,by="SHEET_NO",all.x=T)


#Set any factor to character
ID.f=vector('list',ncol(DATA))
for(f in 1:ncol(DATA)) ID.f[[f]]=class(DATA[,f])
ID.f=which(ID.f=="factor")
for(f in ID.f) DATA[,f]=as.character(DATA[,f])

ID.f=vector('list',ncol(Scalefish))
for(f in 1:ncol(Scalefish)) ID.f[[f]]=class(Scalefish[,f])
ID.f=which(ID.f=="factor")
for(f in ID.f) Scalefish[,f]=as.character(Scalefish[,f])

for(i in 1:ncol(SPECIES.names))if(is.factor(SPECIES.names[,i])) SPECIES.names[,i]=as.character(SPECIES.names[,i])


#Expand records from species that were reported in 'NO DISCARDS' (number discarded)
colnames(DATA)[match("NO DISCARDS",colnames(DATA))]="NO_DISCARDS"
dummy=subset(DATA,NO_DISCARDS>1)
DATA=subset(DATA,NO_DISCARDS<=1|is.na(NO_DISCARDS))
EXPNDA=vector('list',nrow(dummy))
for(e in 1:nrow(dummy))
{
  dd=dummy[e,]  
  EXPn=dd[rep(seq_len(nrow(dd)), dd$NO_DISCARDS), ]  
  EXPn$NO_DISCARDS=1
  EXPNDA[[e]]=EXPn
}
EXPNDA=do.call(rbind,EXPNDA)
DATA=rbind(DATA,EXPNDA)

These.are.scalies=c("BB","DM","PS","RS","MK","NW","QS")
move.to.scale=subset(DATA,SPECIES%in%These.are.scalies)
names(move.to.scale)[match(c("LINE_NO"),names(move.to.scale))]=c("Line no")
DATA=subset(DATA,!SPECIES%in%These.are.scalies)

move.to.scale$"NO UNMEASURED"=NA
move.to.scale=move.to.scale[,match(c("SHEET_NO","Line no","SPECIES","TL","SEX","NO UNMEASURED"),names(move.to.scale))]
Scalefish=rbind(Scalefish,move.to.scale)

#add hook info to Scalefish table. 
# note: Blank SPECIES in Boat.bio is the line number of a scalefish
these.are.scalies=Boat_bio%>%
                    filter(is.na(SPECIES))%>%
                    dplyr::select(UNIQUE_ID,NewComments,LostFlag,DeadFlag,HookedTime,ReleasedTime,BaitSpeciesId,
                                  HookLocation,HookType,HookSize,WireTrace,RetainedFlag)%>%
                    distinct(UNIQUE_ID,.keep_all=T)
Scalefish=Scalefish%>%  
          mutate(dummy=`Line no`,
                 dummy=ifelse(nchar(dummy)==2,paste('0',dummy,sep=''),
                       ifelse(nchar(dummy)==1,paste('00',dummy,sep=''),
                              dummy)),
                 UNIQUE_ID=paste(SHEET_NO,dummy,sep=''))%>%
      dplyr::select(-dummy)%>%
        left_join(these.are.scalies,by="UNIQUE_ID")


#Fix upper lower case issues
DATA$SPECIES=toupper(DATA$SPECIES)
Scalefish$SPECIES=toupper(Scalefish$SPECIES)


#remove typos
typos.shk=toupper(c( "BZ"  , "CS" ,"BM","GK"))
DATA=subset(DATA,!SPECIES%in%typos.shk)

#scale up unmeasured scalefish
names(Scalefish)[match("NO UNMEASURED",names(Scalefish))]="Numbers"

Scalefish=Scalefish%>%   # Jack entered FL into the 'no unmeasured' column so remove
            mutate(Numbers=ifelse(Numbers>1 & grepl("PA",SHEET_NO),NA,Numbers))  



Scalefish$SPECIES=with(Scalefish,
                       ifelse(SPECIES=="HM","MK",
                        ifelse(SPECIES=="SD","BL",
                        ifelse(SPECIES=="DW","DM",
                        ifelse(SPECIES=="SW","SL",
                               SPECIES)))))

typos=toupper(c("bp","bt","bx","jf","pj","qQ","wd","ww",
        "FE","JK","KW","LK","NG","PF","SD","SF",
        "SR","WA","WH","XC","XR"))


Scalefish=subset(Scalefish,!SPECIES%in%typos)

Scalefish$Numbers=with(Scalefish,ifelse(is.na(Numbers),1,ifelse(Numbers==0,1,Numbers)))
Scalefish$SHEET_NO=with(Scalefish,ifelse(SHEET_NO=="s00265","S00265",SHEET_NO))

Scalefish$TL=with(Scalefish,ifelse(TL==0,NA,TL))
Expand.scale=subset(Scalefish,Numbers>1)
Scalefish=subset(Scalefish,Numbers==1)

EXPNDA=vector('list',nrow(Expand.scale))
for(e in 1:nrow(Expand.scale))
{
  dd=Expand.scale[e,]  
  EXPn=dd[rep(seq_len(nrow(dd)), dd$Numbers), ]  
  EXPNDA[[e]]=EXPn
}
EXPNDA=do.call(rbind,EXPNDA)
EXPNDA$Numbers=1
Scalefish=rbind(Scalefish,EXPNDA)
Scalefish=merge(Scalefish,Use.full.boat.hdr,by="SHEET_NO",all.x=T)   
names(Scalefish)[match("Line no",names(Scalefish))]="LINE_NO"
Scalefish$FL=NA

Scalefish=subset(Scalefish,!is.na(SPECIES))

Scalefish$SPECIES=paste(Scalefish$SPECIES,".T",sep="")

DATA$SPECIES=with(DATA,ifelse(SPECIES=="se","SE",ifelse(SPECIES=="tk","TK",
                       ifelse(SPECIES=="wr","WR",ifelse(SPECIES=="ww","WW",SPECIES)))))
DATA$SPECIES=with(DATA,ifelse(SPECIES=="wd","WD",SPECIES))
DATA$SPECIES=with(DATA,ifelse(SPECIES%in%c("dw","DW"),"BW",SPECIES))
DATA$SPECIES=with(DATA,ifelse(SPECIES=="bw","BW",SPECIES)) 
DATA$SPECIES=with(DATA,ifelse(SPECIES=="bt","BT",SPECIES)) 
DATA$SPECIES=with(DATA,ifelse(SPECIES=="sr","SR",SPECIES)) 

DATA$SPECIES=with(DATA,ifelse(SPECIES=="LJ","LJ.T",SPECIES)) 
DATA$SPECIES=with(DATA,ifelse(SPECIES=="SA","SA.T",SPECIES)) 
DATA$SPECIES=with(DATA,ifelse(SPECIES=="TW","TW.T",SPECIES)) 


Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="ba.T","BA.T",SPECIES)) 
Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="bb.T","BB.T",SPECIES)) 
Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="Bg.T","BG.T",SPECIES)) 
Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="br.T","BR.T",SPECIES)) 
Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="kf.T","KF.T",SPECIES)) 
Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="sr.T","SR.T",SPECIES)) 
Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="st.T","ST.T",SPECIES)) 
Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="yt.T","YT.T",SPECIES)) 

Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="sl.T","SL.T",SPECIES)) 
Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="sc.T","SC.T",SPECIES)) 
Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="dm.T","DM.T",SPECIES)) 

Scalefish=Scalefish%>%
            mutate(TL=case_when(TL<17~NA,
                                SPECIES=="BD.T" & SHEET_NO=="J00925" & TL>200 ~TL/10,
                                SPECIES=="LF.T" & SHEET_NO=="M00207" & TL>50 ~TL/10,
                                SPECIES=="TA.T" & SHEET_NO=="BU0041" & TL>120 ~TL/10,
                                SPECIES=="JE.T" & TL>150 ~TL/10,
                                SPECIES=="NW.T" & TL>100 ~TL/10,
                                SPECIES=="QS.T" & TL>110 ~TL/10,
                                SPECIES=="TV.T" & TL>110 ~TL/10,
                                TRUE~TL))

#Fix sex
DATA$SEX=with(DATA,ifelse(SEX=="f","F",ifelse(SEX=="m","M",
      ifelse(SEX%in%c(",","?","9","B","n","N","p","P","u","Y"),NA,SEX))))


Cnhg=match(c("NO HOOKS","DART TAG NO","FINTAG NO","FINTAG 2","ATAG NO","BAG NO"),names(DATA))
names(DATA)[Cnhg]=c("N.hooks","DART_TAG_NO","FINTAG_NO","FINTAG_2","ATAG_NO","BAG_NO")

#Add individuals (mostly bycatch species but some commercial) not reported as row entries but only mentioned in boat header comments
Boat_bio_header_sp=subset(Boat_bio_header_sp,select=c(SHEET_NO,SPECIES2))
colnames(Boat_bio_header_sp)[2]="SPECIES"
Dummyss=subset(DATA,SHEET_NO%in%unique(Boat_bio_header_sp$SHEET_NO))
Dummyss=Dummyss[!duplicated(Dummyss$SHEET_NO),]
set.to.na=names(Dummyss)
set.to.na=set.to.na[-match(c("SHEET_NO","LINE_NO","UNIQUE_ID",
      "AVE.SET.TIME","AVE.HAUL.TIME","SOAK.TIME","date","Month","year",
      "BOAT","BLOCK","SKIPPER",           
      "START_SET","END_SET","START_HAUL","END_HAUL","BOTDEPTH",
      "MESH_SIZE", "MESH_DROP", "NET_LENGTH", "TEMP", "END1LATD",
      "END1LATM","END1LNGD","END1LNGM","END2LATD","END2LATM","END2LNGD",
      "END2LNGM","N.hooks","gillnet effort","MID LAT","MID LONG",
      "Longline effort","Method"),set.to.na)]
Dummyss[,match(set.to.na,names(Dummyss))]=NA
Dummyss=Dummyss[,-match("SPECIES",names(Dummyss))]

Boat_bio_header_sp=merge(Boat_bio_header_sp,Dummyss,by="SHEET_NO",all.x=T)
Boat_bio_header_sp=Boat_bio_header_sp[,match(names(DATA),names(Boat_bio_header_sp))]

DATA=rbind(DATA,Boat_bio_header_sp)

#fix temperature
DATA$TEMP=with(DATA,ifelse(TEMP==2537,25.7,ifelse(TEMP==0,NA,TEMP)))


#Convert TL to FL if FL is NA and there is TL info
DATA=DATA%>%
  mutate(FL=case_when(FL==0~NA,
                      SHEET_NO=='U00014' & SPECIES=='TG' & FL<3~FL*10,
                      SHEET_NO=='R00200'& SPECIES=='HZ' & FL<20~FL*10,
                      SHEET_NO=='D00102'& SPECIES=='WH' & FL<20~FL*10,
                      TRUE~FL),
         TL=case_when(TL<10~NA,
                      TL==0~NA,
                      SHEET_NO=='B00410' & SPECIES=='SC' & TL<12 ~ TL*10,
                      SHEET_NO=='R00237' & SPECIES=='WW' & TL<14 ~ TL*10,
                      SHEET_NO=='U00014' & SPECIES=='TG' & TL<4~TL*10,
                      SHEET_NO=='U00113' & SPECIES=='BW' & TL<20~TL*10,
                      SHEET_NO=='PA0078' & SPECIES=='WH' & TL<20~TL*10,
                      SHEET_NO=='U00013' & SPECIES=='TG'& TL<15~TL*10,
                      SHEET_NO=='U00024' & SPECIES=='WP' & TL<15~TL*10,
                      TRUE~TL))
DATA=merge(DATA,TL_FL[,match(c("SPECIES","a.intercept","b.slope"),names(TL_FL))],by="SPECIES",all.x=T)
DATA$TL=with(DATA,ifelse(!is.na(FL) & !is.na(TL) & FL>TL,NA,TL))
DATA$FL=with(DATA,ifelse(is.na(FL)&!is.na(TL),(TL-a.intercept)/b.slope,FL))
DATA=DATA[,-match(c("a.intercept","b.slope"),names(DATA))]
DATA=DATA%>%
        mutate(FL=case_when(FL<=0~NA,
                            FL>10 & TL>40 & SPECIES=='WC'~NA,
                            FL<20 & SPECIES=="ER"~NA,
                            TRUE~FL))

#Amend species code to family for species outside reported distribution  
DATA$dummys=DATA$`MID LAT`
DATA=DATA%>%
  mutate(SPECIES=case_when(SPECIES=='BC' & dummys>25 ~'CA',
                           SPECIES=='CW' & dummys>24.5 ~'XX',
                           SPECIES=='SS'  ~'AV',    
                           TRUE~SPECIES))%>%
  dplyr::select(-dummys)

DATA=DATA%>%
  mutate(SPECIES=ifelse(SPECIES=='GH' & SHEET_NO=='I00842','HG',
                 ifelse(NewComments=='POSSIBLY MILK SHARK HEAD ONLY LAB' & SPECIES=='XX','MI',
                        SPECIES)))
                        
#Extract depredation events
Depredated=DATA%>%filter(grepl(paste(c('head','depredat'),collapse = '|'),tolower(NewComments)))%>%
            dplyr::select(SHEET_NO,LINE_NO,date,year,BOAT,Method,SPECIES,NewComments)

#Manually add lost species and subtract lost hooks  

#note:  'lost hooks' or 'lost species' was entered only in the comments. 
#       Some records already deducted by data person entry so only change here records not amended by data entry person.
#       Don't do for Parks Australia shots, this is done in PA script

upto=as.Date("2025-06-17")  #manually fix records after this date in case_when() and 'Add.sp'

lost=DATA[grep(paste(c('lost','straightened'),collapse='|'),tolower(DATA$COMMENTS.hdr)),]%>%
          filter(Method=='LL')%>%
          distinct(SHEET_NO,date,COMMENTS.hdr)%>%
          filter(date>upto)
  #hooks
lost.hooks=lost[grep('hook',tolower(lost$COMMENTS.hdr)),]%>%
            filter(!grepl('PA',SHEET_NO))%>%arrange(date)
DATA=DATA%>%
  mutate(N.hooks.deployed=N.hooks,
         N.hooks=case_when(SHEET_NO=="N00348" & N.hooks==50 ~ (N.hooks-6),
                           SHEET_NO=="S00254" & N.hooks==450 ~ (N.hooks-30),
                           SHEET_NO=="W00115" & N.hooks==50 ~ (N.hooks-30),
                           SHEET_NO=="N00101" & N.hooks==50 ~ (N.hooks-4),
                           SHEET_NO=="N00857" & N.hooks==60 ~ (N.hooks-30),
                           SHEET_NO=="I00798" & N.hooks==50 ~ (N.hooks-15),
                           SHEET_NO=="N00630" & N.hooks==50 ~ (N.hooks-35),
                           SHEET_NO=="N00582" & N.hooks==50 ~ (N.hooks-31),
                           SHEET_NO=="Z00030" & N.hooks==62 ~ (N.hooks-31),
                           SHEET_NO=="W00040" & N.hooks==62 ~ (N.hooks-20),
                           SHEET_NO=="N00353" & N.hooks==50 ~ (N.hooks-2),
                           SHEET_NO=="D00156" & N.hooks==50 ~ (N.hooks-32),
                           SHEET_NO=="D00144" & N.hooks==50 ~ (N.hooks-4),
                           TRUE~N.hooks),
         N.hooks.lost=N.hooks.deployed-N.hooks)
  #species
Add.sp=data.frame(SHEET_NO=c(rep("N00101",2),"N00104",rep("N00110",2),"N00115",rep("N00130",2),"N00135","N00151",
                             "N00161","N00162","N00164","N00179","N00184","N00189","N00163"),
                  SPECIES=c(rep('TK',2),rep('TG',2),'TK',rep('TK',11),'MI'),
                  TL=c(rep(NA,2),rep(300,2),NA,rep(NA,11),NA))

DATA.add.sp.com=DATA[1:nrow(Add.sp),]%>%
                        mutate(across(everything(), ~NA),
                               SHEET_NO=Add.sp$SHEET_NO,
                               SPECIES=Add.sp$SPECIES,
                               TL=Add.sp$TL)
DATA=rbind(DATA,DATA.add.sp.com)

#ACA
#Annual data validation
do.annual.dat.validation=FALSE
if(do.annual.dat.validation)
{
  a=DATA%>%filter(date>as.Date("2025-06-17") & BOAT=='NAT')
  a=merge(a,SPECIES.names,by.x="SPECIES",by.y="Species",all.x=T)
  #check locations
  a%>%
    rename(lat='MID LAT',
           long='MID LONG')%>%
    distinct(lat,long,SHEET_NO)%>%
    mutate(lat=-abs(lat))%>%
    ggplot(aes(long,lat,label=SHEET_NO))+
    geom_text_repel()
  
  #check species
  Check.species=a%>%
    distinct(SPECIES,COMMON_NAME,SCIENTIFIC_NAME)
  Check.species=Check.species%>%
    filter(is.na(SPECIES)|is.na(COMMON_NAME))
  Check.species=a%>%
    filter(SPECIES%in%Check.species$SPECIES | COMMON_NAME%in%Check.species$COMMON_NAME)%>%
    dplyr::select(SHEET_NO,LINE_NO,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,COMMENTS.hdr,NewComments,COMMENTS)
  if(nrow(Check.species)>0) write.csv(Check.species,
                                      handl_OneDrive('Analyses/Surveys/Naturaliste_longline/outputs/Data validation/Check.species.csv'),row.names = F)
  
  
  #check lengths, sex
  a%>%
    ggplot(aes(TL))+
    geom_bar()+
    facet_wrap(~COMMON_NAME,scales='free')
  
  a%>%
    ggplot(aes(FL))+
    geom_bar()+
    facet_wrap(~COMMON_NAME,scales='free')
  
  table(a$SEX,useNA = 'ifany')
  
  #check diet and reprod data entered in columns not just comments
  x=a%>%
    filter(grepl(paste(c('uterine','egg','-f','-m','embryo','pup','ovary'),collapse='|'),tolower(NewComments)))%>%
    dplyr::select(SHEET_NO,SPECIES,LINE_NO,
                  CLASPLENTH,CLASP_CALC,GON_STAGE,RUN_SPERM,MAXOVRYDIA,OG,NO_YOLKOVA,UTERINESTG,NO_EMBRYOS,
                  NO_UNDEVELOPED,EMBLEN_1,EMBLEN_2,EMBLEN_3,EMBLEN_4,EMBLEN_5,EMBLEN_6,EMBLEN_7,EMBLEN_8,EMBLEN_9,
                  EMBLEN_10,EMBLEN_11,EMBLEN_12,EMBLEN_1_SEX,EMBLEN_2_SEX,EMBLEN_3_SEX,EMBLEN_4_SEX,EMBLEN_5_SEX,
                  EMBLEN_6_SEX,EMBLEN_7_SEX,EMBLEN_8_SEX,EMBLEN_9_SEX,EMBLEN_10_SEX,EMBLEN_11_SEX,EMBLEN_12_SEX,
                  NewComments,date)
  
  #check heads been entered as record
  a%>%filter(grepl('head',tolower(NewComments)))%>%distinct(NewComments,SPECIES,COMMON_NAME,SCIENTIFIC_NAME)
}

#Create Biological data frame
keep.biol=c("SHEET_NO","SPECIES","date","TL","FL","PL","SEX","RELEASE CONDITION","UMBIL_SCAR", "NO_DISCARDS",
            "CLASPLENTH","CLASP_CALC","GON_STAGE","RUN_SPERM","MAXOVRYDIA","NO_YOLKOVA","UTERINESTG","NO_EMBRYOS",
            "NO_UNDEVELOPED","EMBLEN_1","EMBLEN_2","EMBLEN_3","EMBLEN_4","EMBLEN_5","EMBLEN_6","EMBLEN_7","EMBLEN_8",
            "EMBLEN_9","EMBLEN_10","EMBLEN_11","EMBLEN_12","STMCH_FULL","STMCH_CONT",
            "ATAG_NO","DART_TAG_NO","FINTAG_NO","FINTAG_2","BAG_NO","COMMENTS","COMMENTS.hdr","UNIQUE_ID")
DATA.bio=DATA[,match(keep.biol,names(DATA))]

#Merge scalefish and shark data
DATA$Numbers=1

#Amend nonsense species
Scalefish$dummys=Scalefish$`MID LAT`
Scalefish=Scalefish%>%
  mutate(SPECIES=case_when(SPECIES=='RB.T' & dummys>25 ~'XX.T',
                           TRUE~SPECIES))%>%
  dplyr::select(-dummys)
  
Scalefish$VERT_SAMPL=NA
Scalefish$SEX=as.character(Scalefish$SEX)
Scalefish$SEX=with(Scalefish,ifelse(SEX=="f","F",ifelse(SEX=="m","M",SEX)))
Scalefish$SPECIES=as.character(Scalefish$SPECIES)
names(Scalefish)[match(c("NO HOOKS"),names(Scalefish))]=c("N.hooks")
Scalefish$BAG_NO=""
Scalefish$"RELEASE CONDITION"=""
add.these.cols=c('PL','COMMENTS',"Weight","BloodFlag","FinClipFlag","MuscleFlag","Lactate","BleedingFlag",
                 "HookRemoved","TrunkL")
for(i in 1:length(add.these.cols))
{
  if(!add.these.cols[i]%in%names(Scalefish)) Scalefish=Scalefish%>%add_column(!!(add.these.cols[i]) :=NA)
}

DATA=DATA[,match(names(Scalefish),names(DATA))]
 
DATA=DATA%>%mutate(SPECIES=case_when(SPECIES=='SH' & SHEET_NO=="PA0039"~'HS',  #fixing Abbey's stuff up
                                     SPECIES=='SR' & SHEET_NO=="PA0107"~'SG',
                                     SPECIES=='LJ' & SHEET_NO=="PA0090"~'PJ',
                                     TRUE~SPECIES))
Scalefish=Scalefish%>%
            mutate(SPECIES=case_when(SPECIES=='LJ.T' & SHEET_NO=="PA0090"~'PJ',
                                     SHEET_NO=='PA0019' & SPECIES=='XX.T' & NewComments=="Rockfish"~'WB.T',
                                     SHEET_NO=='PA0039' & SPECIES=='XX.T' & NewComments=="Remora"~'SF.T',
                                     SHEET_NO=='PA0045' & SPECIES=='XX.T' & grepl('Crayfish',NewComments)~'WL',
                                     SHEET_NO=='PA0043' & SPECIES=='XX.T' & grepl('Porcupine',NewComments)~'GL.T',
                                     SHEET_NO=='PA0052' & SPECIES=='XX.T' & grepl('Black rockfish',NewComments)~'WB.T',
                                     SHEET_NO%in%c('PA0139','PA0056','PA0147') & SPECIES=='XX.T' & grepl('wrasse',tolower(NewComments))~'BW.T',
                                     SHEET_NO=='PA0055' & SPECIES=='XX.T' & grepl('BOX FISH',NewComments)~'WH.T',
                                     SHEET_NO=='PA0069' & SPECIES=='XX.T' & grepl('WOLF EEL',NewComments)~'GM.T',
                                     SHEET_NO=='PA0084' & SPECIES=='XX.T' & grepl('Globe fish',NewComments)~'GL.T',
                                     SHEET_NO=='PA0085' & SPECIES=='XX.T' & grepl('BAILER SNAIL',NewComments)~NA,
                                     SHEET_NO=='PA0106' & SPECIES=='XX.T' & grepl('Squirrel',NewComments)~'SQ.T',
                                     SHEET_NO=='PA0141' & SPECIES=='XX.T' & grepl('Copper fish',NewComments)~'NW.T',
                                     SHEET_NO=='PA0147' & SPECIES=='XX.T' & grepl('Wirrah',NewComments)~'WW.T',
                                     SHEET_NO=='PA0147' & SPECIES=='XX.T' & grepl('Eel',NewComments)~'GM.T',
                                     SHEET_NO=='PA0155' & SPECIES=='XX.T' & grepl('Dentex',NewComments)~'DC.T',
                                     TRUE~SPECIES))

#Combine shark and scalefish datasets
DATA$TYPE="Elasmo"
Scalefish=Scalefish%>%
            mutate(TYPE=ifelse(SPECIES=='PJ',"Elasmo","Scalefish"))

DATA$SPECIES=with(DATA,ifelse(SPECIES=="JE","JE.T",SPECIES))
DATA$TYPE=with(DATA,ifelse(SPECIES=="JE.T","Scalefish",TYPE))

DATA=rbind(DATA,Scalefish)  

DATA=DATA%>%
  mutate(TYPE=ifelse(grepl(".T",SPECIES),'Scalefish',TYPE),
         TYPE=ifelse(SPECIES%in%c("BT","HT","PT","ST","WT"),'Elasmo',TYPE))



#Fix lats
names(DATA)[match(c("MID LAT","MID LONG"),names(DATA))]=c('Mid.Lat','Mid.Long')
DATA$Mid.Lat=-abs(DATA$Mid.Lat)
DATA$END1LATD=-abs(DATA$END1LATD)
DATA$END2LATD=-abs(DATA$END2LATD)


#Add moon phase
#note: 0 and 100 are full moon, 50 is new moon, 25 last quarter and 75 first quater
DATA$Moon=lunar.phase(DATA$date,name = T)

#Remove NA species (these are NA rows add by Shark database in the boat.bio table that corresponds to scalefish)
DATA=subset(DATA,!is.na(SPECIES))

#Add Number caught
DATA$Number=1


#Fixes

#fix inconsistent species codes
DATA$SPECIES=with(DATA,ifelse(SPECIES=="LP","ZE",SPECIES))   #LP is zebra shark
DATA.bio$SPECIES=with(DATA.bio,ifelse(SPECIES=="LP","ZE",SPECIES))

#fix dodgy latitude
DATA$Mid.Lat=with(DATA,ifelse(SHEET_NO=="N00401",-20.90,Mid.Lat))


DATA$Mid.Lat=with(DATA,ifelse(SHEET_NO=="N00401",-20.90,Mid.Lat))                            
DATA$END1LNGD=with(DATA,ifelse(SHEET_NO=="R00890",113.96,END1LNGD))
DATA$Mid.Long=with(DATA,ifelse(SHEET_NO=="R00890",113.9632,Mid.Long))
DATA$Mid.Long=with(DATA,ifelse(SHEET_NO=="N00099",113.4272,Mid.Long))
DATA$Mid.Long=with(DATA,ifelse(SHEET_NO=="N00597",113.2404,Mid.Long))
DATA$Mid.Long=with(DATA,ifelse(SHEET_NO=="N00558",113.2055,Mid.Long))

DATA$Mid.Lat=with(DATA,ifelse(is.na(Mid.Lat) &!is.na(BLOCK),-as.numeric(substr(BLOCK,1,2)),Mid.Lat))
DATA$Mid.Long=with(DATA,ifelse(is.na(Mid.Long) &!is.na(BLOCK),100+as.numeric(substr(BLOCK,3,4)),Mid.Long))

DATA$Mid.Lat=with(DATA,ifelse(is.na(Mid.Lat) & Mid.Lat>(-1) &!is.na(BLOCK),-as.numeric(substr(BLOCK,1,2)),Mid.Lat))
DATA$Mid.Lat=with(DATA,ifelse(is.na(Mid.Lat) & END1LATD==0 &!is.na(BLOCK),-as.numeric(substr(BLOCK,1,2)),Mid.Lat))
DATA$Mid.Lat=with(DATA,ifelse(is.na(Mid.Lat) & !substr(Mid.Lat,1,3)==(-as.numeric(substr(BLOCK,1,2))),-as.numeric(substr(BLOCK,1,2)),Mid.Lat))

DATA$Mid.Lat=with(DATA,ifelse(Mid.Lat<(-40) & SHEET_NO=="S00232" &!is.na(BLOCK),-as.numeric(substr(BLOCK,1,2)),Mid.Lat))

DATA$Mid.Long=with(DATA,ifelse(Mid.Long==0 &!is.na(BLOCK),100+as.numeric(substr(BLOCK,3,4)),Mid.Long))
DATA$Mid.Long=with(DATA,ifelse(Mid.Long<80 & SHEET_NO=="F00110" &!is.na(BLOCK),100+as.numeric(substr(BLOCK,3,4)),Mid.Long))



# DATA$Mid.Lat=with(DATA,ifelse(Lat.round==(-45.5) &END1LATD==(-30),END1LATD,Mid.Lat))
# DATA$Mid.Lat=with(DATA,ifelse(Mid.Long<=58,END1LATD,Mid.Lat))
# DATA$Mid.Lat=with(DATA,ifelse(!round(Mid.Lat)==round(END1LATD) & 
#                                 BLOCK%in%c(2914,3115,3215,3315,3414,3415),END1LATD,Mid.Lat))    
# DATA$Mid.Lat=with(DATA,ifelse(END1LATD==(-24) &BLOCK==3415,END2LATD,Mid.Lat))
# DATA$Mid.Lat=with(DATA,ifelse(round(Mid.Lat)> (-26) & 
#                                 BLOCK%in%c(3215,3423,3517),END1LATD,Mid.Lat)) 

# DATA$Mid.Long=with(DATA,ifelse(Mid.Long<=62,END1LNGD,Mid.Long))
# DATA$Mid.Long=with(DATA,ifelse(BLOCK==3415 & Mid.Long>116,END1LNGD,Mid.Long))
# DATA$Mid.Long=with(DATA,ifelse(BLOCK==3014 & Mid.Long>116,END1LNGD,Mid.Long))

DATA$Mid.Lat=ifelse(DATA$Mid.Lat==0,NA,DATA$Mid.Lat)

DATA=DATA%>%
        mutate(Mid.Lat=case_when(SHEET_NO=='M00239' ~18.44,
                                 TRUE~Mid.Lat))

DATA$Lat.round = ceiling(DATA$Mid.Lat) -0.5
DATA$Long.round = floor(DATA$Mid.Long) +0.5

#Check lat and longs not on land
Do.dis=FALSE
if(Do.dis)
{
  WAcoast<-read.table(handl_OneDrive("Data/Mapping/WAcoastPointsNew.txt"), header=T)
  WAcoast=WAcoast%>%mutate(Latitude=abs(Latitude))
  dd=DATA%>%
    distinct(SHEET_NO,.keep_all = T)
  dd$Mid.Lat=abs(dd$Mid.Lat)
  plot(seq(112,130,length.out=10),seq(36,12,length.out=10), type="n", xlab="", ylab="", xlim=c(112,130),ylim=c(36,12))
  polygon(WAcoast$Longitude,WAcoast$Latitude)
  points(dd$Mid.Long,dd$Mid.Lat,col='forestgreen')
  text(x=121.5,y=22.5,labels='M00239',pos=4)
  
  a=dd%>%filter(Mid.Long>114.14  & Mid.Long<114.75 &  Mid.Lat<(25.11)& Mid.Lat>(26.53))
  
}

# Add fishing zones
DATA$zone=as.character(with(DATA,ifelse(Mid.Long>=116.5 & Mid.Lat<=(-26),"Zone2",
              ifelse(Mid.Long<116.5 & Mid.Lat<=(-33),"Zone1",
             ifelse(Mid.Lat>(-33) & Mid.Lat<=(-26) & Mid.Long<116.5,"West",
            ifelse(Mid.Lat>(-26) & Mid.Long<114,"Closed",
             ifelse(Mid.Lat>(-26) & Mid.Long>=114 & Mid.Long<123.75,"North",
            ifelse(Mid.Lat>(-26) & Mid.Long>=123.75,"Joint",NA))))))))



#Add full species names
this.nms=subset(SPECIES.names,Species%in%unique(DATA$SPECIES))
DATA=merge(DATA,SPECIES.names,by.x="SPECIES",by.y="Species",all.x=T)
Table.species=sort(table(DATA$SPECIES))


#add method to records with NAs
DATA$MESH_SIZE=with(DATA,ifelse(MESH_SIZE%in%c("6.5/7","605","6.587","65"),"6.5",
                                ifelse(MESH_SIZE%in%c("7.0","73"),"7",
                                       ifelse(MESH_SIZE%in%c("8.8"),"8",MESH_SIZE))))

DATA$Method=with(DATA,ifelse(is.na(Method) & MESH_SIZE%in%c("6","6.5","7","8"),"GN",
                             ifelse(is.na(Method)& N.hooks>25,"LL",
                                    ifelse(is.na(Method)& BOAT=="E35","GN",Method))))

DATA$Method=with(DATA,ifelse(is.na(Method) & BOAT%in%c("B67","F244","F517"),"GN",Method))

DATA$MESH_SIZE=with(DATA,ifelse(is.na(MESH_SIZE) & BOAT=="E35","6.5",MESH_SIZE))
DATA$MESH_SIZE=with(DATA,ifelse(is.na(MESH_SIZE) & BOAT=="B67" & year>1997,"7",
                ifelse(is.na(MESH_SIZE) & BOAT=="F244" & year>2004,"6.5",            
                ifelse(is.na(MESH_SIZE) & BOAT=="F517" & year==2002,"6.5",
                ifelse(is.na(MESH_SIZE) & BOAT=="F517" & year>2002,"7",
                MESH_SIZE)))))

DATA$MESH_SIZE=with(DATA,ifelse(is.na(MESH_SIZE) & BOAT%in%c("C48","PS17","E67"),"7",
                                MESH_SIZE))

DATA=DATA%>%
  mutate(MESH_SIZE=ifelse(Method=='GN' & MESH_SIZE=='165',"6.5",
                   ifelse(Method=='GN' & MESH_SIZE=='4"',"4",
                   ifelse(Method=='GN' & MESH_SIZE=='5"',"5",
                   ifelse(Method=='GN' & MESH_SIZE=='6"',"6",
                   ifelse(Method=='GN' & MESH_SIZE=='7"',"7",
                   ifelse(Method=='GN' & MESH_SIZE=='8"',"8",
                   ifelse(Method=='GN' & MESH_SIZE=='10"',"10",
                   MESH_SIZE)))))))) 



#fix boat
DATA$BOAT=with(DATA,ifelse(BOAT=="TRACEY LEA","E35",BOAT))

#Fix sex
DATA$SEX=with(DATA,ifelse(SEX=="f","F",ifelse(SEX=="m","M",
      ifelse(SEX%in%c(",","?","n","N","p","P","Y"),"U",SEX))))

#Fix FL
DATA$FL=with(DATA,ifelse(FL<10,NA,FL))
DATA$FL=with(DATA,ifelse(SPECIES=="BW" & FL<41,NA,ifelse(SPECIES=="WH" & FL==12,120,
                       ifelse(SPECIES=="SD" & FL<20,20,FL)))) #Re set minimum FL observed 
DATA$FL=with(DATA,ifelse(SPECIES=="MI" & FL==690,69,
                  ifelse(SPECIES=="SC" & FL==9.90,99,
                  ifelse(SPECIES=="SC" & FL<11,105.3,
                  ifelse(SHEET_NO=="D00142" & SPECIES=="SO" & FL>600,FL/10,
                        FL)))))  #some typos
DATA=DATA%>%
  mutate(FL=case_when(SPECIES=='PJ' & FL>140 ~ FL/10,
                      TRUE~FL))

#Fix TL
DATA=DATA%>%
  mutate(TL=case_when(SPECIES=='BB.T' & TL<10 ~ NA,
                      SPECIES=='DM.T' & TL<20 ~ NA,
                      SPECIES=='BA.T' & TL>120 ~ NA,
                      SPECIES=='KJ.T' & TL>70 ~ NA,
                      SPECIES=='ML.T' & TL>60 ~ NA,
                      SPECIES=='NW.T' & (TL>110 | TL<10)~ NA,
                      SPECIES=='JE.T' & TL>200 ~ TL/10,
                      SPECIES=='PJ' & TL>160 ~ TL/10,
                      SPECIES=='QS.T' & TL>130 ~ TL/10,
                      SPECIES=='TV.T' & TL<5 ~ NA,
                      SPECIES=='TV.T' & TL>100 ~ TL/10,
                      TRUE~TL))

dothis=FALSE
if(dothis)
{
  AA=rev(sort(table(DATA$SPECIES)))
  DIS=names(AA)[101:120]
  
  DATA%>%
    filter(SPECIES%in%DIS)%>%
    mutate(Size=ifelse(TYPE=='Scalefish',TL,
                       ifelse(TYPE=='Elasmo',FL,
                              NA)),
           Size=ifelse(SPECIES%in%c('ER','SR','BC','PC','SM'),TL,Size))%>%
    ggplot()+
    geom_histogram(aes(Size))+
    facet_wrap(~SPECIES,scales='free')+
    scale_x_continuous(limits = c(0, NA))
  
  di='SM'
  b1=DATA%>%filter(SPECIES==di)%>%distinct(SHEET_NO,FL,TL,PL)%>%filter(!is.na(TL))%>%arrange(TL)
  tail(b1)
  SPECIES.names%>%filter(Species ==di)
}

#Fix depth numbers
DATA$BOTDEPTH=with(DATA,ifelse(SHEET_NO=="J00999",84,
                        ifelse(SHEET_NO=="J01188",73,
                        ifelse(SHEET_NO=="R00413",85,
                        ifelse(SHEET_NO=="R00440",120,
                        ifelse(SHEET_NO=="R00716",7,
                        ifelse(SHEET_NO=="R00930",48,
                        ifelse(SHEET_NO=="R00422",75,
                        ifelse(SHEET_NO=="R01059",56,BOTDEPTH)))))))))



#Add shot data to DATA.bio
add.this=c("SOAK.TIME","Method","date","Day","Month","year","BOAT","BLOCK",
           "Mid.Lat","Mid.Long", "Lat.round","Long.round","zone","SKIPPER","BOTDEPTH","MESH_SIZE","MESH_DROP",
           "NET_LENGTH","N.hooks","SHEET_NO","TEMP")   
D=DATA[,match(add.this,names(DATA))]
D=subset(D,SHEET_NO%in%unique(DATA.bio$SHEET_NO))
D=D[!duplicated(D$SHEET_NO),]
DATA.bio=merge(DATA.bio,D,by="SHEET_NO",all.x=T)
DATA.bio=merge(DATA.bio,SPECIES.names,by.x="SPECIES",by.y="Species",all.x=T)

DATA.bio$UMBIL_SCAR=with(DATA.bio,ifelse(UMBIL_SCAR=='y','Y',ifelse(UMBIL_SCAR=='n','N',
           ifelse(UMBIL_SCAR=='p','P',
                  ifelse(UMBIL_SCAR%in%c('?','`','2','3','7','B','f','F','m','M','V'),NA,UMBIL_SCAR)))))

Biol.vars=c("UMBIL_SCAR","CLASPLENTH","CLASP_CALC","GON_STAGE","RUN_SPERM","MAXOVRYDIA",
            "NO_YOLKOVA","UTERINESTG","NO_EMBRYOS","NO_UNDEVELOPED","EMBLEN_1",
            "STMCH_FULL","STMCH_CONT")


for(q in 1:length(Biol.vars))
{
  id=match(Biol.vars[q],names(DATA.bio))
  DATA.bio[,id]=ifelse(DATA.bio[,id]%in%c('?','`'),NA,DATA.bio[,id])
}
Cannot.be.0=c("MAXOVRYDIA","NO_YOLKOVA","NO_EMBRYOS","NO_UNDEVELOPED","EMBLEN_1")
for(q in 1:length(Cannot.be.0))
{
  id=match(Cannot.be.0[q],names(DATA.bio))
  DATA.bio[,id]=ifelse(DATA.bio[,id]==0,NA,DATA.bio[,id])
}
rm(D)


#Create data set for ecoystem analysis of GN fishery
Ecos.nams=c("SHEET_NO"  , "LINE_NO" , "SPECIES" , "TL" , "SEX"   ,         
            "AVE.SET.TIME","AVE.HAUL.TIME","SOAK.TIME", "Month" , "year" ,          
            "BOAT" , "BLOCK" , "SKIPPER" , "END_SET" , "END_HAUL" ,
            "BOTDEPTH"  ,  "MESH_SIZE" ,  "MESH_DROP" , "NET_LENGTH" , "END1LATD"  ,     
            "END1LATM"  ,"END1LNGD"  ,  "END1LNGM" ,"END2LATD" ,  "END2LATM"  ,     
            "END2LNGD" , "END2LNGM" , "N.hooks" ,"gillnet effort", "Mid.Lat",        
            "Mid.Long"  , "Longline effort", "Method"   ,  "FL","PL","Moon"  ,  "date"    ,       
            "Day"   , "Set.time" , "Haul.time" , "Number" ,        
            "Lat.round" , "Long.round" ,"zone","COMMENTS.hdr" )

DATA.ecosystems=DATA[,match(Ecos.nams,names(DATA))]

Chnge.this=c("SOAK.TIME","gillnet effort","Longline effort")
To.this=  c("SOAK_TIME","gillnet.effort","Longline.effort")
names(DATA.ecosystems)[match(Chnge.this,names(DATA.ecosystems))]=To.this

DATA.ecosystems=merge(DATA.ecosystems,SPECIES.names,by.x="SPECIES",by.y="Species",all.x=T)

#Fix reproductive biology vars    UTERINESTG
DATA.bio$CLASP_CALC=with(DATA.bio,ifelse(CLASP_CALC=="y","Y",ifelse(CLASP_CALC=='n','N',
                        ifelse(CLASP_CALC=="p","P",CLASP_CALC))))
DATA.bio$RUN_SPERM=with(DATA.bio,ifelse(RUN_SPERM=="y","Y",ifelse(RUN_SPERM=='n','N',RUN_SPERM)))
DATA.bio$RUN_SPERM=with(DATA.bio,ifelse(SEX=="F"|RUN_SPERM%in%c("1","2","F"),NA,RUN_SPERM))
DATA.bio$MAXOVRYDIA=with(DATA.bio,ifelse(MAXOVRYDIA==450,45,MAXOVRYDIA))
DATA.bio$NO_EMBRYOS=with(DATA.bio,ifelse(NO_EMBRYOS==413,NA,NO_EMBRYOS))

DATA.bio$GON_STAGE=with(DATA.bio,ifelse(!GON_STAGE%in%c("1","2","3"),NA,GON_STAGE))
DATA.bio$UTERINESTG=with(DATA.bio,ifelse(!UTERINESTG%in%c("1","2","3","4","5","6"),NA,UTERINESTG))

DATA.bio$UMBIL_SCAR=with(DATA.bio,ifelse(SPECIES=="LG"&FL>90,NA,UMBIL_SCAR))


DATA.bio$COMMON_NAME=with(DATA.bio,ifelse(COMMON_NAME=="NERVOUS SHARK","Nervous shark",
              ifelse(COMMON_NAME=="SNAGGLETOOTH","Snaggletooth shark",
                  COMMON_NAME)))

DATA.bio$SCIENTIFIC_NAME=with(DATA.bio,ifelse(COMMON_NAME=="Snaggletooth shark",'Hemipristis elongata',
                                              SCIENTIFIC_NAME))

#duplicated record (double tagged)
DATA.bio=subset(DATA.bio,!is.na(SHEET_NO))
a=subset(DATA.bio,SHEET_NO=="R00490" & FL==131 & SPECIES=="TK")
a=a[1,]
DATA.bio=subset(DATA.bio,!(SHEET_NO=="R00490" & FL==131 & SPECIES=="TK"))
DATA.bio=rbind(DATA.bio,a)


#Guitarfish, wedgefish and banjo rays composition
Res.ves=c("FLIN","HAM","HOU","NAT","RV BREAKSEA","RV Gannet","RV GANNET","RV SNIPE 2")
shov.sp=c("Guitarfish & shovelnose ray","Spotted shovelnose","Whitespot shovelnose",
          "Fiddler ray")
Shovel.prop.n=DATA.bio%>%
                filter(!BOAT%in%Res.ves & Lat.round> (-26) &COMMON_NAME %in%shov.sp)%>%
                mutate(Group=ifelse(SCIENTIFIC_NAME=='Rhynchobatus autraliae',"Wedgefishes",
                                    "Banjo rays"))%>%
              group_by(Group) %>%
              summarise (n = n()) %>%
              mutate(freq = n / sum(n))%>%
              dplyr::select(-n)%>%
              data.frame

Shovel.prop.s=DATA.bio%>%
                filter(!BOAT%in%Res.ves & Lat.round<= (-26)&COMMON_NAME %in%shov.sp)%>%
                mutate(Group=ifelse(SCIENTIFIC_NAME=='Rhynchobatus autraliae',"Wedgefishes",
                                    "Banjo rays"))%>%
                group_by(Group) %>%
                summarise (n = n()) %>%
                mutate(freq = n / sum(n))%>%
                dplyr::select(-n)%>%
                data.frame


# Change PL to Disc width for stingrays
Stingrays=35000:40000
DATA=DATA%>%
  mutate(COMMON_NAME=case_when(SPECIES=='ER' & Mid.Lat<=(-31)~'Southern eagle ray',
                               TRUE~COMMON_NAME),
         SCIENTIFIC_NAME=case_when(SPECIES=='ER' & Mid.Lat<=(-31)~'Myliobatis tenuicaudatus',
                                   TRUE~SCIENTIFIC_NAME),
         Disc.width=ifelse(CAES_Code%in%Stingrays & !is.na(PL),PL,
                           ifelse(CAES_Code%in%Stingrays & is.na(PL) & grepl("PA",SHEET_NO),TL,NA)),
         CAES_Code=ifelse(SPECIES=='ER' & Mid.Lat<=(-31),39001,CAES_Code))

DATA.bio=DATA.bio%>%
  mutate(COMMON_NAME=case_when(SPECIES=='ER' & Mid.Lat<=(-31)~'Southern eagle ray',
                               TRUE~COMMON_NAME),
         SCIENTIFIC_NAME=case_when(SPECIES=='ER' & Mid.Lat<=(-31)~'Myliobatis tenuicaudatus',
                                   TRUE~SCIENTIFIC_NAME),
         Disc.width=ifelse(CAES_Code%in%Stingrays & !is.na(PL),PL,
                           ifelse(CAES_Code%in%Stingrays & is.na(PL) & grepl("PA",SHEET_NO),TL,NA)),
         CAES_Code=ifelse(SPECIES=='ER' & Mid.Lat<=(-31),39001,CAES_Code))


DATA.ecosystems=DATA.ecosystems%>%
  mutate(COMMON_NAME=case_when(SPECIES=='ER' & Mid.Lat<=(-31)~'Southern eagle ray',
                               TRUE~COMMON_NAME),
         SCIENTIFIC_NAME=case_when(SPECIES=='ER' & Mid.Lat<=(-31)~'Myliobatis tenuicaudatus',
                                   TRUE~SCIENTIFIC_NAME),
         Disc.width=ifelse(CAES_Code%in%Stingrays & !is.na(PL),PL,
                           ifelse(CAES_Code%in%Stingrays & is.na(PL) & grepl("PA",SHEET_NO),TL,NA)),
         CAES_Code=ifelse(SPECIES=='ER' & Mid.Lat<=(-31),39001,CAES_Code))


DATA$SPECIES=with(DATA,ifelse(SHEET_NO=="PA0031" & SPECIES=="DW.T","BW",SPECIES)) #typo, ammended by Jack Parker
DATA.ecosystems$SPECIES=with(DATA.ecosystems,ifelse(SHEET_NO=="PA0031" & SPECIES=="DW.T","BW",SPECIES))
DATA.bio$SPECIES=with(DATA.bio,ifelse(SHEET_NO=="PA0031" & SPECIES=="DW.T","BW",SPECIES))


#Add lactate in Comments not entered in Lactate column
DATA$Lactate=ifelse(DATA$Lactate=="",NA,DATA$Lactate)
DATA=DATA%>%
      mutate(Lactate=case_when(is.na(Lactate) & grepl("lacta", COMMENTS)~gsub("[^[:digit:].]", "",  sub(".*lactate", "", COMMENTS)),
                               is.na(Lactate) & grepl(paste(c("lacta",'lactate:'),collapse="|"), COMMENTS) & grepl(paste(c('Hi','HI','hi','high','High',': Hi'),collapse="|"),COMMENTS)~'Hi',
                               TRUE~Lactate),
             Lactate=tolower(Lactate))


#remove gillnet effort data from Method ==LL
DATA=DATA%>%
  mutate(MESH_DROP=ifelse(Method=='LL',NA,MESH_DROP),
         NET_LENGTH=ifelse(Method=='LL',NA,NET_LENGTH),
         MESH_SIZE=ifelse(Method=='LL',NA,MESH_SIZE))
DATA.ecosystems=DATA.ecosystems%>%
        mutate(MESH_DROP=ifelse(Method=='LL',NA,MESH_DROP),
               NET_LENGTH=ifelse(Method=='LL',NA,NET_LENGTH),
               MESH_SIZE=ifelse(Method=='LL',NA,MESH_SIZE))
DATA.bio=DATA.bio%>%
        mutate(MESH_DROP=ifelse(Method=='LL',NA,MESH_DROP),
               NET_LENGTH=ifelse(Method=='LL',NA,NET_LENGTH),
               MESH_SIZE=ifelse(Method=='LL',NA,MESH_SIZE))

DATA$Mid.Lat=-abs(DATA$Mid.Lat)
DATA.bio$Mid.Lat=-abs(DATA.bio$Mid.Lat)
DATA.ecosystems$Mid.Lat=-abs(DATA.ecosystems$Mid.Lat)


# EXPORT SECTION -----------------------------------------------------------------------

write.csv(Shovel.prop.n,handl_OneDrive("Data/Catch and Effort/prop_banjo_wedge_north.csv"),row.names = F)
write.csv(Shovel.prop.s,handl_OneDrive("Data/Catch and Effort/prop_banjo_wedge_south.csv"),row.names = F)


#flush console
all.objs=ls()
all.objs=subset(all.objs,!all.objs%in%c('DATA','DATA.bio','DATA.ecosystems','handl_OneDrive','Boat_hdr','Usr','User'))

rm(list=all.objs)
