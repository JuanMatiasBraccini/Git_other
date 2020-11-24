# SCRIPT FOR SOURCING BIOLOGICAL DATA FROM SHARK DATABASE

library(RODBC)
library(lunar)   #moon phases
library(lubridate)
library(tidyverse)
options(stringsAsFactors = FALSE)


# DATA SECTION -----------------------------------------------------------------------


#Sharks data base
setwd("U:/Shark")  # working directory
#setwd('C:/Matias')  #while working from home
channel <- odbcConnectAccess2007("Sharks v20200323.mdb")  #new databased updated by Vero
#channel <- odbcConnectAccess2007("Sharks.mdb") 

Boat_bio=sqlFetch(channel, "Boat_bio", colnames = F) 
Scalefish=sqlFetch(channel, "Scalefish", colnames = F) 
Boat_hdr=sqlFetch(channel, "Boat_hdr", colnames = F)   
close(channel)

#Species names
SPECIES.names=read.csv("C:/Matias/Data/Species.code.csv")


#Species historically recorded in Boat header comments
Boat_bio_header_sp=read.csv("C:/Matias/Data/Shark_bio/Missing_species_from_comments.csv",stringsAsFactors=F)


#TL-FL coefficients
TL_FL=read.csv("C:/Matias/Data/Naturaliste/FL_to_TL.csv",stringsAsFactors=F)




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
  mutate(date=as.Date(DATE,format="%Y-%m-%d"),
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


#Merge biological and sampling information  
Use.full.boat.hdr=c("Count","season","ZONE","BOTTYPE","CTCHABLITY","RECORDER",
                    "SEA_CONDTN","MOON","MOON PHASE","SUBBLOCK",
                    "SEA/WEATHER CONDITIONS","Region","Buffer Zone")
Use.full.boat.hdr=Boat_hdr[,-match(Use.full.boat.hdr,names(Boat_hdr))]
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


#Expand records from bycatch species that were reported as a total sum
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

#add hook info to scalefish. Blank SPECIES in Boat.bio is the line number of a scalefish
these.are.scalies=Boat_bio%>%
                    filter(is.na(SPECIES))%>%
                    dplyr::select(UNIQUE_ID,NewComments,LostFlag,DeadFlag,HookedTime,ReleasedTime,BaitSpeciesId,
                                  HookLocation,HookType,HookSize,WireTrace,RetainedFlag)
Scalefish=Scalefish%>%
          mutate(dummy=`Line no`,
                 dummy=ifelse(nchar(dummy)==2,paste('0',dummy,sep=''),
                       ifelse(nchar(dummy)==1,paste('00',dummy,sep=''),
                              dummy)),
                 UNIQUE_ID=paste(SHEET_NO,dummy,sep=''))%>%
      dplyr::select(-dummy)%>%
        left_join(these.are.scalies,by="UNIQUE_ID")


#remove typos
typos.shk=c( "BZ"  , "CS" ,"BM","GK")
DATA=subset(DATA,!SPECIES%in%typos.shk)

#scale up unmeasured scalefish
names(Scalefish)[match("NO UNMEASURED",names(Scalefish))]="Numbers"

Scalefish$SPECIES=with(Scalefish,ifelse(SPECIES=="HM","MK",ifelse(SPECIES=="SD","BL",
                    ifelse(SPECIES=="SW","SL",SPECIES))))



typos=c("bp","bt","bx","jf","pj","qQ","wd","ww",
        "FE","JK","KW","LK","NG","PF","SD","SF",
        "SR","WA","WH","XC","XR")


Scalefish=subset(Scalefish,!SPECIES%in%typos)

Scalefish$Numbers=with(Scalefish,ifelse(is.na(Numbers),1,ifelse(Numbers==0,1,Numbers)))
Scalefish$SHEET_NO=with(Scalefish,ifelse(SHEET_NO=="s00265","S00265",SHEET_NO))

Expand.scale=subset(Scalefish,Numbers>1)
Scalefish=subset(Scalefish,Numbers==1)
Expand.scale$TL=with(Expand.scale,ifelse(TL==0,NA,TL))
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


#Fix sex
DATA$SEX=with(DATA,ifelse(SEX=="f","F",ifelse(SEX=="m","M",
      ifelse(SEX%in%c(",","?","9","B","n","N","p","P","u","Y"),NA,SEX))))


Cnhg=match(c("NO HOOKS","DART TAG NO","FINTAG NO","FINTAG 2","ATAG NO","BAG NO"),names(DATA))
names(DATA)[Cnhg]=c("N.hooks","DART_TAG_NO","FINTAG_NO","FINTAG_2","ATAG_NO","BAG_NO")

#Add bycatch species in boat header comments
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
DATA$TEMP=with(DATA,ifelse(TEMP==2537,25.7,TEMP))

#Convert TL to FL if FL is NA and there is TL info
DATA=merge(DATA,TL_FL[,match(c("SPECIES","a.intercept","b.slope"),names(TL_FL))],by="SPECIES",all.x=T)
DATA$TL=with(DATA,ifelse(TL==0,NA,TL))
DATA$FL=with(DATA,ifelse(!is.na(FL) & !is.na(TL) & FL>TL,NA,FL))
DATA$FL=with(DATA,ifelse(is.na(FL)&!is.na(TL),(TL-a.intercept)/b.slope,FL))
DATA=DATA[,-match(c("a.intercept","b.slope"),names(DATA))]


#Create Biological data dataframe
keep.biol=c("SHEET_NO","SPECIES","date","TL","FL","PL","SEX","RELEASE CONDITION","UMBIL_SCAR", "NO_DISCARDS",
            "CLASPLENTH","CLASP_CALC","GON_STAGE","RUN_SPERM","MAXOVRYDIA","NO_YOLKOVA","UTERINESTG","NO_EMBRYOS",
            "NO_UNDEVELOPED","EMBLEN_1","EMBLEN_2","EMBLEN_3","EMBLEN_4","EMBLEN_5","EMBLEN_6","EMBLEN_7","EMBLEN_8",
            "EMBLEN_9","EMBLEN_10","EMBLEN_11","EMBLEN_12","STMCH_FULL","STMCH_CONT",
            "ATAG_NO","DART_TAG_NO","FINTAG_NO","FINTAG_2","BAG_NO","COMMENTS","COMMENTS.hdr")
DATA.bio=DATA[,match(keep.biol,names(DATA))]

#Merge scalefish and shark data
DATA$Numbers=1

Scalefish$VERT_SAMPL=NA
Scalefish$SEX=as.character(Scalefish$SEX)
Scalefish$SEX=with(Scalefish,ifelse(SEX=="f","F",ifelse(SEX=="m","M",SEX)))
Scalefish$SPECIES=as.character(Scalefish$SPECIES)
names(Scalefish)[match(c("NO HOOKS"),names(Scalefish))]=c("N.hooks")
Scalefish$BAG_NO=""
add.these.cols=c('PL','COMMENTS',"Weight","BloodFlag","FinClipFlag","MuscleFlag","Lactate","BleedingFlag",
                 "HookRemoved","TrunkL")
for(i in 1:length(add.these.cols))
{
  if(!add.these.cols[i]%in%names(Scalefish)) Scalefish=Scalefish%>%add_column(!!(add.these.cols[i]) :=NA)
}

DATA=DATA[,match(names(Scalefish),names(DATA))]



#Combine shark and scalefish datasets
DATA$TYPE="Elasmo"
Scalefish$TYPE="Scalefish"

DATA=rbind(DATA,Scalefish)  
DATA$TYPE=with(DATA,ifelse(grepl(".T",SPECIES),'Scalefish',TYPE))
DATA$TYPE=ifelse(DATA$SPECIES=="BT",'Elasmo',DATA$TYPE)

#Fix lats
names(DATA)[match(c("MID LAT","MID LONG"),names(DATA))]=c('Mid.Lat','Mid.Long')
DATA$Mid.Lat=-DATA$Mid.Lat
DATA$END1LATD=-DATA$END1LATD
DATA$END2LATD=-DATA$END2LATD


#Add moon phase
#note: 0 and 100 are full moon, 50 is new moon, 25 last quarter and 75 first quater
DATA$Moon=lunar.phase(DATA$date,name = T)

#Remove NA species
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

DATA$Mid.Lat=with(DATA,ifelse(Mid.Lat>(-1) &!is.na(BLOCK),-as.numeric(substr(BLOCK,1,2)),Mid.Lat))
DATA$Mid.Lat=with(DATA,ifelse(END1LATD==0 &!is.na(BLOCK),-as.numeric(substr(BLOCK,1,2)),Mid.Lat))

DATA$Mid.Long=with(DATA,ifelse(Mid.Long==0 &!is.na(BLOCK),100+as.numeric(substr(BLOCK,3,4)),Mid.Long))

DATA$Mid.Lat=with(DATA,ifelse(!substr(Mid.Lat,1,3)==(-as.numeric(substr(BLOCK,1,2))),-as.numeric(substr(BLOCK,1,2)),Mid.Lat))

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


DATA$Lat.round = ceiling(DATA$Mid.Lat) -0.5
DATA$Long.round = floor(DATA$Mid.Long) +0.5



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


#Fix sex
DATA$SEX=with(DATA,ifelse(SEX=="f","F",ifelse(SEX=="m","M",
      ifelse(SEX%in%c(",","?","n","N","p","P","Y"),"U",SEX))))

#Fix FL
DATA$FL=with(DATA,ifelse(FL<10,NA,FL))

#Re set minimum FL observed 
DATA$FL=with(DATA,ifelse(SPECIES=="BW" & FL<60,60,ifelse(SPECIES=="WH" & FL<25,25,
                       ifelse(SPECIES=="SD" & FL<20,20,FL))))

DATA$FL=with(DATA,ifelse(SPECIES=="MI" & FL==690,69,FL))

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



# EXPORT SECTION -----------------------------------------------------------------------

write.csv(Shovel.prop.n,'C:/Matias/Data/Catch and Effort/prop_banjo_wedge_north.csv',row.names = F)
write.csv(Shovel.prop.s,'C:/Matias/Data/Catch and Effort/prop_banjo_wedge_south.csv',row.names = F)


#flush console
all.objs=ls()
all.objs=subset(all.objs,!all.objs%in%c('DATA','DATA.bio','DATA.ecosystems'))

rm(list=all.objs)
