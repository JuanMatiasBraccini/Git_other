                    #SOURCE CONVENTIONAL TAGGING DATA#


library(RODBC)    		#library for importing excel data
library(lubridate)
library(dplyr)


#DATA SECTION

#WA sharks

  #Sharks.mdb data base
setwd("U:/Shark")  # working directory    
#setwd("M:/Fisheries Research/Production Databases/Shark")
channel <- odbcConnectAccess2007("Sharks v20200323.mdb")      
Tagging=sqlFetch(channel, "Tag data", colnames = F) 
Boat_bio=sqlFetch(channel, "Boat_bio", colnames = F) 
Boat_hdr=sqlFetch(channel, "Boat_hdr", colnames = F) 
Boat_bio=sqlFetch(channel, "Boat_bio", colnames = F) 
Flinders_hdr=sqlFetch(channel, "FLINDERS HDR", colnames = F) 
close(channel)  

#remove duplicated tags
Tagging=Tagging[order(Tagging$"Tag no",Tagging$"RELEASE DATE"),]
Tagging=Tagging[!duplicated(Tagging$"Tag no"),]

setwd("C:/Matias/Data/Tagging/Conventional_tagging")


Tagging.check=read.csv("C:/Matias/Data/Tagging/Conventional_tagging/Boat_bio tag releases.csv")

#Gummy shark
GummyWA=read.csv("Terry_data/InWA.csv")
GummySA=read.csv("Terry_data/OutWA.intoWA.csv")


#Species codes 
Species.Codes=read.csv("C:/Matias/Data/Species.code.csv")     
Species.Size.Range=read.csv("C:/Matias/Data/Species.Size.Range.csv")      




#PROCEDURE SECTION

#Early manipulations
Flinders_hdr$Method="DL"
Flinders_hdr=subset(Flinders_hdr,select=c(SHEET_NO,Method))
Boat_hdr=subset(Boat_hdr,select=c(SHEET_NO,Method))
Boat_hdr$Method=as.character(Boat_hdr$Method)
Gear=rbind(Boat_hdr,Flinders_hdr)


#Identification of wobbegongs was found to be unreliable so set all wobbies to general
Tagging$SPECIES=as.character(Tagging$SPECIES)
Tagging$SPECIES=with(Tagging,ifelse(SPECIES%in%c("WC","WD","WS","WW"),"WB",SPECIES))

Tagging$SPECIES=with(Tagging,ifelse(SPECIES=="LP","ZE",SPECIES))   #LP is zebra shark

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

#Fix Tag.no if incomplete
Tagging$Tag.no=as.character(Tagging$FINTAGNO)
Tagging$ATAG.NO=Tagging$"ATAG NO"
Tagging$RELEASE.DATE=Tagging$"RELEASE DATE"


Tagging %>% mutate(across(where(is.factor), as.character)) -> Tagging  #convert all factors to character
Boat_bio %>% mutate(across(where(is.factor), as.character)) -> Boat_bio
Gummy %>% mutate(across(where(is.factor), as.character)) -> Gummy

Tagging=Tagging%>%
          rename(Tag.no2='Tag no',
                 Recaptured="Captured?")%>%
          mutate(CONDITION=ifelse(CONDITION=="?",NA,CONDITION),
                 Tag.type=case_when(!is.na(Tag.no)|!is.na(Tag.no2)~'conventional',
                                    is.na(Tag.no) & !is.na(DARTTAGNO)~'conventional.dart',
                                    is.na(Tag.no) & !is.na(ATAG.NO)~'acoustic',
                                    TRUE~'unknown'),
                 Tag.no=ifelse(is.na(Tag.no) & !is.na(Tag.no2),Tag.no2,
                        ifelse(is.na(Tag.no) & !is.na(DARTTAGNO),DARTTAGNO,
                        ifelse(is.na(Tag.no) & !is.na(ATAG.NO),ATAG.NO,
                        Tag.no))))%>%
          dplyr::select(-Tag.no2)%>%
          mutate(Tag.no=tolower(Tag.no))


#Add TL to species where TL is measured but not FL and any tagging event not in Tagging
Boat_bio=Boat_bio%>%
            rename(Tag.no="FINTAG NO",
                   DARTTAGNO="DART TAG NO",
                   ATAG.NO="ATAG NO",
                   FINTAG.2="FINTAG 2")%>%
            dplyr::select(SHEET_NO,SPECIES,FL,TL,Tag.no,DARTTAGNO,ATAG.NO,FINTAG.2)%>%
            mutate(Tag.type2=case_when(!is.na(Tag.no)|!is.na(FINTAG.2)~'conventional',
                                      is.na(Tag.no) & !is.na(DARTTAGNO)~'conventional.dart',
                                      is.na(Tag.no) & !is.na(ATAG.NO)~'acoustic',
                                      TRUE~'unknown'),
                    Tag.no=ifelse(is.na(Tag.no) & !is.na(DARTTAGNO),paste("D",DARTTAGNO,sep=""),
                                ifelse(is.na(Tag.no) & !is.na(ATAG.NO),paste("A",ATAG.NO,sep=""),
                                Tag.no)))%>%
            filter(!is.na(Tag.no))%>%
            mutate(Tag.no=tolower(Tag.no))
Tagging=full_join(Tagging,subset(Boat_bio,select=c(SPECIES,TL,Tag.no,Tag.type2)),by=c("SPECIES","Tag.no"))%>%
            mutate(FL=ifelse(is.na(FL)&!is.na(TL),TL*.85,FL),
                   Tag.type=ifelse(is.na(Tag.type)&!is.na(Tag.type2),Tag.type2,Tag.type))%>%
        dplyr::select(-Tag.type2)



#check for duplicates   
Tagging$Unico=with(Tagging,paste(Tag.no,SPECIES,FL))
ind=which(duplicated(Tagging$Unico)==T)
Dup.Tags=Tagging$Unico[ind]
if(length(Dup.Tags)>0)Tagging=Tagging[!(duplicated(Tagging$Unico)),-match("Unico",names(Tagging))]


#subset tagging data
Tagging$Day.rel=day(Tagging$RELEASE.DATE)    
Tagging$Mn.rel=month(Tagging$RELEASE.DATE)
Tagging$Yr.rel=year(Tagging$RELEASE.DATE)
Tagging$Day.rec=day(Tagging$DATE_CAPTR)
Tagging$Mn.rec=month(Tagging$DATE_CAPTR)
Tagging$Yr.rec=year(Tagging$DATE_CAPTR)
these.vars=c("SHEET_NO","SPECIES","FINTAGNO","Tag.no","Tag.type","FL","SEX","CONDITION","REL_LATD","REL_LATM",
             "REL_LNGD","REL_LNGM","Day.rel","Mn.rel","Yr.rel",
             "CAPT_METHD","Recaptured","CAP_LATD","CAP_LATM","CAP_LNGD","CAP_LNGM","CAP_FL",
             "Day.rec","Mn.rec","Yr.rec")
Tagging=Tagging[,match(these.vars,names(Tagging))]%>%
              mutate(Recaptured=ifelse(Recaptured%in%c("Y","y"),"YES","NO"))


#combine degrees and minutes (already in decimales degrees)
Tagging$REL_LNGM=ifelse(Tagging$REL_LNGM>100,0,Tagging$REL_LNGM)  #remove nonsense
Tagging$Lat.rels=with(Tagging,-(REL_LATD+(REL_LATM)/100))
Tagging$Long.rels=with(Tagging,(REL_LNGD+(REL_LNGM)/100))
Tagging$Lat.rec=with(Tagging,-(CAP_LATD+(CAP_LATM)/100))
Tagging$Long.rec=with(Tagging,(CAP_LNGD+(CAP_LNGM)/100))

#drop redundant vars
Drop.this=c("REL_LNGD","REL_LNGM","CAP_LATD","CAP_LATM","CAP_LNGD","CAP_LNGM","REL_LATD","REL_LATM")
Tagging=Tagging[,-match(Drop.this,names(Tagging))]



#add gummy tagging data
Gummy$Tag.type="conventional"
Gummy=Gummy[,match(names(Tagging),names(Gummy))]
Tagging=rbind(Tagging,Gummy)



#change nonsense recapture data
Tag.nonsense.rec=c(476,3021)
these.nonsense.rec=match(Tag.nonsense.rec,Tagging$FINTAGNO)
na.these.cols=match(c("Lat.rec","Long.rec"),names(Tagging))
Tagging[these.nonsense.rec,na.these.cols]=NA  #set to NA the recapture location

Tagging=Tagging%>%
            mutate(Recaptured=ifelse(Recaptured=="YES" & is.na(CAP_FL)
                                     & is.na(Day.rec) & is.na(Mn.rec) & is.na(Yr.rec)
                                     & is.na(Lat.rec) & is.na(Long.rec),"NO",Recaptured),
                   Lat.rec=ifelse(Recaptured=="NO",NA,Lat.rec),
                   Long.rec=ifelse(Recaptured=="NO",NA,Long.rec))

#fix species names
Tagging$SPECIES=toupper(Tagging$SPECIES)

#change awkward names
colnames(Tagging)[match(c("SEX","SPECIES","FL"),names(Tagging))]=
  c("Sex","Species","Rel_FL")

#fix sex
Tagging$Sex=ifelse(Tagging$Sex%in%c("F","f"),"F",ifelse(Tagging$Sex%in%c("m","M"),"M","U"))


#Create usefull variables

#areas
eachArea=c("JASDGDLF.zone2","JASDGDLF.zone1","WCDGDLF","closed","WANCSF","JANSF")
Tagging$Areas=with(Tagging,ifelse(Lat.rels<=(-26) & Lat.rels>=(-33) & Long.rels <=116,eachArea[3],
                                  ifelse(Lat.rels<(-33) & Lat.rels>=(-41) & Long.rels <=116.5,eachArea[2],
                                         ifelse(Lat.rels<(-31) & Lat.rels>=(-41) & Long.rels >116.5,eachArea[1],
                                                ifelse(Long.rels>=114 & Long.rels<=123.75 & Lat.rels >=(-18),eachArea[5],
                                                       ifelse(Long.rels>123.75 & Lat.rels >=(-18),eachArea[6],eachArea[4]))))))
Tagging$Areas=with(Tagging,ifelse(Lat.rels<=(-29) & Long.rels >129,"SA",Areas))


Tagging$Areas.rec=with(Tagging,ifelse(Lat.rec<=(-26) & Lat.rec>=(-33) & Long.rec <=116,eachArea[3],
                      ifelse(Lat.rec<(-33) & Lat.rec>=(-41) & Long.rec <=116.5,eachArea[2],
                      ifelse(Lat.rec<(-31) & Lat.rec>=(-41) & Long.rec >116.5,eachArea[1],
                      ifelse(Long.rec>=114 & Long.rec<=123.75 & Lat.rec >=(-18),eachArea[5],
                      ifelse(Long.rec>123.75 & Lat.rec >=(-18),eachArea[6],eachArea[4]))))))
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
Tagging=left_join(Tagging,Species.Codes,by="Species")


#Remove unknown species
Tagging=subset(Tagging,!Species=="")


#Fix some dates
Tagging$Day.rec=with(Tagging,ifelse(FINTAGNO=="A154A",10,Day.rec))
Tagging$Mn.rec=with(Tagging,ifelse(FINTAGNO=="A154A",3,Mn.rec))
Tagging$Yr.rec=with(Tagging,ifelse(FINTAGNO=="A154A",2013,Yr.rec))

Tagging$Day.rec=with(Tagging,ifelse(FINTAGNO%in%c("884","3326","3327"),NA,Day.rec))
Tagging$Mn.rec=with(Tagging,ifelse(FINTAGNO%in%c("884","3326","3327"),NA,Mn.rec))
Tagging$Yr.rec=with(Tagging,ifelse(FINTAGNO%in%c("884","3326","3327"),NA,Yr.rec))


ID=which(Tagging$FINTAGNO=="239" & Tagging$Yr.rel==1900)
if(length(ID)>0) Tagging=Tagging[-ID,]
Tagging$Day.rec=with(Tagging,ifelse(FINTAGNO%in%c("239"),9,Day.rec))
Tagging$Mn.rec=with(Tagging,ifelse(FINTAGNO%in%c("239"),12,Mn.rec))
Tagging$Yr.rec=with(Tagging,ifelse(FINTAGNO%in%c("239"),1997,Yr.rec))


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

Tagging$Yr.rel=with(Tagging,ifelse(Yr.rel==2020,NA,Yr.rel))

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

#Add sampling method
Gear=subset(Gear,SHEET_NO%in%unique(Tagging$SHEET_NO))
Gear=Gear[!duplicated(Gear$SHEET_NO),]
Tagging=merge(Tagging,Gear,by="SHEET_NO",all.x=T)
names(Tagging)[match("Method",names(Tagging))]="REL_METHD"





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
