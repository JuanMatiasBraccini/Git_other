library(tidyverse)
library(cowplot)
library(readxl)
library(data.table)
library(Hmisc)
library(countrycode)
library(rnaturalearth)
library(ggrepel)
library(scales)

if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
hndl.out=handl_OneDrive("Scientific manuscripts/Perspective_Double standards/6. Outputs/")
source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/ggplot.themes.R'))

# Infographic - Production, exports and imports --------------------------------------------------------------------

#1. Bring in data
  #1.1 Production, total imports, total exports
# source https://www.agriculture.gov.au/abares/research-topics/fisheries/fisheries-data#australian-fisheries-and-aquaculture-statistics-2022
  #these tables have production, exports and imports but by either commodity or country
  #Tables 13 & 14 have exports by commodity and country, respectively
  #Tables 15 & 16 have imports by commodity and country, respectively

Exports=fun1(d=read_excel(paste0(handl_OneDrive("Scientific manuscripts/Perspective_Double standards/1. Data sets/"),
                                 'AustFishAquacStats_2022_Tables_v1.0.0.xlsx'), sheet = "Table 13",skip = 7)%>%
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
                                          TRUE~Commodity)),
             Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))%>%
  filter(!Group=="Other")%>%
  mutate(Commodity=gsub("\\.(?=[a-zA-Z])", " ", Commodity, perl = TRUE),
         Commodity=case_when(Commodity=="other finfish. sharks and rays"~"other finfish, sharks & rays",
                             Commodity=="other crustaceans and molluscs"~"other crustaceans & molluscs",
                             TRUE~Commodity),
         Commodity=capitalize(Commodity))%>%
  filter(Units=="Volume (1000s tonnes)")%>%
  group_by(Year,Units,Commodity)%>%
  summarise(Quantity=sum(Quantity,na.rm=T))%>%
  ungroup()%>%
  mutate(Type='Exports')

Imports=fun1(d=read_excel(paste0(handl_OneDrive("Scientific manuscripts/Perspective_Double standards/1. Data sets/"),
                                 'AustFishAquacStats_2022_Tables_v1.0.0.xlsx'), sheet = "Table 15",skip = 7)%>%
               filter(!grepl('Includes prepared and preserved',Commodity))%>%
               filter(!Commodity%in%c("Value","Quantity","Edible c","Edible","Non-edible","Edible c",
                                      "Finfish, Sharks and Rays","Crustaceans and Molluscs",
                                      "Total","Total Finfish, Sharks and Rays","Total Crustaceans and Molluscs",
                                      "Total edible fisheries products","Total edible acd",
                                      'Marine fats and oils','Fish meal','Pearl Oysters b','Pearl Oysters e','Ornamental Finfish','Other non–edible',
                                      'Total non–edible fisheries products','Total fisheries products','Other marine products',
                                      "Other non-edible","Total non-edible fisheries products","Pearl Oysters",
                                      "Live","Total non-edible","Edible a","Total Finfish, Sharks and Rays c",
                                      "Other edible","Edible a","Peal Oysters","Total edible fisheries products acd"))%>%
               mutate(unit=case_when(unit=="$m"~'M AUD',
                                     unit=="t"~'tonnes'),
                      Commodity=case_when(Commodity%in%c("Salmonids a","Salmonids b","Salmonids d")~"Salmonids",
                                          TRUE~Commodity)),
             Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))%>%
  filter(!Group=="Other")%>%
  mutate(Commodity=gsub("\\.(?=[a-zA-Z])", " ", Commodity, perl = TRUE),
         Commodity=case_when(Commodity=="other finfish. sharks and rays"~"other finfish, sharks & rays",
                             Commodity=="other finfish, sharks & rays"~"other finfish",
                             Commodity=="other crustaceans and molluscs"~"other crustaceans & molluscs",
                             TRUE~Commodity),
         Commodity=capitalize(Commodity))%>%
  filter(Units=="Volume (1000s tonnes)")%>%
  group_by(Year,Units,Group)%>%
  summarise(Quantity=sum(Quantity,na.rm=T))%>%
  ungroup()%>%
  mutate(Type='Imports')

use.this=FALSE  #superseded by #1.2.
if(use.this)
{
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
  
  
  #exports by Country
  Production_table14=read_excel(paste0(hndl.in,'AustFishAquacStats_2022_Tables_v1.0.0.xlsx'), sheet = "Table 14",skip = 7)%>%
    filter(!grepl('Excluding live species',Commodity))
  id.non.edibl=match(c("Non-edible","Total"),Production_table14$Commodity)
  Production_table14=Production_table14[-(id.non.edibl[1]:id.non.edibl[2]),]
  id.non.edibl=match(c("Non-edible","Total"),Production_table14$Commodity)
  Production_table14=Production_table14[-(id.non.edibl[1]:id.non.edibl[2]),]
  Production_table14=Production_table14%>%
    filter(!Commodity%in%c("Value","Quantity","Edible a","Edible c","Edible","Non-edible","Edible c",
                           "Finfish, Sharks and Rays","Crustaceans and Molluscs",
                           "Total","Total Finfish, Sharks and Rays","Total Crustaceans and Molluscs",
                           "Total edible fisheries products","Total edible",
                           'Marine fats and oils','Fish meal','Pearl Oysters b','Ornamental Finfish','Other non–edible',
                           'Total non–edible fisheries products','Total fisheries products',
                           "Other non-edible","Total non-edible fisheries products","Pearl Oysters"))%>%
    mutate(unit=case_when(unit=="$’000"~'thousands AUD',
                          unit=="t"~'tonnes'))  
  #imports by Country
  Production_table16=read_excel(paste0(hndl.in,'AustFishAquacStats_2022_Tables_v1.0.0.xlsx'), sheet = "Table 16",skip = 7)%>%
    filter(!grepl('Excluding live species',Commodity))
  id.non.edibl=match(c("Non-edible","Total non-edible"),Production_table16$Commodity)
  Production_table16=Production_table16[-(id.non.edibl[1]:id.non.edibl[2]),]
  id.non.edibl=match(c("Non-edible","Total non-edible"),Production_table16$Commodity)
  Production_table16=Production_table16[-(id.non.edibl[1]:id.non.edibl[2]),]
  Production_table16=Production_table16%>%
    filter(!Commodity%in%c("Value","Quantity","Edible a","Edible c","Edible","Non-edible","Edible c",
                           "Finfish, Sharks and Rays","Crustaceans and Molluscs",
                           "Total","Total Finfish, Sharks and Rays","Total Crustaceans and Molluscs",
                           "Total edible fisheries products","Total edible","Total imports",
                           'Marine fats and oils','Fish meal','Pearl Oysters b','Ornamental Finfish','Other non–edible',
                           'Total non–edible fisheries products','Total fisheries products',
                           "Other non-edible","Total non-edible fisheries products","Pearl Oysters",
                           "Ediblea"))%>%
    mutate(unit=case_when(unit=="$’000"~'thousands AUD',
                          unit=="t"~'tonnes'))  
  
}

  #1.2. Imports and exports by Country, Australian State and TradeCode
#source https://www.agriculture.gov.au/abares/research-topics/trade/dashboard
hndl.in=handl_OneDrive("Data/Seafood imports and exports/") 
ABARES_trade_data=fread(paste0(hndl.in,'ABARES_trade_data.csv'))%>%data.frame
C3803499_HTISC=fread(paste0(hndl.in,'C3803499_HTISC.csv'))%>%data.frame
C4203471_AHECC=fread(paste0(hndl.in,'C4203471_AHECC.csv'))%>%data.frame
Fish_codes=fread(paste0(hndl.in,'Fish codes.csv'))%>%data.frame%>%
  filter(Keep==1)%>%
  filter(!grepl('unfit for',tolower(Description)))#remove unfit for human consumption

Code.description=rbind(C3803499_HTISC,C4203471_AHECC)%>%
  distinct(Code, .keep_all = T)%>%
  rename(TradeCode=Code)%>%
  filter(TradeCode%in%unique(ABARES_trade_data$TradeCode))

ABARES_trade_data=ABARES_trade_data%>%
                  left_join(Code.description,by='TradeCode')%>%
                  mutate(Country=case_when(grepl('France',Overseas_location)~'France',
                                           Overseas_location=="Antarctica, nfd" ~"Antarctica" ,
                                           Overseas_location%in%c("Australia (Re-imports)","Christmas Island",
                                                                  "Cocos (Keeling) Islands","Norfolk Island",
                                                                  "JPDA (Joint Petroleum Development Area - administered by Australia and Timor-Leste)") ~"Australia" ,
                                           Overseas_location=="Belgium and Luxembourg" ~"Belgium" ,
                                           grepl('China',Overseas_location)~'China',
                                           Overseas_location=="Denmark (includes Greenland and Faroe Islands)" ~"Denmark",
                                           Overseas_location%in%c("Falkland Islands (includes South Georgia and South Sandwich Islands)",
                                                                  "United Kingdom, Channel Islands and Isle of Man, nfd") ~"United Kingdom",
                                           Overseas_location%in%c("Former USSR, nfd","Russian Federation") ~"Russia",
                                           Overseas_location=="Italy (includes Holy See and San Marino)" ~"Italy",
                                           grepl('Netherlands',Overseas_location)~'Netherlands',
                                           grepl('Serbia',Overseas_location)~'Serbia',
                                           grepl('Switzerland',Overseas_location)~'Switzerland',
                                           grepl('United States',Overseas_location)~'United States',
                                           TRUE~Overseas_location),
                         Country=case_when(Country%in%c("No Country Details","Ship and Aircraft Stores",
                                                        "Trust Territory Pac Isld","Unidentified","Unknown")~"NA",
                                           TRUE~Country))

ABARES_trade_data=ABARES_trade_data%>%filter(TradeCode%in%unique(Fish_codes$TradeCode))
ABARES_trade_data$Continent=countrycode(sourcevar = ABARES_trade_data$Country, origin = "country.name", destination = "continent")
ABARES_trade_data=ABARES_trade_data%>%
                  mutate(Continent=case_when(Country=="French Antilles (Guadeloupe and Martinique)"~"Americas",
                                            Country=="International Waters"~"International Waters",
                                            Country%in%c("No Country Details","Ship and Aircraft Stores",
                                                         "Trust Territory Pac Isld","Unidentified","Unknown")~"NA",
                                   TRUE~Continent),
                         Group=tolower(Description),
                         Group=str_remove(Group, paste0('excluding', "\\s+.*")),
                         Group=str_remove(Group, paste0('excl.', "\\s+.*")),
                         Group=str_remove(Group, paste0('excl', "\\s+.*")),
                         Group1=case_when(TradeCode%in%c(5119110,511910019,511910020)~'Unspecified fish/invertebrate',
                                          TradeCode%in%c(305300016,305540074,305530073,3055991,305590079,3055992,305590026,3055990,305590080,
                                                        3055910,305520072,306930013,306920012,306910011,306950015,15042090,15042010,1504200006,
                                                        305790099,3057950,305720092,1504100005,305200015,305200013,305200014,1504100034,15041090,
                                                        15041010,1504100050,302700027,302900027,302910001,303800024,
                                                        303900077,3039101,303910090,305200032)~'Dried fish; fish oil, livers & extracts',
                                          TradeCode%in%c(2301200003,309900090,3099090,3051001,3091010,305100031,309100010,2301200031)~'Fish flour',
                                          TradeCode%in%c(308120042,308110041,1605610065,3081901,308190043,308190042)~'Sea cucumbers',
                                          TradeCode%in%c(308220052,308210051,1605620066,3082901,308290053,308290052)~'Sea urchins',
                                          
                                          TradeCode%in%c(307490020)~'Squid',
                                           grepl(paste(c('clams',"cockles"),collapse='|'),Group)~'Clams & cockles',
                                                          TradeCode%in%c(306190005,306290012,306190026,306290027,306990020,1605400010,
                                                                         3069990,306990021,3061901,3061961,306190047,3061960,306190046,
                                                                         306190052,3063990,306390008,306390007,
                                                                         1605400011,16054090,16054010,16054030,1605400021,1605400023,
                                                                         3062950,306290009,3062901,306290072)~'Other crustaceans',
                                          grepl(paste(c('cuttle fish','sepia'),collapse='|'),Group)~'Cuttlefish',
                                          grepl(paste(c('abalone','albalone','haliotis'),collapse='|'),Group)~'Abalone',
                                          TradeCode%in%c(307920060,3079201,307920050,307910034,3079101,307910039,307910035,3079190,3079111,
                                                         307910040,3079991,307990032,307990036,307990025,307910024,3079990,3079901,307990061,
                                                         307990028,307910030,1605900012,1605900014,1605900013,3079992,307990062,
                                                         1605900063,16055901,1605590090,1605590091,1605900015,16059092,16059099,16059019,
                                                         16059090,1605900062,1605900061)~'Other molluscs',
                                          TradeCode%in%c(307320017,307310016,307390017,3073951,307390038,3073950,307390037,3073990,3073910,
                                                         1605530043)~'Mussels',
                                          TradeCode%in%c(307520022,307510021,3075951,307590024,307590022,3075950,307590023,3075990,3075910,
                                                         1605550045)~'Octopus',
                                          TradeCode%in%c(307120011,307110010,3071901,307190012,307190011,307100013,
                                                         1605510041)~'Oysters',
                                          TradeCode%in%c(3072910,307220015,3072201,307220016,307210014,3072101,16059091,
                                                         1605520042,3072990,307290015,3072901,307290036,307290035,3072991,307290037)~'Scallops',
                                          TradeCode%in%c(3062119,306120002,3061218,306120025,3061520,306150027,
                                                         3061123,3061133,3061122,3061132,306110024,3061124,3061150,306110001,
                                                         3061191,3061190,3061121,3061131,306320002,306310001,
                                                         1605300008,306220007,306220008,16053090,16053020,1605300020,1605300022,
                                                         1605300009,16053010,3062130,3062140,3062129,3061119,3062193,3062191,
                                                         3062199,306220033,3062201,306220002,306210001,3062120,3062122,306210006,
                                                         3062192,3062190,3062121,3062111,3061111)~'Lobsters',
                                          grepl(paste(c('freshwater crayfish'),collapse='|'),Group)~'Freshwater crayfish',
                                          TradeCode%in%c(3061621,306160028,306170034,306170029,3061750,3061390,306130003,
                                                         306130040,306130042,3061320,306170033,306130041,306360006,
                                                         16052090,1605200019,1605200007,1605290090,16052010,1605210081,1605200006,
                                                         1605200018,306230009,1605200005,306230010,306260006,3062310,
                                                         306270007,3062390,306230060,306230062,306230061,3061310)~'Prawns',
                                          TradeCode%in%c(306140004,3061419,306140026,306330003,16051020,1605100010,1605100017,
                                                         16051090,1605100004,16051010,1605100016,1605100003,306240011,3062402,306240004)~'Crabs',
                                          grepl(paste(c('aquatic invertebrates','jellyfish'),collapse='|'),Group)~'Other aquatic invertebrates',
                                          TradeCode%in%c(305320042,16030012,304100002,304100036,
                                                         305300033,3053901,305390049,305390050,305300017,305100012,3049090,3037912,3037990,
                                                         16042011,302690026,303790023,303790002,305690030,3056950,305690089,3056951,305690090,
                                                         302290013,3044901,304490080,304490079,304440073,304430072,
                                                         3028901,3026909,302690042,302890050,304100042,3041909,304190058,302990003,
                                                         3045950,304590089,3045951,304590090,302590029,3025901,302590030,302490001,302290026,
                                                         3022902,3022901,302290019,302290020,302390040,304530082,
                                                         3048901,304890040,304890039,304790029,304830033,304900042,3038950,303890079,3038951,
                                                         3037919,303790055,303890080,304200022,304200021,304200045,304200044,3042909,
                                                         304290092,304290091,3042002,304200007,304200039,304200006,304200038,3039910,
                                                         303990092,3049091,304900011,304900041,3049919,304990079,304990072,3049901,304990080,
                                                         303590090,3036901,303690069,303690070,304950073,
                                                         301990029,301990006,3019950,301990009,3019910,3019909,301990035,3019901,3019902,
                                                         3019903,301990010,1604200060,1604200070,1604200066,1604200039,1604190032,1604190031,
                                                         16042014,1604200043,1604200042,16042090,16042019,16041990,16041910,1604190034,16041920,
                                                         1604190030,1604190035,16041921,1604190033,305490062,305490061,3054951,3054901,3054950,
                                                         305490023,305490022,305490064,305490063,30289004,302890049)~'Mixed finfish',
                                         grepl(paste(c('anchov','anchovies','engraulis','sardine'),collapse='|'),Group)~'Anchovies & sardines',
                                         grepl(paste(c('caviar'),collapse='|'),Group)~'Caviar',
                                         grepl(paste(c('cod'),collapse='|'),Group)~'Cod',
                                         TradeCode%in%c(3027401,302740034,302740035,302660025,3032601,303260054,303260053,303760020,
                                                        3019201,3019202,301920004,1604170065)~'Eels',
                                         TradeCode%in%c(3025501,302550023,302630022,302530020,302530021,
                                                        303670064,3036701,303670065,304940072)~'Pollocks',
                                         grepl(paste(c('pollock','coalfish'),collapse='|'),Group)~'Pollocks',
                                         grepl(paste(c('albacore','tuna','swordfish','skipjack','bonito'),collapse='|'),Group)~'Tunas & billfish',
                                         grepl(paste(c('salmon','trout'),collapse='|'),Group)~'Salmons & trouts',
                                         TradeCode%in%c(3025601,3036801,303680065,303680066,3037910)~'Whitings',
                                         TradeCode%in%c(3027201,302720032,302720033,304320061,304310060,302710031,
                                                        302710032,3032401,303240052,303240051,304620012,304610011,
                                                        3032301,303230051,303230050,3054441,305440055,305440054,
                                                        305640085,305640084,3049301,304930074,304930071,304510079,304510080,3045101,
                                                        304330062,3053101,305310040,305310041,3027901,302790039,302790040,304630013,
                                                        3032911,303290060,3032910,303290059,
                                                        302730033,3027301,304390069,3043901,304390068,303250052,3032501,303250053,
                                                        304690019,3046901,304690018,3019301,3019302,3019303,3019304,301930007)~'Tilapias, catfish, Nile perch & Carps',
                                         TradeCode%in%c(3024601,302460006,303560059)~'Cobia',
                                         TradeCode%in%c(3041202,3026807,302680041,304550084)~'Toothfish',
                                         grepl(paste(c('toothfish'),collapse='|'),Group)~'Toothfish',
                                         TradeCode%in%c(3025201,302520019,302620021,304720021,303720016)~'Haddock',
                                         TradeCode%in%c(3025401,302540021,302540022,304740023,304290062,304290063,
                                                        3036601,303660063,303780022,303660064,304200032,304200031,304200033,
                                                        304900034,304200043,304200005,304200004,304900010,304900009,
                                                        304990071)~'Hakes',
                                         TradeCode%in%c(3024101,302400018,302410001,304860036,3035106,3035105,303510051,
                                                        303510052,305610081,3056101,305610027,303500013,305420021,305420020,
                                                        1604120022,1604120021,1604200041,16041290,16041210,1604120051,305420035,
                                                        305420052,3054220)~'Herrings',
                                         TradeCode%in%c(302450005,303550056,303550058)~'Jack & horse mackerels',
                                         TradeCode%in%c(302640023,3024401,302440004,302440005,303740018,3035401,303540054,
                                                        303540057,1604150028,1604150027,16041510,1604150053)~'Mackerels',
                                         TradeCode%in%c(3022101,302210025,302210026,303310005,303310006,302210010)~'Halibuts',
                                         TradeCode%in%c(3022201,302220011)~'Plaice',
                                         grepl(paste(c('sole'),collapse='|'),Group)~'Sole',
                                         TradeCode%in%c(303390008,3033911,303390010,3033910,303390009)~'Other flatfishes',
                                         grepl(paste(c('turbots'),collapse='|'),Group)~'Turbots',
                                         TradeCode%in%c(3028401,302840043,303770021,303840073,303840074)~'Seabass',
                                         TradeCode%in%c(3028501,302850044,302850045)~'Seabreams',
                                        grepl(paste(c('ornamental','Live Australian species of syngnathids'),collapse='|'),Group)~'Ornamental finfish',
                                        TradeCode%in%c(3011020)~'Ornamental finfish',
                                        TradeCode%in%c(302920002,303920091,1604180067,305710091)~'Shark fins',
                                        TradeCode%in%c(302650024,302810040,302810041,304560085,304470076,
                                                       3038101,303750019,303810070,303810071,304960075,304880038)~'Dogfish & other sharks',
                                        TradeCode%in%c(304480077,3028201,302820042,303820071,303820072,304970076)~'Rays & skates',
                                         TRUE~NA))

ABARES_trade_data=ABARES_trade_data%>%
          mutate(Group2=case_when(Group1%in%c("Freshwater crayfish","Lobsters","Prawns")~"Lobsters & prawns",
                                  Group1%in%c("Clams & cockles","Crabs","Cuttlefish","Mussels","Octopus","Other crustaceans",
                                              "Other molluscs","Oysters","Squid")~"Other crustaceans & molluscs",
                                  Group1%in%c("Abalone","Scallops")~'Abalone & scallops',
                                  Group1%in%c("Anchovies & sardines","Caviar","Cobia","Cod",
                                              "Dogfish & other sharks","Eels","Haddock","Hakes",
                                              "Halibuts","Herrings","Jack & horse mackerels","Mackerels","Mixed finfish",
                                              "Ornamental finfish","Ornamental fish","Plaice","Pollocks","Rays & skates","Salmons & trouts",
                                              "Seabass","Seabreams","Shark fins","Sole","Tilapias, catfish, Nile perch & Carps",
                                              "Toothfish","Tunas & billfish","Turbots","Whitings")~'Finfish',
                                  Group1%in%c("Dried fish; fish oil, livers & extracts","Fish flour")~'Fish',
                                  Group1%in%c("Other aquatic invertebrates","Sea cucumbers","Sea urchins")~'Other invertebrates',
                                  TRUE~NA))
#note: Value= in $; Quantity in kg so convert to millions of $ and tonnes
ABARES_trade_data=ABARES_trade_data%>%
                    mutate(Value_millions=Value/1e6,
                           Quantity_tonnes=Quantity/1e3)

  #Indices
#note: https://impact.economist.com/projects/illicit-trade-environment-index classes rankings as
# Major, 0-30; High, 30-50; Moderate, 50-70; Minor, 70-100 Risks
Environmental.Performance.Index=fread(paste0(hndl.in,'Environmental.Performance.Index.csv'))%>% # (Wolf et al. 2022) https://epi.yale.edu/downloads
  data.frame%>%
  dplyr::select(country,EPI.new)%>%
  rename(Country=country,
         Value=EPI.new)%>%
  mutate(Index='EPI',
         Country=case_when(Country=="United States of America"~"United States",
                           Country=="Viet Nam"~"Vietnam",
                           TRUE~Country))

Global.ilicit.trade.index=fread(paste0(hndl.in,'Global ilicit trade index.csv'))%>% # (The Economist Intelligence Unit 2018) https://www.tracit.org/illicit-trade-index-visualization.html#rankingsSection
  data.frame%>%
  rename(Value=Illicit.Trade.Index.Score)%>%
  mutate(Index='GITI',
         Country=case_when(Country=="Viet Nam"~"Vietnam",
                           Country=="Korea, Rep."~"South Korea",
                           TRUE~Country))

Global.Slavery.Index=fread(paste0(hndl.in,'2023-Global-Slavery-Index.csv'))%>% # (The Walk Free Foundation 2023) https://www.walkfree.org/global-slavery-index/
  data.frame%>%
  rename(Value='Total.vulnerability.score..')%>%
  mutate(Index='GSI',
         Country=case_when(Country=="United States of America"~"United States",
                           Country=="Viet Nam"~"Vietnam",
                           TRUE~Country))
Check.risk=FALSE
if(Check.risk)
{
  Risk.cuts=c(1, 2, 4,8 ,18 ,64)
  Risk.mat=expand.grid(Ind1=c('Minor','Moderate','High','Mayor'),
                       Ind2=c('Minor','Moderate','High','Mayor'),
                       Ind3=c('Minor','Moderate','High','Mayor'))%>%
    mutate(Ind1.value=case_when(Ind1=='Minor'~1,
                                Ind1=='Moderate'~2,
                                Ind1=='High'~3,
                                Ind1=='Mayor'~4),
           Ind2.value=case_when(Ind2=='Minor'~1,
                                Ind2=='Moderate'~2,
                                Ind2=='High'~3,
                                Ind2=='Mayor'~4),
           Ind3.value=case_when(Ind3=='Minor'~1,
                                Ind3=='Moderate'~2,
                                Ind3=='High'~3,
                                Ind3=='Mayor'~4),
           Risk.value=Ind1.value*Ind2.value*Ind3.value,
           Risk=case_when(Risk.value<=Risk.cuts[2]~'Negligible',
                          Risk.value>Risk.cuts[2] & Risk.value<=Risk.cuts[3] ~'Low',
                          Risk.value>Risk.cuts[3] & Risk.value<=Risk.cuts[4] ~'Medium',
                          Risk.value>Risk.cuts[4] & Risk.value<=Risk.cuts[5] ~'High',
                          Risk.value>Risk.cuts[5]~'Severe'))
}
Risk.cuts=c(1, 2, 4,8 ,18 ,64)
Performance.indices=rbind(Environmental.Performance.Index,
                          Global.ilicit.trade.index,
                          Global.Slavery.Index)%>%
                    spread(Index,Value)%>%
                    mutate(Country=case_when(Country=="C\xf4te d'Ivoire"~"Ivory coast",
                                             Country=="T\xfcrkiye"~"Turkiye",
                                     TRUE~Country),
                           Risk.EPI=case_when(EPI<30~'Mayor',
                                              EPI<50 & EPI>30~'High',
                                              EPI<70 & EPI>50~'Moderate',
                                              EPI>70~'Minor'),
                           Risk.GITI=case_when(GITI<30~'Mayor',
                                               GITI<50 & GITI>30~'High',
                                               GITI<70 & GITI>50~'Moderate',
                                               GITI>70~'Minor'),
                           Risk.GSI=case_when(GSI>70~'Mayor',
                                              GSI>50 & GSI<70~'High',
                                              GSI>30 & GSI<50~'Moderate',
                                              GSI<30~'Minor'),
                           GSI.rev=100-GSI,
                           Risk_formula=(EPI*max(GITI,na.rm=T)*max(GSI.rev,na.rm=T))+(GSI.rev*max(GITI,na.rm=T))+GITI,
                           Risk.EPI.value=case_when(Risk.EPI=='Minor'~1,
                                                    Risk.EPI=='Moderate'~2,
                                                    Risk.EPI=='High'~3,
                                                    Risk.EPI=='Mayor'~4),
                           Risk.GITI.value=case_when(Risk.GITI=='Minor'~1,
                                                     Risk.GITI=='Moderate'~2,
                                                     Risk.GITI=='High'~3,
                                                     Risk.GITI=='Mayor'~4),
                           Risk.GSI.value=case_when(Risk.GSI=='Minor'~1,
                                                    Risk.GSI=='Moderate'~2,
                                                    Risk.GSI=='High'~3,
                                                    Risk.GSI=='Mayor'~4),
                           Risk.value=Risk.EPI.value*Risk.GITI.value*Risk.GSI.value,
                           Risk=case_when(Risk.value<=Risk.cuts[2]~'Negligible',
                                          Risk.value>Risk.cuts[2] & Risk.value<=Risk.cuts[3] ~'Low',
                                          Risk.value>Risk.cuts[3] & Risk.value<=Risk.cuts[4] ~'Medium',
                                          Risk.value>Risk.cuts[4] & Risk.value<=Risk.cuts[5] ~'High',
                                          Risk.value>Risk.cuts[5]~'Severe'))
RiskColors=data.frame(Risk=c('Negligible','Low','Medium','High','Severe'),
                      Color=c("cornflowerblue","chartreuse3","yellow1","orange","brown1")) 
Performance.indices=Performance.indices%>%left_join(RiskColors,by='Risk')

#Define range of years
MIN.yr=min(ABARES_trade_data$Calendar_year) #1998
MAX.yr=max(ABARES_trade_data$Calendar_year)  #Past 2011, there is no export data on abalone.. Pre Covid?


#2. Plots
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
                                         "mackerel","whitings","hakes","toothfish","herrings"),collapse='|'),Commodity)~'Finfish',
                           grepl(paste(c("lobsters","prawn"),collapse='|'),Commodity)~'Rock lobsters & Prawns',
                           grepl(paste(c("abalone","scallops"),collapse='|'),Commodity)~'Abalone & Scallops',
                           grepl(paste(c("crabs","crustaceans","molluscs","octopus","mussels",
                                         "pipis","squid","oysters"),collapse='|'),Commodity)~'Other crustaceans & molluscs',
                           TRUE~Commodity),
           Group=factor(Group,levels=Grups))%>%
    mutate(Quantity=ifelse(grepl('thousands',Units),Quantity/1000,Quantity),
           Units=ifelse(grepl('AUD',Units),'Value (M AUD)',Units),
           Quantity=ifelse(grepl('tonnes',Units),Quantity/1000,Quantity),
           Units=ifelse(grepl('tonnes',Units),'Volume (1000s tonnes)',Units))

  return(d)
}
fun1.country=function(d,Grups)
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
           Quantity=as.numeric(Quantity))
  d$Group=countrycode(sourcevar = d$Commodity, origin = "country.name", destination = "continent")
  d=d%>%
    mutate(Group=ifelse(is.na(Group),"Other",Group),
           Group=factor(Group,levels=Grups))%>%
    mutate(Quantity=ifelse(grepl('thousands',Units),Quantity/1000,Quantity),
           Units=ifelse(grepl('AUD',Units),'Value (M AUD)',Units),
           Quantity=ifelse(grepl('tonnes',Units),Quantity/1000,Quantity),
           Units=ifelse(grepl('tonnes',Units),'Volume (1000s tonnes)',Units))
  
  return(d)
}
fun2=function(d,TITLE,KPTN,KLS,d.inset,UniT,NRW,thresld,
              add.inset,inset.loc,fill.inset,inset.txt.size,inset.txt.loc,
              Min.other.crus.mol)
{
  d=d%>%filter(Units%in%UniT)
     
  p=d%>%
    ggplot(aes(Year,Quantity,color=Group))+
    geom_line(linewidth=1.5)+
    theme_PA()+
    theme(legend.title = element_blank(),
          legend.position = 'top')+
    scale_color_manual(values = KLS)+ 
    guides(color = guide_legend(nrow = NRW, byrow = TRUE))+
    ylab(UniT)
  if(!is.null(TITLE))p=p+labs(title = TITLE)
  if(!is.null(KPTN))p=p+labs(caption = KPTN)
  
  d.inset=d.inset%>%filter(Units%in%UniT)
  Tab.inset=d.inset%>%
    mutate(Group = droplevels(Group))%>%
    group_by(Group,Commodity)%>%
    summarise(Quantity=sum(Quantity,na.rm=T),.groups = 'drop_last')%>%
    ungroup()%>%
    mutate(Prop=Quantity/sum(Quantity,na.rm=T))%>%
    arrange(-Prop)%>%data.frame
  names(Tab.inset)[match("Quantity",names(Tab.inset))]=UniT
  if(add.inset)
  {
    p_inset=d.inset%>%
      mutate(Group = droplevels(Group))%>%
      group_by(Group,Commodity)%>%
      summarise(Quantity=sum(Quantity,na.rm=T),.groups = 'drop_last')%>%
      group_by(Group)%>%
      mutate(Prop=Quantity/sum(Quantity,na.rm=T))%>%
      arrange(Group,-Prop)%>%
      mutate(cumulative_sum = cumsum(Prop),
             Commodity=case_when(cumulative_sum>thresld & Group=='Finfish'~'Other finfish',
                                 Quantity<Min.other.crus.mol & Group=='Other crustaceans & molluscs'~'Other cru. & mol.',
                                 TRUE~Commodity))%>%
      ungroup()%>%
      group_by(Group,Commodity)%>%
      summarise(Quantity=sum(Quantity,na.rm=T),.groups = 'drop_last')%>%
      ungroup()%>%
      mutate(Prop=Quantity/sum(Quantity,na.rm=T))%>%
      ggplot(aes(seq_along(Prop), Prop,fill = Group)) +
      geom_col(width = 1) +
      geom_text(aes(y = max(Prop)*inset.txt.loc, label = Commodity),size=inset.txt.size, angle = 90, hjust = 1,color='black') +
      coord_radial(rotate_angle = TRUE, expand = FALSE)+ 
      labs(fill = "Group") + 
      theme_void() + 
      theme(legend.position = 'none',
            panel.background = element_rect(fill = fill.inset))+
      scale_fill_manual(values = KLS)
    p=ggdraw() +
      draw_plot(p)+
        draw_plot(p_inset, x = inset.loc$x, y = inset.loc$y, width = inset.loc$x.w, height = inset.loc$x.h) 
  }
  
  return(list(p=p,Tab.commodities=Tab.inset))
}
fun3=function(d,UNITS,KLS,LINTYP,LINWID,TITLE=NULL,KPTN,Axs.t.siz,Axs.T.siz,Leg.siz)
{
  p=d%>%
    filter(Units==UNITS)%>%
    ggplot(aes(Year,Quantity))+
    geom_line(aes(linetype = Type,linewidth = Type,color=Group))+
    theme_PA(leg.siz=Leg.siz,axs.t.siz=Axs.t.siz,axs.T.siz=Axs.T.siz)+
    theme(legend.title = element_blank(),
          legend.position = 'right',
          plot.caption = element_text(hjust = 0))+
    scale_color_manual(values = KLS)+ 
   # guides(color = guide_legend(nrow = 2, byrow = TRUE), 
  #         linetype = guide_legend(nrow = 1, byrow = TRUE))+
    ylab(UNITS)+
    scale_linetype_manual(values = LINTYP)+
    scale_linewidth_manual(values = LINWID)
  if(!is.null(TITLE))p=p+labs(title = TITLE)
  if(!is.null(KPTN))p=p+labs(caption = KPTN)
  return(p)
}
fun4=function(d,UNITS,TITLE=NULL,KPTN,Axs.t.siz,Axs.T.siz,Leg.siz,NROW,KLS,nrow.leg,add.ylab=FALSE)
{
  YLB=''
  if(add.ylab) YLB=UNITS
  p=d%>%
    mutate(Commodity=factor(Commodity,levels=names(KLS)))%>%
    filter(Units==UNITS)%>%
    ggplot(aes(x=Group,y=Quantity,fill=Commodity))+
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=KLS)+
    facet_wrap(~Type,nrow=NROW,scales='free')+
    theme_PA(leg.siz=Leg.siz,axs.t.siz=Axs.t.siz,axs.T.siz=Axs.T.siz)+
    theme(plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = 'top',
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA), 
          plot.background = element_rect(fill = "transparent", colour = NA))+
    guides(fill = guide_legend(nrow = nrow.leg, byrow = TRUE))+ylab(YLB)+xlab('')
  if(!is.null(TITLE))p=p+labs(title = TITLE)
  if(!is.null(KPTN))p=p+labs(caption = KPTN)
  return(p)
  
}

#2.1 Annual production, exports and imports
if(use.this)
{
  #wild caught fisheries and aquaculture production
  a=fun1(d=Production_table2,
         Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))%>%
    filter(!Group=="Other")
  
  Production=a%>%
    group_by(Year,Units,Group)%>%
    summarise(Quantity=sum(Quantity,na.rm=T))%>%
    ungroup()%>%
    mutate(Type='Production')
  
  
  Production_breakdown=a%>%
    group_by(Units,Group,Commodity)%>%
    summarise(Quantity=sum(Quantity,na.rm=T))%>%
    ungroup()%>%
    mutate(Type='Production',
           Commodity=case_when(Commodity=='other.finfish..sharks.and.rays'~'other finfish, sharks & rays',
                               Commodity=='rock.lobsters'~'rock lobsters',
                               Commodity=='other.crustaceans'~'other crustaceans',
                               Commodity=='other.molluscs'~'other molluscs',
                               TRUE~Commodity),
           Commodity=capitalize(Commodity))
  
  #wild caught fisheries only production
  do.this=FALSE
  if(do.this)
  {
    a=fun1(d=Production_table3,
           Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))%>%
      mutate(Commodity=gsub("\\.(?=[a-zA-Z])", " ", Commodity, perl = TRUE),
             Commodity=case_when(Commodity=="sharks.. rays"~"sharks & rays",
                                 TRUE~Commodity),
             Commodity=capitalize(Commodity))
    KLs=c('skyblue1','chocolate3','cornsilk3','limegreen')
    names(KLs)=levels(a$Group)
    Production_wild.caught.only=fun2(d=a%>%
                                       filter(!Group=="Other nei")%>%
                                       group_by(Year,Units,Group)%>%
                                       summarise(Quantity=sum(Quantity,na.rm=T))%>%
                                       ungroup(),
                                     TITLE='Wild-caught fisheries production',
                                     KPTN='source: ABARES',
                                     KLS=KLs,
                                     d.inset=a%>%filter(!Group=="Other nei"),
                                     UniT="Volume (1000s tonnes)",
                                     NRW=1,
                                     thresld=0.9,
                                     add.inset=FALSE,
                                     inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                                     fill.inset="grey90",
                                     inset.txt.size=3.5,
                                     inset.txt.loc=.85,
                                     Min.other.crus.mol=100)
    
    Production_wild.caught.only_value=fun2(d=a%>%
                                             filter(!Group=="Other nei")%>%
                                             group_by(Year,Units,Group)%>%
                                             summarise(Quantity=sum(Quantity,na.rm=T))%>%
                                             ungroup(),
                                           TITLE='Wild-caught fisheries production',
                                           KPTN='source: ABARES',
                                           KLS=KLs,
                                           d.inset=a%>%filter(!Group=="Other nei"),
                                           UniT="Value (M AUD)",
                                           NRW=1,
                                           thresld=0.9,
                                           add.inset=FALSE,
                                           inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                                           fill.inset="grey90",
                                           inset.txt.size=3.5,
                                           inset.txt.loc=.85,
                                           Min.other.crus.mol=500)
    
  }
  
  #exports by commodity  
  a=fun1(d=Production_table13,
         Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))%>%
    filter(!Group=="Other")%>%
    mutate(Commodity=gsub("\\.(?=[a-zA-Z])", " ", Commodity, perl = TRUE),
           Commodity=case_when(Commodity=="other finfish. sharks and rays"~"other finfish, sharks & rays",
                               Commodity=="other crustaceans and molluscs"~"other crustaceans & molluscs",
                               TRUE~Commodity),
           Commodity=capitalize(Commodity))
  Exports_breakdown=a%>%
    group_by(Units,Group,Commodity)%>%
    summarise(Quantity=sum(Quantity,na.rm=T))%>%
    ungroup()%>%
    mutate(Type='Exports',
           Commodity=capitalize(Commodity))
  
  if(do.this)
  {
    KLs=c('skyblue1','chocolate3','cornsilk3','limegreen')
    names(KLs)=levels(a$Group)
    Exports_commodity=fun2(d=a%>%
                             group_by(Year,Units,Group)%>%
                             summarise(Quantity=sum(Quantity,na.rm=T))%>%
                             ungroup(),
                           TITLE='Edible exports by commodity',
                           KPTN='source: ABARES',
                           KLS=KLs,
                           d.inset=a,
                           UniT="Volume (1000s tonnes)",
                           NRW=1,
                           thresld=0.9,
                           add.inset=FALSE,
                           inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                           fill.inset="grey90",
                           inset.txt.size=3.5,
                           inset.txt.loc=.85,
                           Min.other.crus.mol=100)
    
    Exports_commodity_value=fun2(d=a%>%
                                   group_by(Year,Units,Group)%>%
                                   summarise(Quantity=sum(Quantity,na.rm=T))%>%
                                   ungroup(),
                                 TITLE='Edible exports by commodity',
                                 KPTN='source: ABARES',
                                 KLS=KLs,
                                 d.inset=a,
                                 UniT="Value (M AUD)",
                                 NRW=1,
                                 thresld=0.9,
                                 add.inset=FALSE,
                                 inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                                 fill.inset="grey90",
                                 inset.txt.size=3.5,
                                 inset.txt.loc=.85,
                                 Min.other.crus.mol=100)
    
  }
  
  #exports by country 
  if(do.this)
  {
    a=fun1.country(d=Production_table14,
                   Grups=c("Americas","Asia","Europe","Oceania","Other"))%>%
      mutate(Commodity=gsub("\\.(?=[a-zA-Z])", " ", Commodity, perl = TRUE))
    KLs=c('slateblue4','sienna','wheat2','olivedrab','red2')
    names(KLs)=levels(a$Group)
    Exports_country=fun2(d=a%>%
                           group_by(Year,Units,Group)%>%
                           summarise(Quantity=sum(Quantity,na.rm=T))%>%
                           ungroup(),
                         TITLE='Edible exports by continent',
                         KPTN='source: ABARES',
                         KLS=KLs,
                         d.inset=a,
                         UniT="Volume (1000s tonnes)",
                         NRW=1,
                         thresld=0.9,
                         add.inset=FALSE,
                         inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                         fill.inset="grey90",
                         inset.txt.size=3.5,
                         inset.txt.loc=.85,
                         Min.other.crus.mol=100)
    
    Exports_country_value=fun2(d=a%>%
                                 group_by(Year,Units,Group)%>%
                                 summarise(Quantity=sum(Quantity,na.rm=T))%>%
                                 ungroup(),
                               TITLE='Edible exports by continent',
                               KPTN='source: ABARES',
                               KLS=KLs,
                               d.inset=a,
                               UniT="Value (M AUD)",
                               NRW=1,
                               thresld=0.9,
                               add.inset=FALSE,
                               inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                               fill.inset="grey90",
                               inset.txt.size=3.5,
                               inset.txt.loc=.85,
                               Min.other.crus.mol=100)
    
  }
  
  #imports by commodity  
  a=fun1(d=Production_table15,
         Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))%>%
    filter(!Group=="Other")%>%
    mutate(Commodity=gsub("\\.(?=[a-zA-Z])", " ", Commodity, perl = TRUE),
           Commodity=case_when(Commodity=="other finfish. sharks and rays"~"other finfish, sharks & rays",
                               Commodity=="other finfish, sharks & rays"~"other finfish",
                               Commodity=="other crustaceans and molluscs"~"other crustaceans & molluscs",
                               TRUE~Commodity),
           Commodity=capitalize(Commodity))
  Imports_breakdown=a%>%
    group_by(Units,Group,Commodity)%>%
    summarise(Quantity=sum(Quantity,na.rm=T))%>%
    ungroup()%>%
    mutate(Type='Imports',
           Commodity=capitalize(Commodity))
  if(do.this)
  {
    KLs=c('skyblue1','chocolate3','cornsilk3','limegreen')
    names(KLs)=levels(a$Group)
    
    Imports_commodity=fun2(d=a%>%
                             group_by(Year,Units,Group)%>%
                             summarise(Quantity=sum(Quantity,na.rm=T))%>%
                             ungroup(),
                           TITLE='Edible imports by commodity',
                           KPTN='source: ABARES',
                           KLS=KLs,
                           d.inset=a,
                           UniT="Volume (1000s tonnes)",
                           NRW=1,
                           thresld=0.9,
                           add.inset=FALSE,
                           inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                           fill.inset="grey90",
                           inset.txt.size=3.5,
                           inset.txt.loc=.85,
                           Min.other.crus.mol=100)
    
    Imports_commodity_value=fun2(d=a%>%
                                   group_by(Year,Units,Group)%>%
                                   summarise(Quantity=sum(Quantity,na.rm=T))%>%
                                   ungroup(),
                                 TITLE='Edible imports by commodity',
                                 KPTN='source: ABARES',
                                 KLS=KLs,
                                 d.inset=a,
                                 UniT="Value (M AUD)",
                                 NRW=1,
                                 thresld=0.9,
                                 add.inset=FALSE,
                                 inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                                 fill.inset="grey90",
                                 inset.txt.size=3.5,
                                 inset.txt.loc=.85,
                                 Min.other.crus.mol=100)
    
  }
  
  #imports by country
  if(do.this)
  {
    a=fun1.country(d=Production_table16,
                   Grups=c("Americas","Asia","Europe","Oceania","Africa","Other"))%>%
      mutate(Commodity=gsub("\\.(?=[a-zA-Z])", " ", Commodity, perl = TRUE))
    KLs=c('slateblue4','sienna','wheat2','olivedrab','lightblue3','red2')
    names(KLs)=levels(a$Group)
    Imports_country=fun2(d=a%>%
                           group_by(Year,Units,Group)%>%
                           summarise(Quantity=sum(Quantity,na.rm=T))%>%
                           ungroup(),
                         TITLE='Edible imports by continent',
                         KPTN='source: ABARES',
                         KLS=KLs,
                         d.inset=a,
                         UniT="Volume (1000s tonnes)",
                         NRW=1,
                         thresld=0.9,
                         add.inset=FALSE,
                         inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                         fill.inset="grey90",
                         inset.txt.size=3.5,
                         inset.txt.loc=.85,
                         Min.other.crus.mol=100)
    
    Imports_country_value=fun2(d=a%>%
                                 group_by(Year,Units,Group)%>%
                                 summarise(Quantity=sum(Quantity,na.rm=T))%>%
                                 ungroup(),
                               TITLE='Edible imports by continent',
                               KPTN='source: ABARES',
                               KLS=KLs,
                               d.inset=a,
                               UniT="Value (M AUD)",
                               NRW=1,
                               thresld=0.9,
                               add.inset=FALSE,
                               inset.loc=data.frame(x = .565, y = .2, x.w = .5, x.h = .5),
                               fill.inset="grey90",
                               inset.txt.size=3.5,
                               inset.txt.loc=.85,
                               Min.other.crus.mol=100)
    
  }
  
  #plot together
  d=rbind(Production,Exports,Imports)%>%
    mutate(Group=as.character(Group),
           Group=case_when(Group=='Rock lobsters & Prawns'~'Rock lobsters\n& Prawns',
                           Group=='Abalone & Scallops'~'Abalone\n & Scallops',
                           Group=='Other crustaceans & molluscs'~'Other crustaceans\n & molluscs',
                           TRUE~Group),
           Group=factor(Group,levels=c("Finfish","Rock lobsters\n& Prawns",
                                       "Abalone\n & Scallops","Other crustaceans\n & molluscs")),
           Type=factor(Type,levels=c('Production','Exports','Imports')))%>%
    filter(Year>=MIN.yr)
  KLs=c('skyblue1','chocolate3','cornsilk3','limegreen')
  names(KLs)=levels(d$Group)
  Production.exports.imports=fun3(d=d,
                                  UNITS="Volume (1000s tonnes)",
                                  KLS=KLs,
                                  LINTYP=c("Production" = "twodash", "Exports" = "dotted", "Imports" = "solid"),
                                  LINWID=c("Production" = 2, "Exports" = 1.5, "Imports" = .8),
                                  KPTN=c('Production= Fisheries and aquaculture production\nExports=Edible exports\nImports=Edible imports\n(source: ABARES, australian-fisheries-and-aquaculture-statistics-2022)'),
                                  Axs.t.siz=12, Axs.T.siz=16, Leg.siz=11)
  
  Production.exports.imports_value=fun3(d=d,
                                        UNITS="Value (M AUD)",
                                        KLS=KLs,
                                        LINTYP=c("Production" = "twodash", "Exports" = "dotted", "Imports" = "solid"),
                                        LINWID=c("Production" = 2, "Exports" = 1.5, "Imports" = .8),
                                        KPTN=c('Production= Fisheries and aquaculture production\nExports=Edible exports\nImports=Edible imports\n(source: ABARES, australian-fisheries-and-aquaculture-statistics-2022)'),
                                        Axs.t.siz=12, Axs.T.siz=16, Leg.siz=11)
  
  d=rbind(Production_breakdown,Exports_breakdown,Imports_breakdown)%>%
    mutate(Commodity=case_when(Commodity%in%c("Other finfish, sharks & rays",
                                              "Sharks and rays")~"Other finfish,\nsharks & rays",
                               Commodity=="Ornamental fish"~"Ornamental\nfish",
                               Commodity%in%c("Other crustaceans","Other molluscs",
                                              "Other crustaceans & molluscs")~"Other crustaceans\n& molluscs",
                               Commodity=="Rock lobsters"~"Rock\nlobsters",
                               Commodity%in%c("Squids","Squid and octopus")~"Squid & octopus",
                               TRUE~Commodity),
           Group=as.character(Group),
           Group=case_when(Group=='Rock lobsters & Prawns'~'Rock lobsters\n& Prawns',
                           Group=='Abalone & Scallops'~'Abalone\n & Scallops',
                           Group=='Other crustaceans & molluscs'~'Other crustaceans\n & molluscs',
                           TRUE~Group),
           Group=factor(Group,levels=c("Finfish","Rock lobsters\n& Prawns",
                                       "Abalone\n & Scallops","Other crustaceans\n & molluscs")),
           Type=factor(Type,levels=c('Production','Exports','Imports')))%>%
    group_by(Units,Group,Commodity,Type)%>%
    summarise(Quantity=sum(Quantity,na.rm=T))%>%
    ungroup()
  Kls.com=c("Hakes"="skyblue","Herrings"="turquoise","Ornamental\nfish"="steelblue",
            "Salmonids"="royalblue3","Swordfish"="slategray1",
            "Toothfish"="navyblue","Tunas"="slateblue","Other finfish,\nsharks & rays"="skyblue4",
            "Prawns"="sienna1","Rock\nlobsters"="chocolate4",
            "Abalone"="cornsilk3","Scallops"="cornsilk1",
            "Crabs"="limegreen","Mussels"="lawngreen","Oysters"="lightgreen",
            "Squid & octopus"="forestgreen","Other crustaceans\n& molluscs"="olivedrab")
  Production.exports.imports_breakdown=fun4(d,
                                            UNITS="Volume (1000s tonnes)",
                                            KPTN=NULL,
                                            Axs.t.siz=10, Axs.T.siz=10, Leg.siz=10,
                                            NROW=1,
                                            KLS=Kls.com,
                                            nrow.leg=3)
  Production.exports.imports_breakdown_value=fun4(d,
                                                  UNITS="Value (M AUD)",
                                                  KPTN=NULL,
                                                  Axs.t.siz=10, Axs.T.siz=10, Leg.siz=10,
                                                  NROW=1,
                                                  KLS=Kls.com,
                                                  nrow.leg=3)
  
}


#2.2 Commodity imports and exports by Country and year  

  #Map of Australia
Australia <- ne_states(country = "Australia", returnclass = "sf")
Limy=c(-42,-12)
Limx=c(100,160)
p_Map=ggplot(data = Australia) +
  geom_sf(color = "grey60", fill = "grey95",alpha=.5) +
  xlab("") + ylab("")+
  coord_sf(xlim =Limx , ylim = Limy, expand = T)+
  theme_void()

  #Plotting functions
fn.barplt=function(d,show.LGN=FALSE,Y.lbl='',axs.size=11,lg.size=14,yMX,custom_colors,LBL.size=4)
{
  if(is.null(yMX)) yMX=sum(d$Var)
  p=d%>%
    ggplot(aes(x=1,y=.data[['Var']],fill=.data[['Country']]))+
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(drop = FALSE,values = custom_colors,breaks = levels(d$Country))+
    theme_PA(axs.t.siz=axs.size,leg.siz=lg.size)+xlab('')+ylab(Y.lbl)+ylim(0,yMX)+
    theme(axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          legend.position = 'top',
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA), 
          plot.background = element_rect(fill = "transparent", colour = NA))
  #Legend<<-g_legend(p)
  if(!show.LGN) p=p+theme(legend.position = 'none')
  
  p=p+
    geom_text_repel(aes(label = Country),
                    position = position_stack(vjust = 0.5),
                    max.overlaps= getOption("ggrepel.max.overlaps", default = 20),
                    size = LBL.size, 
                    xlim = c(1e3, NA),
                    color = "black", 
                    direction = "y", 
                    segment.linetype = "dotted",
                    segment.size = .7,
                    segment.alpha = .5,
                    min.segment.length = 2, 
                    segment.curvature = -0.1,
                    segment.ncp = 3,
                    segment.angle = 20,
                    box.padding = .3)+ 
    coord_cartesian(clip = "off",ylim = c(0,yMX))+
    theme_stacked_bar()+
    theme(axis.text = element_text(size = axs.size),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          plot.margin = unit(c(0.2,4,0,0), "cm"))+ # ("left", "right", "bottom", "top")
    scale_y_continuous(labels = comma) 
  
  return(p)
}
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 
theme_stacked_bar <- function( ) {
  
  theme(
    legend.position = "none",
    plot.margin = unit(c(2,7,2,2),"cm"),
    axis.text = element_text(size=3)
  )
}
fn.barplt2=function(dd,Ylab,LGN.titl,LGN.pos=c(0.7, 0.1),Axs.t.siz=12,Leg.siz=12,bckf.fil="grey99",PROP=0.95,NRW.leg=1)
{
  dd1=dd%>%
    group_by(Commodity)%>%
    summarise(Var=sum(Var,na.rm=T))%>%
    ungroup()%>%
    arrange(-Var)%>%
    mutate(CumSum=cumsum(Var),
           Prop=CumSum/sum(Var),
           Commodity1=ifelse(Prop<=PROP,Commodity,'Other'))
  
  dd=dd%>%left_join(dd1%>%dplyr::select(Commodity,Commodity1),by='Commodity')%>%
    dplyr::select(-Commodity)%>%rename(Commodity=Commodity1)%>%
    group_by(Commodity,TradeFlow)%>%
    summarise(Var=sum(Var))
  
  p=dd%>%
    mutate(TradeFlow=factor(TradeFlow,levels=c('Imports','Exports')))%>%
    ggplot(aes(Commodity,Var,fill=TradeFlow))+
    geom_bar(stat = 'identity',show.legend = FALSE)+
    coord_flip()+
    theme_PA(axs.t.siz=Axs.t.siz,leg.siz=Leg.siz)+ylab(Ylab)+xlab('')+ scale_y_continuous(labels = comma)+
    theme(legend.position = LGN.pos,
          legend.key = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = bckf.fil, colour = NA), 
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_blank())
  if(any(!LGN.pos=='none'))
  {
    p=p+geom_point(aes(y = 0, color = TradeFlow), size = 0, shape = 15) +
      guides(fill = guide_legend(nrow=NRW.leg,override.aes = list(size = 3)))
  }
  
  if(is.null(LGN.titl)) p=p+theme(legend.title = element_blank())
  if(!is.null(LGN.titl)) p=p+guides(fill=guide_legend(title=LGN.titl))
  
  return(p)
}
fn.barplt.plain=function(d,Y.lbl,axs.size=13,lg.size=14)
{
  d=d%>%mutate(Country=as.character(Country))
  LVLs=d%>%arrange(-Var)%>%pull(Country)
  d=d%>%
    mutate(Country=factor(Country,levels=rev(LVLs)))
  p=d%>%
    ggplot(aes(x=.data[['Country']],y=.data[['Var']]))+
    geom_bar(stat="identity",fill='brown4')+coord_flip()+
    theme_PA(axs.t.siz=axs.size,leg.siz=lg.size)+xlab('')+ylab(Y.lbl)+
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA), 
          plot.background = element_rect(fill = "transparent", colour = NA))
  if(Y.lbl=='1000s Tonnes') p=p+scale_y_continuous(labels = comma) 
  
  return(p)
}

  #Get trade data
Non.comsumption=c('Ornamental finfish')
dummy_group=ABARES_trade_data%>%
  filter(!Group1%in%Non.comsumption)%>%
  group_by(Group2,Calendar_year,Country,Continent,TradeFlow)%>%
  summarise(Value_millions=sum(Value_millions,na.rm=T),Quantity_tonnes=sum(Quantity_tonnes,na.rm=T))%>%
  ungroup()%>%
  rename(Group=Group2,
         Year=Calendar_year)
dummy_group.commodity=ABARES_trade_data%>%
  filter(!Group1%in%Non.comsumption)%>%
  group_by(Group1,Group2,Calendar_year,Country,Continent,TradeFlow)%>%
  summarise(Value_millions=sum(Value_millions,na.rm=T),Quantity_tonnes=sum(Quantity_tonnes,na.rm=T))%>%
  ungroup()%>%
  rename(Commodity=Group1,
         Group=Group2,
         Year=Calendar_year)
Explain.prop=0.8
Top.countries=dummy_group%>%
                filter(Year<=2011)%>%   #some groups under reported past this year in ABARES_trade_data
                group_by(Country)%>%
                summarise(Val=sum(Quantity_tonnes,na.rm=T))%>%
                ungroup()%>%
                arrange(-Val)%>%
                mutate(CumSum=cumsum(Val),
                       Prop=CumSum/sum(Val),
                       Country1=ifelse((Prop - Explain.prop)<=0,Country,'Other'),
                       Country1=case_when(Country1=='Korea, Republic of (South)'~'South Korea',
                                          TRUE~Country1))
Top.countries.value=dummy_group%>%
                filter(Year<=2011)%>%   
                group_by(Country)%>%
                summarise(Val=sum(Value_millions,na.rm=T))%>%
                ungroup()%>%
                arrange(-Val)%>%
                mutate(CumSum=cumsum(Val),
                       Prop=CumSum/sum(Val),
                       Country1=ifelse((Prop - Explain.prop)<=0,Country,'Other'),
                       Country1=case_when(Country1=='Korea, Republic of (South)'~'South Korea',
                                          TRUE~Country1))

d1=dummy_group%>%
  left_join(Top.countries%>%
              dplyr::select(Country,Country1),by='Country')
a=d1%>%filter(is.na(Country1))
d1=d1%>%
  mutate(Country1=ifelse(is.na(Country1) & Country%in%a$Country,'Other',Country1))%>%
  dplyr::select(-Country)%>%rename(Country=Country1)%>%
  mutate(Country=factor(Country,levels=unique(Top.countries$Country1)))%>%
  group_by(Country,TradeFlow,Year)%>%
  summarise(Quantity_tonnes=sum(Quantity_tonnes,na.rm=T),Value_millions=sum(Value_millions,na.rm=T))%>%
  ungroup()

d1_commodity=dummy_group.commodity%>%
                  group_by(Commodity,TradeFlow,Year)%>%
                  summarise(Quantity_tonnes=sum(Quantity_tonnes,na.rm=T),Value_millions=sum(Value_millions,na.rm=T))%>%
                  ungroup()%>%
                  mutate(Commodity=case_when(Commodity=="Tilapias, catfish, Nile perch & Carps"~"Tilapias, catfish,\nNile perch & carps",
                                          #   Commodity=="Anchovies & sardines"~"Anchovies &\nsardines",
                                          #   Commodity=="Salmons & trouts"~"Salmons &\ntrouts",
                                             Commodity=="Dried fish; fish oil, livers & extracts"~"Dried fish, fish oil,\nlivers & extracts",
                                             TRUE~Commodity))%>%
                  ungroup()

Kol.Risk.Other='grey80'
KLS.country=Performance.indices%>%filter(Country%in%unique(Top.countries$Country1))
names.KLS.country=KLS.country$Country
KLS.country=KLS.country$Color
names(KLS.country)=names.KLS.country
KLS.country=c(KLS.country,c("Other"=Kol.Risk.Other))

  #Create plots by year for tonnage
Yr.list=c(MIN.yr,2021) #post 2021 seems incomplete for lobster, prawns, abalone. Only fixed the 2021 year (see issues in 'Compare sources_exports.jpg')
do.this=FALSE
if(do.this) #compare the two  sources
{
  rbind(d1_commodity%>%group_by(TradeFlow,Year)%>%summarise(Tons=sum(Quantity_tonnes))%>%mutate(source='ABARES'),
        rbind(Exports%>%
                group_by(Year)%>%summarise(Tons=sum(Quantity*1000))%>%mutate(TradeFlow='Exports',source='AustFishAquacStats_2022_Tables')%>%
                dplyr::select(TradeFlow,Year,Tons,source),
              Imports%>%
                group_by(Year)%>%summarise(Tons=sum(Quantity*1000))%>%mutate(TradeFlow='Imports',source='AustFishAquacStats_2022_Tables')%>%
                dplyr::select(TradeFlow,Year,Tons,source)))%>%
    ggplot(aes(Year,Tons,color=source))+geom_line()+facet_wrap(~TradeFlow,scales='free')+
    theme(legend.position = 'top')+ylim(0,NA)
  ggsave(paste0(hndl.out,"Compare sources.jpg"),width = 10,height = 6) 
  
  
  rbind(d1_commodity%>%filter(TradeFlow=='Exports')%>%mutate(Commodity=ifelse(!Commodity%in%c("Prawns","Abalone","Lobsters"),'Other',Commodity))%>%group_by(Commodity,Year)%>%
          summarise(Tons=sum(Quantity_tonnes))%>%mutate(source='ABARES'),
        Exports%>%
          mutate(Commodity=ifelse(Commodity=="Rock lobsters","Lobsters",Commodity),
                 Commodity=ifelse(!Commodity%in%c("Prawns","Abalone","Lobsters"),'Other',Commodity))%>%
          group_by(Commodity,Year)%>%summarise(Tons=sum(Quantity*1000))%>%mutate(TradeFlow='Exports',source='AustFishAquacStats_2022_Tables')%>%
          dplyr::select(Commodity,Year,Tons,source))%>%
    ggplot(aes(Year,Tons,color=source))+geom_line()+facet_wrap(~Commodity,scales='free')+
    theme(legend.position = 'top')+ylim(0,NA)
  ggsave(paste0(hndl.out,"Compare sources_exports.jpg"),width = 10,height = 6)
  
}
# Fix d1_commodity 2021 bu adding lobster and abalone  #ACA
d1_commodity=d1_commodity%>%
  filter(!(Commodity%in%c("Abalone","Lobsters") & TradeFlow=="Exports" & Year==Yr.list[2]))
d1_commodity=rbind(d1_commodity,
                   Exports%>%
                     filter(Units=='Volume (1000s tonnes)'&Commodity%in%c('Abalone',"Rock lobsters"))%>%
                     group_by(Commodity,Year)%>%
                     summarise(Quantity_tonnes=sum(Quantity*1000,na.rm=T))%>%
                     ungroup()%>%
                     mutate(Value_millions=NA,
                            TradeFlow='Exports',
                            Commodity=ifelse(Commodity=='Rock lobsters',"Lobsters",Commodity))%>%
                     dplyr::select(Commodity, TradeFlow, Year, Quantity_tonnes, Value_millions)%>%
                     filter(Year==Yr.list[2]))

p.yr.list=vector('list',length=length(Yr.list))
names(p.yr.list)=Yr.list

  #Risk legend
p.risk=RiskColors%>%ggplot(aes(x=1,y=1:nrow(RiskColors),fill=Risk))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(drop = FALSE,values =RiskColors$Color , breaks = RiskColors$Risk)+
  theme_PA()+theme(legend.position = 'top',legend.title = element_blank())
Legend<-g_legend(p.risk)  

    #map & countries
what.show='plain'
for(i in 1:length(p.yr.list))
{
  dd=d1%>%
    filter(Year==Yr.list[i] & TradeFlow=='Imports')%>%
    rename(Var=Quantity_tonnes)%>%
    mutate(Var=Var/1000)  #1000s of tonnes
  imp.flow=sum(dd$Var)
  imports=fn.barplt(d=dd%>%mutate(Var=Var/sum(Var,na.rm=T)),show.LGN=FALSE,Y.lbl='Proportion of imports',yMX=NULL,custom_colors=KLS.country,LBL.size=3)
  
  dd=d1%>%
    filter(Year==Yr.list[i] & TradeFlow=='Exports')%>%
    rename(Var=Quantity_tonnes)%>%
    mutate(Var=Var/1000)
  exp.flow=sum(dd$Var)
  if(what.show=='risk')exports=fn.barplt(d=dd%>%mutate(Var=Var/sum(Var,na.rm=T)),show.LGN=FALSE,Y.lbl='',yMX=NULL,custom_colors=KLS.country,LBL.size=3.5)
  if(what.show=='plain') exports=fn.barplt.plain(d=dd%>%mutate(Var=Var/sum(Var,na.rm=T)),Y.lbl="Proportion of exports",axs.size=11)
  p.yr.list[[i]]=list(imports=imports,exports=exports,imp.flow=imp.flow,exp.flow=exp.flow)
}

  #commodities
p.trade.comm=p.yr.list
for(i in 1:length(p.yr.list))
{
  LGn.pos=c(0.75,0.15)
  if(i==1)  LGn.pos = 'none'
  
  p.trade.comm[[i]]=fn.barplt2(dd=d1_commodity%>%
                                     filter(Year==Yr.list[i])%>%
                                     rename(Var=Quantity_tonnes)%>%
                                     mutate(Var=Var/1000),
                               Ylab="1000s Tonnes",
                               LGN.titl=NULL, #Yr.list[i]
                               LGN.pos=LGn.pos,
                               Axs.t.siz=8.5,Leg.siz=12,
                               bckf.fil="transparent",
                               NRW.leg=2) 
  
}

  #combine  & countries with commodities
p.trade=p.yr.list
MX.flow=max(c(with(p.yr.list[[1]],c(imp.flow,exp.flow)),with(p.yr.list[[2]],c(imp.flow,exp.flow))))
for(i in 1:length(p.yr.list))
{
  Arrow.dat=data.frame(Trade=c('Imports','Exports'),
                       x=c(98,150),
                       y=c(-18,-25),
                       x.end=c(115,160),
                       y.end=c(-25,-18),
                       Flow=with(p.yr.list[[i]],c(5*imp.flow/MX.flow,5*exp.flow/MX.flow)))
  
  p.trade[[i]]=ggdraw() +
    draw_plot(p_Map+
                labs(title=names(p.yr.list)[i])+theme(plot.title = element_text(hjust=0.5,size=30))+
                geom_curve(data=Arrow.dat,
                           aes(x = x, y = y, xend = x.end, yend = y.end),size = Arrow.dat$Flow,
                           arrow = arrow(length = unit(0.8, "cm"), type = "open"),
                           color = "cadetblue",alpha=1, curvature = 0.2,show.legend = FALSE))+
    draw_plot(p.yr.list[[i]]$imports, x = 0.01, y = 0, width = .375, height = 1)+
    draw_plot(p.yr.list[[i]]$exports, x = .65, y = 0, width = .35, height = 1)
    #draw_plot(Legend, x = 0.4, y = .8, width = .2, height = .1)+
    #draw_plot(p.trade.comm[[i]], x = .3, y = .0, width = .3, height = .75)+
    
}

#Annual Proportion of imports by index
p_ann.imp.indices=d1%>%
  filter(TradeFlow=='Imports')%>%
  left_join(Performance.indices%>%
              filter(Country%in%unique(d1$Country))%>%
              dplyr::select(Country,Risk,Color),by='Country')%>%
  mutate(Risk=ifelse(is.na(Risk),'Other',Risk),
         Risk=factor(Risk,levels=c("Negligible","Low","Medium","High","Severe","Other")),
         Color=ifelse(is.na(Color) & Country=='Other','grey80',Color))%>%
  group_by(Year,Risk,Color)%>%
  summarise(Var=sum(Quantity_tonnes,na.rm=T))%>%
  ggplot(aes(x=Year,y=Var,fill=Risk))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(drop = FALSE,values = c(RiskColors$Color,Kol.Risk.Other),breaks = c(RiskColors$Risk,"Other"))+
  theme_PA(axs.t.siz=10,leg.siz=10)+ylab('Proportion of imports')+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))+
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))+
  labs(caption='Commodities for human consumption only')



#Create infographic
plot_grid(p.trade[[2]], 
          plot_grid(p_ann.imp.indices,p.trade.comm[[2]],rel_widths = c(1.4, 1)),
          nrow=2,ncol=1)
ggsave(paste0(hndl.out,"Infographic_Map_trade flow.jpg"),width = 10,height = 6) 









  
  
  
  
# Infographic - Shark production, exports and imports --------------------------------------------------------------------

  
# Infographic - Regulations --------------------------------------------------------------------
  