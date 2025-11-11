library(tidyverse)
library(cowplot)
library(readxl)
library(data.table)
library(Hmisc)
library(countrycode)
library(rnaturalearth)

if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
hndl.out=handl_OneDrive("Scientific manuscripts/Perspective_Double standards/6. Outputs/")
source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/ggplot.themes.R'))

# Infographic - Production, exports and imports --------------------------------------------------------------------

#1. Bring in data
  #1.1 source https://www.agriculture.gov.au/abares/research-topics/fisheries/fisheries-data#australian-fisheries-and-aquaculture-statistics-2022
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
  #imports by commodity
Production_table15=read_excel(paste0(hndl.in,'AustFishAquacStats_2022_Tables_v1.0.0.xlsx'), sheet = "Table 15",skip = 7)%>%
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
                             TRUE~Commodity))
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



  #1.2. source https://www.agriculture.gov.au/abares/research-topics/trade/dashboard
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
                           TRUE~Overseas_location))

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
                                          
                                          TradeCode%in%c(305320042,305310040,305310041,3053101,16030012,304100002,304100036,
                                                         305300033,3053901,305390049,305390050,305300017,305100012,3049090,3037912,3037990,
                                                         16042011,302690026,303790023,303790002,305690030,3056950,305690089,3056951,305690090,
                                                         302290013,3044901,304490080,304490079,304440073,304430072,304330062,
                                                         3028901,3026909,302690042,302890050,304100042,3041909,304190058,302990003,
                                                         3045950,304590089,3045951,304590090,302590029,3025901,302590030,302490001,302290026,
                                                         3022902,3022901,302290019,302290020,302390040,304530082,3045101,304510080,304510079,
                                                         3048901,304890040,304890039,304790029,304830033,304900042,3038950,303890079,3038951,
                                                         3037919,303790055,303890080,304200022,304200021,304200045,304200044,3042909,
                                                         304290092,304290091,3042002,304200007,304200039,304200006,304200038,3039910,
                                                         303990092,3049091,304900011,304900041,3049919,304990079,304990072,3049901,304990080,
                                                         303590090,3036901,303690069,303690070,304950073,304930071,304930074,3049301,
                                                         301990029,301990006,3019950,301990009,3019910,3019909,301990035,3019901,3019902,
                                                         3019903,301990010,1604200060,1604200070,1604200066,1604200039,1604190032,1604190031,
                                                         16042014,1604200043,1604200042,16042090,16042019,16041990,16041910,1604190034,16041920,
                                                         1604190030,1604190035,16041921,1604190033,305490062,305490061,3054951,3054901,3054950,
                                                         305490023,305490022,305490064,305490063,305640084,305640085,30289004)~'Mixed finfish',
                                        
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
                                         TradeCode%in%c(302730033,3027301,304390069,3043901,304390068,303250052,3032501,303250053,
                                                        304690019,3046901,304690018,3019301,3019302,3019303,3019304,301930007)~'Carps',
                                         TradeCode%in%c(3027201,302720032,302720033,304320061,304310060,302710031,
                                                        302710032,3032401,303240052,303240051,304620012,304610011,
                                                        3032301,303230051,303230050,3054441,305440055,305440054)~'Tilapias & catfish',
                                         TradeCode%in%c(3027901,302790039,302790040,304630013,3032911,303290060,3032910,303290059)~'Nile perch',
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
                                        grepl(paste(c('ornamental','Live Australian species of syngnathids'),collapse='|'),Group)~'Ornamental fish',
                                        TradeCode%in%c(3011020)~'Ornamental fish',
                                        
                                        TradeCode%in%c(302920002,303920091,1604180067,305710091)~'Shark fins',
                                        TradeCode%in%c(302650024,302810040,302810041,304560085,304470076,
                                                       3038101,303750019,303810070,303810071,304960075,304880038)~'Dogfish & other sharks',
                                        TradeCode%in%c(304480077,3028201,302820042,303820071,303820072,304970076)~'Rays & skates',
                                         
                                         TRUE~NA))




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

#2.1 Create each individual plot

  #wild caught fisheries and aquaculture production
a=fun1(d=Production_table2,
       Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))%>%
  filter(!Group=="Other")
Production=a%>%
              group_by(Year,Units,Group)%>%
              summarise(Quantity=sum(Quantity,na.rm=T))%>%
              ungroup()%>%
              ggplot(aes(Year,Quantity,color=Units))+
              geom_line(linewidth=1.5)+facet_wrap(~Group,nrow=1)+
              theme_minimal()+
              theme(legend.position = 'top')+
              labs(caption = 'Fisheries and aquaculture production (source:ABARES)')


  #wild caught fisheries only production
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

  #exports by commodity  
a=fun1(d=Production_table13,
       Grups=c('Finfish','Rock lobsters & Prawns','Abalone & Scallops','Other crustaceans & molluscs'))%>%
  filter(!Group=="Other")%>%
  mutate(Commodity=gsub("\\.(?=[a-zA-Z])", " ", Commodity, perl = TRUE),
         Commodity=case_when(Commodity=="other finfish. sharks and rays"~"other finfish, sharks & rays",
                             Commodity=="other crustaceans and molluscs"~"other crustaceans & molluscs",
                             TRUE~Commodity),
         Commodity=capitalize(Commodity))
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

  #exports by country  
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

  #imports by country  
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


#Commodity imports and exports by Country and year  #ACA
#Missing. Using dummy, for this I need to know TradeCode common name in ABARES_trade_data
dummy=expand.grid(Commodity=c('Finfish','Crustaceans','Molluscs'),
                  Country=c('Japan','United States','Thailand'),
                  Year=c(1989,2021),
                  Trade.flow=c('import','export'))%>%
  
  mutate(Value=case_when(Year==1989 & Commodity=='Finfish' & Trade.flow=='import' & Country=='Japan' ~10,
                         Year==1989 & Commodity=='Finfish' & Trade.flow=='import'& Country=='United States'~8,
                         Year==1989 & Commodity=='Finfish' & Trade.flow=='import'& Country=='Thailand'~1,
                         Year==1989 & Commodity=='Crustaceans' & Trade.flow=='import' & Country=='Japan' ~3,
                         Year==1989 & Commodity=='Crustaceans' & Trade.flow=='import'& Country=='United States'~3,
                         Year==1989 & Commodity=='Crustaceans' & Trade.flow=='import'& Country=='Thailand'~2,
                         Year==1989 & Commodity=='Molluscs' & Trade.flow=='import' & Country=='Japan' ~1,
                         Year==1989 & Commodity=='Molluscs' & Trade.flow=='import'& Country=='United States'~3,
                         Year==1989 & Commodity=='Molluscs' & Trade.flow=='import'& Country=='Thailand'~4,
                         
                         Year==1989 & Commodity=='Finfish' & Trade.flow=='export' & Country=='Japan' ~30,
                         Year==1989 & Commodity=='Finfish' & Trade.flow=='export'& Country=='United States'~15,
                         Year==1989 & Commodity=='Finfish' & Trade.flow=='export'& Country=='Thailand'~1,
                         Year==1989 & Commodity=='Crustaceans' & Trade.flow=='export' & Country=='Japan' ~100,
                         Year==1989 & Commodity=='Crustaceans' & Trade.flow=='export'& Country=='United States'~50,
                         Year==1989 & Commodity=='Crustaceans' & Trade.flow=='export'& Country=='Thailand'~20,
                         Year==1989 & Commodity=='Molluscs' & Trade.flow=='export' & Country=='Japan' ~5,
                         Year==1989 & Commodity=='Molluscs' & Trade.flow=='export'& Country=='United States'~1,
                         Year==1989 & Commodity=='Molluscs' & Trade.flow=='export'& Country=='Thailand'~1,
                         
                         Year==2021 & Commodity=='Finfish' & Trade.flow=='import' & Country=='Japan' ~7,
                         Year==2021 & Commodity=='Finfish' & Trade.flow=='import'& Country=='United States'~4,
                         Year==2021 & Commodity=='Finfish' & Trade.flow=='import'& Country=='Thailand'~100,
                         Year==2021 & Commodity=='Crustaceans' & Trade.flow=='import' & Country=='Japan' ~20,
                         Year==2021 & Commodity=='Crustaceans' & Trade.flow=='import'& Country=='United States'~10,
                         Year==2021 & Commodity=='Crustaceans' & Trade.flow=='import'& Country=='Thailand'~40,
                         Year==2021 & Commodity=='Molluscs' & Trade.flow=='import' & Country=='Japan' ~10,
                         Year==2021 & Commodity=='Molluscs' & Trade.flow=='import'& Country=='United States'~30,
                         Year==2021 & Commodity=='Molluscs' & Trade.flow=='import'& Country=='Thailand'~100,
                         
                         Year==2021 & Commodity=='Finfish' & Trade.flow=='export' & Country=='Japan' ~50,
                         Year==2021 & Commodity=='Finfish' & Trade.flow=='export'& Country=='United States'~50,
                         Year==2021 & Commodity=='Finfish' & Trade.flow=='export'& Country=='Thailand'~1,
                         Year==2021 & Commodity=='Crustaceans' & Trade.flow=='export' & Country=='Japan' ~200,
                         Year==2021 & Commodity=='Crustaceans' & Trade.flow=='export'& Country=='United States'~150,
                         Year==2021 & Commodity=='Crustaceans' & Trade.flow=='export'& Country=='Thailand'~40,
                         Year==2021 & Commodity=='Molluscs' & Trade.flow=='export' & Country=='Japan' ~80,
                         Year==2021 & Commodity=='Molluscs' & Trade.flow=='export'& Country=='United States'~10,
                         Year==2021 & Commodity=='Molluscs' & Trade.flow=='export'& Country=='Thailand'~1))
dummy$unit='Volume (1000s tonnes)'


Australia <- ne_states(country = "Australia", returnclass = "sf")
Limy=c(-42,-12)
Limx=c(105,165)
p_Map=ggplot(data = Australia) +
  geom_sf(color = "black", fill = "grey80") +
  xlab("") + ylab("")+
  coord_sf(xlim =Limx , ylim = Limy, expand = T)+
  theme_void()


fn.barplt=function(d,yr,trade,Unit,show.LGN=FALSE,Y.lbl='',axs.size=13,lg.size=14,yMX=max(dummy$Value))
{
  p=d%>%
    filter(Year==yr & Trade.flow==trade & unit==Unit)%>%
    ggplot(aes(x=Commodity,y=Value,fill=Country))+
    geom_bar(position="stack", stat="identity")+
    theme_PA(axs.t.siz=axs.size,leg.siz=lg.size)+xlab('')+ylab(Y.lbl)+
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA), 
          plot.background = element_rect(fill = "transparent", colour = NA))+
    ylim(0,yMX)
  if(!show.LGN)p=p+theme(legend.position = 'none')
  return(p)
}
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

#Original year
p1.imp=fn.barplt(d=dummy%>%filter(Commodity=='Finfish'),yr=1989,trade='import',
                 Unit='Volume (1000s tonnes)',show.LGN=TRUE)
legend <- g_legend(p1.imp)
p1.imp=p1.imp+theme(legend.position = 'none')
p2.imp=fn.barplt(d=dummy%>%filter(Commodity=='Crustaceans'),yr=1989,trade='import',
                 Unit='Volume (1000s tonnes)',Y.lbl='Volume (1000s tonnes)')
p3.imp=fn.barplt(d=dummy%>%filter(Commodity=='Molluscs'),yr=1989,trade='import',Unit='Volume (1000s tonnes)')
p1.exp=fn.barplt(d=dummy%>%filter(Commodity=='Finfish'),yr=1989,trade='export',Unit='Volume (1000s tonnes)')
p2.exp=fn.barplt(d=dummy%>%filter(Commodity=='Crustaceans'),yr=1989,trade='export',Unit='Volume (1000s tonnes)')
p3.exp=fn.barplt(d=dummy%>%filter(Commodity=='Molluscs'),yr=1989,trade='export',Unit='Volume (1000s tonnes)')
p.original=ggdraw() +
  draw_plot(p_Map+
              labs(title=1989)+theme(plot.title = element_text(hjust=0.5,vjust = 12.5,size=30))+
              geom_curve(data=data.frame(x=112,y=c(-15,-30,-39),
                                         x.end=130,y.end=c(-25,-26,-28)),
                         aes(x = x, y = y, xend = x.end, yend = y.end),size = 1.5,
                         arrow = arrow(length = unit(0.3, "cm"), type = "open"),
                         color = "brown4", curvature = 0.2,show.legend = FALSE)+
              geom_curve(data=data.frame(x=135,y=c(-25,-26,-28),
                                         x.end=158,y.end=c(-15,-30,-38)),
                         aes(x = x, y = y, xend = x.end, yend = y.end),size = 1.5,
                         arrow = arrow(length = unit(0.3, "cm"), type = "open"),
                         color = "brown4", curvature = 0.2,show.legend = FALSE))+
  draw_plot(legend, x = 0.4, y = .8, width = .2, height = .1)+
  draw_plot(p1.imp, x = 0, y = .6, width = .15, height = .3)+
  draw_plot(p2.imp, x = 0, y = .3, width = .15, height = .3)+
  draw_plot(p3.imp, x = 0, y = 0, width = .15, height = .3)+
  draw_plot(p1.exp, x = .8, y = .6, width = .15, height = .3)+
  draw_plot(p2.exp, x = .8, y = .3, width = .15, height = .3)+
  draw_plot(p3.exp, x = .8, y = 0, width = .15, height = .3) 


#Current year
p1.imp=fn.barplt(d=dummy%>%filter(Commodity=='Finfish'),yr=2021,trade='import',
                 Unit='Volume (1000s tonnes)',show.LGN=TRUE)
legend <- g_legend(p1.imp)
p1.imp=p1.imp+theme(legend.position = 'none')
p2.imp=fn.barplt(d=dummy%>%filter(Commodity=='Crustaceans'),yr=2021,trade='import',Unit='Volume (1000s tonnes)')
p3.imp=fn.barplt(d=dummy%>%filter(Commodity=='Molluscs'),yr=2021,trade='import',Unit='Volume (1000s tonnes)')
p1.exp=fn.barplt(d=dummy%>%filter(Commodity=='Finfish'),yr=2021,trade='export',Unit='Volume (1000s tonnes)')
p2.exp=fn.barplt(d=dummy%>%filter(Commodity=='Crustaceans'),yr=2021,trade='export',Unit='Volume (1000s tonnes)')
p3.exp=fn.barplt(d=dummy%>%filter(Commodity=='Molluscs'),yr=2021,trade='export',Unit='Volume (1000s tonnes)')
p.current=ggdraw() +
  draw_plot(p_Map+
              labs(title=2021)+theme(plot.title = element_text(hjust=0.5,vjust = 12.5,size=30))+
              geom_curve(data=data.frame(x=112,y=c(-15,-30,-39),
                                         x.end=130,y.end=c(-25,-26,-28)),
                         aes(x = x, y = y, xend = x.end, yend = y.end),size = 1.5,
                         arrow = arrow(length = unit(0.3, "cm"), type = "open"),
                         color = "brown4", curvature = 0.2,show.legend = FALSE)+
              geom_curve(data=data.frame(x=135,y=c(-25,-26,-28),
                                         x.end=158,y.end=c(-15,-30,-38)),
                         aes(x = x, y = y, xend = x.end, yend = y.end),size = 1.5,
                         arrow = arrow(length = unit(0.3, "cm"), type = "open"),
                         color = "brown4", curvature = 0.2,show.legend = FALSE))+
  draw_plot(legend, x = 0.4, y = .8, width = .2, height = .1)+
  draw_plot(p1.imp, x = 0, y = .6, width = .15, height = .3)+
  draw_plot(p2.imp, x = 0, y = .3, width = .15, height = .3)+
  draw_plot(p3.imp, x = 0, y = 0, width = .15, height = .3)+
  draw_plot(p1.exp, x = .8, y = .6, width = .15, height = .3)+
  draw_plot(p2.exp, x = .8, y = .3, width = .15, height = .3)+
  draw_plot(p3.exp, x = .8, y = 0, width = .15, height = .3) 



#2.2 Combine relevant plots
plot_grid(Production_wild.caught.only$p,Exports_commodity$p,
          p.original, p.current, nrow=2,ncol = 2, labels = c("A", "B","C","D"), rel_heights = c(.6, 1))
  ggsave(paste0(hndl.out,"Infographic_Production.jpg"),width = 20,height = 10) 
  
  
  
# Infographic - Shark production, exports and imports --------------------------------------------------------------------

  
# Infographic - Regulations --------------------------------------------------------------------
  