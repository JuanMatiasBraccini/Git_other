#Missing
# Q15 reduce occupations to a tangigle number of categories and use in analysis (see 'Use.Q15')

library(tidyverse)
library(readxl)
library(Hmisc)
library(ggpubr)
require(foreign)
require(MASS)
require(reshape2)
require(nnet)
require(grid) 

# Input data --------------------------------------------------------------------

User="Matias"
# User="Robiul"
if(!exists('handl_OneDrive'))
{
  if(User=="Matias") source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
  
  if(User=="Robiul")
  {
    handl_OneDrive=function(x)paste('C:/Users',Usr,'OneDrive - Department of Primary Industries and Regional Development/Matias',x,sep='/')
  }
}

Snowball.survey <- read_excel(handl_OneDrive("Students/2021_Robiul Hasan/4. Questionnaire/Data/Seafood labelling - Snowball_June 25, 2024_23.38.xlsx"),
                                sheet = "Sheet0",skip = 1,col_names = TRUE)
Snowball.survey.header <- read_excel(handl_OneDrive("Students/2021_Robiul Hasan/4. Questionnaire/Data/Seafood labelling - Snowball_June 25, 2024_23.38.xlsx"),
                              sheet = "Sheet0",skip = 0,col_names = TRUE,n_max=1)

Qualtrix.survey <- read_excel(handl_OneDrive("Students/2021_Robiul Hasan/4. Questionnaire/Data/Seafood labelling - Market_May 30, 2024_05.57.xlsx"),
                              sheet = "Sheet0",skip = 1,col_names = TRUE)
Qualtrix.survey.header <- read_excel(handl_OneDrive("Students/2021_Robiul Hasan/4. Questionnaire/Data/Seafood labelling - Market_May 30, 2024_05.57.xlsx"),
                                     sheet = "Sheet0",skip = 0,col_names = TRUE,n_max=1)

data.set.used.in.analysis='Qualtrix'  #c('Qualtrix','Snowball')

Percent.imported.seafood=c(70,75) #Senior, N., Stewardson, C. (2019). Promoting sustainable Australian fish and seafood. Food Australia, 71(2): 35–37

# Manipulate data --------------------------------------------------------------------
#note Qualtrix.survey$gc=1 #complete surveys
a=unlist(Snowball.survey.header)
Snowball.survey.header=data.frame(Link=names(a),Question=a,Snowball.survey.name=colnames(Snowball.survey))
colnames(Snowball.survey)=Snowball.survey.header$Link

a=unlist(Qualtrix.survey.header)
Qualtrix.survey.header=data.frame(Link=names(a),Question=a,Snowball.survey.name=colnames(Qualtrix.survey))
colnames(Qualtrix.survey)=Qualtrix.survey.header$Link

Snowball.survey=Snowball.survey%>%
                  mutate(Q4=extract_numeric(Q4),
                         gc=NA)

dis.Qualtrix=Qualtrix.survey[,colnames(Qualtrix.survey)[grep(paste(colnames(Snowball.survey),collapse='|'),colnames(Qualtrix.survey))]]
dis.Snowball=Snowball.survey%>%
              dplyr::select(colnames(dis.Qualtrix))


combined.data=rbind(dis.Qualtrix%>%mutate(Survey='Qualtrix'),
                    dis.Snowball%>%mutate(Survey='Snowball'))%>%
      data.frame%>%
      mutate(Q12_6_TEXT=ifelse(Q12_6_TEXT=="Alpha male","Male",Q12_6_TEXT),
             Q12=case_when(Q12=='Prefer self-describe (write in an option)'~Q12_6_TEXT,
                           Q12=="Man"~"Male",
                           Q12=="Woman"~"Female",
                           Q12=="Gender non-conforming"~"Non-binary",
                           is.na(Q12)~"Prefer not to disclose",
                           TRUE~Q12),
             Q12=factor(Q12,levels=c("Female","Male")),
             Q13=case_when(is.na(Q13)~"Prefer not to say",
                           TRUE~Q13),
             Q13=factor(Q13,levels=c("Less than 20","21-40","41-60","Over 60")),
             Q14=case_when(is.na(Q14)~"Prefer not to say",
                           Q14%in%c("Primary school","Secondary school") ~"School",
                           Q14== "TAFE/trade" ~"TAFE",
                           Q14== "University bachelor's degree" ~"Undergraduate",
                           Q14== "University higher degree (Master's or PhD)" ~"Postgraduate",
                           TRUE~Q14),
             Q14=factor(Q14,levels=c("School","TAFE","Undergraduate","Postgraduate")),
             Q16=case_when(is.na(Q16)~"Prefer not to say",
                           TRUE~Q16),
             Q17=case_when(is.na(Q17)~"Prefer not to say",
                           TRUE~Q17),
             Q18=capitalize(tolower(Q18)),
             Q18=ifelse(Q18%in%c("Nil","-",'????????????',"Au","Australia","Austrlia","N/a","No",
                                 "Nowhere","Victoria","Yes","Qld","Qld","Not applicable","2753","4000",
                                 "Been in australia since primary", "I didn't know i'd been in australia",
                                 "I live in perth","I'm living in australia","Na","Nil","None","North korea",
                                 "Sydney"),NA,Q18),
             Q18=case_when(Q18=="Uk"~"UK",
                           Q18=="South korea"~"South Korea",
                           Q18%in%c("New zealand","New zeland")~"New Zealand",
                           Q18%in%c("Srilanka","Sri lanka")~"Sri Lanka",
                           TRUE~Q18),
             Q4=case_when(Q4%in%c("Sixty percent","60 percent","60 pr cent")~'60',
                          Q4=="70 percent"~'70',
                          Q4=="over70%"~'75',
                          Q4=="5p%"~'5',
                          Q4=="about 80 percent comes from overseas"~'80',
                          Q4=="I think it would be around 55%"~'55',
                          Q4%in%c("50 per cent .","about 50 % maybe.")~'50',
                          Q4%in%c("at least forty","around 40%?","At least 40 percent")~'40',
                          Q4=="Thirty percent"~'30',
                          Q4%in%c("probably 60 plus","More than half. 65%")~'65',
                          Q4=="I think Australian seafood is mainly caught domestically"~'0',
                          Q4%in%c("May be unethical due to unknown treatment of seafood","Not sure",
                                  "To much to be honest it's not hard to catch it","I have no clue",
                                  "It is too costly for Australia not to import","I don't know",
                                  "Yes","The only way I could do that was if you wanted it",
                                  "I wonder why it isnt from australia","All good.","unknown",
                                  "I don't mind at all.","A low percentage","i dont know",
                                  "Mindell cons","dont know","Not dure","Too much","Don't know",
                                  "Same amount","Don't know","not sure","DO NOT KNOW","Like",
                                  "I believe that imported seafood is nowhere near the quality of Australian seafood.",
                                  "no idea","I only buy fresh fish from Western Australia","concerned","200","Idk",
                                  "good info","Dont know","Do not know","Unsure","I don't know")~"Do not know",
                           TRUE~Q4),
             Q4=str_remove(Q4, '%'),
             Q4=as.numeric(Q4),
             Q4=10*round(Q4/10),
             Q4=as.character(Q4),
             Q4=ifelse(is.na(Q4),'No answer',Q4),
             Q7_1=case_when(is.na(Q7_1)~"Unsure/unable to answer",
                            TRUE~Q7_1),
             Q8=gsub("\\s*\\([^\\)]+\\)","",Q8),
             Q8=case_when(is.na(Q8) | Q8=='None of the above or prefer not to answer'~"Prefer not to say",
                          TRUE~Q8),
             Q8=str_remove(Q8, 'The '),
             Q8=capitalize(Q8),
             Q9_1=case_when(Q9_1=='Not conerned'~'Not concerned',
                            is.na(Q9_1)~"Unsure/unable to answer",
                            TRUE~Q9_1),
             Q10=gsub("\\s*\\([^\\)]+\\)","",Q10),
             Q10=case_when(is.na(Q10) | Q10=='None of the above or prefer not to answer'~"Prefer not to say",
                          TRUE~Q10),
             Q10=str_remove(Q10, 'The '),
             Q10=capitalize(Q10),
             Q11_1=case_when(is.na(Q11_1) | Q11_1=='Unsure/Unable to answer'~"Unsure/unable to answer",
                            TRUE~Q11_1),
             Q11_2=case_when(is.na(Q11_2)| Q11_2=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_2),
             Q11_3=case_when(is.na(Q11_3)| Q11_3=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_3),
             Q11_4=case_when(is.na(Q11_4)| Q11_4=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_4),
             Q11_5=case_when(is.na(Q11_5)| Q11_5=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_5),
             Q11_6=case_when(is.na(Q11_6)| Q11_6=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_6),
             Q11_7=case_when(is.na(Q11_7)| Q11_7=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_7),
             Q11_8=case_when(is.na(Q11_8)| Q11_8=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_8),
             Q11_9=case_when(is.na(Q11_9)| Q11_9=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_9),
             Q11_10=case_when(is.na(Q11_10)| Q11_10=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_10),
             Q11_11=case_when(is.na(Q11_11)| Q11_11=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_11),
             Q11_12=case_when(is.na(Q11_12)| Q11_12=='Unsure/Unable to answer'~"Unsure/unable to answer",
                             TRUE~Q11_12),
             Q15=tolower(Q15),
             Q15=case_when(grepl('unemployed',Q15)~"unemployed",
                           Q15%in%c("unable to work","unemlpoyed","unemployed","unable to work due to spinal injuries")~'unemployed',
                           Q15%in%c("retired academic","retired research biologist")~"research/academia",
                           grepl('retired',Q15)~"retired",
                           Q15%in%c("tetired")~"retired",
                           is.na(Q15)~"Prefer not to disclose",
                           TRUE~Q15)
      )


# Create list of related questions -------------------------------------------------------------------- 
use.this.data.set=combined.data%>%    
                    filter(Survey%in%data.set.used.in.analysis)


Predictors=c('Survey',paste0('Q',12:18))
names(Predictors)=c('Survey','Gender','Age','Education','Occupation','Citizen','Postcode','Foreign.Country')
Use.Survey=Use.Q15=Use.Q16=Use.Q17=Use.Q18=FALSE
if(!Use.Survey) Predictors=subset(Predictors,!Predictors=="Survey")
if(!Use.Q15) Predictors=subset(Predictors,!Predictors=="Q15")
if(!Use.Q16) Predictors=subset(Predictors,!Predictors=="Q16")
if(!Use.Q17) Predictors=subset(Predictors,!Predictors=="Q17")
if(!Use.Q18) Predictors=subset(Predictors,!Predictors=="Q18")

Single.way.table=lapply(use.this.data.set[, Predictors], table)
Multi.way.table=ftable(xtabs(~ Survey + Q12 + Q13+ Q14, data = use.this.data.set)) #three way table

Q.list=list(Q1_=c('Q1_'),
            Q2_=c('Q2','Q2_20'),
            Q3_=c('Q3_'),
            Q4_=c('Q4'),
            Q5_=c('Q5','Q5_'),
            Q6_=c('Q6_'),
            Q7_=c('Q7_'),
            Q8_=c('Q8','Q8_'),
            Q9_=c('Q9_'),
            Q10_=c('Q10','Q10_'),
            Q11_=c('Q11_'))

Q.forced.strip=list(Q1_='',
                    Q2_='Products purchased in the last 3 months',
                    Q3_='',
                    Q4_='',
                    Q5_="'Flake' refers to",
                    Q6_='',
                    Q7_='Level of concern if the use of umbrella labelling was commonplace',
                    Q8_='Concerning issues about umbrella labelling',
                    Q9_='Level of concern if mislabelling was commonplace',
                    Q10_='Concerning issues about mislabelling',
                    Q11_='')

LVLS=list(Q1_=c("More than once per week","Once a week","Once a month","Once or twice",
                "Once every 3 months","Have not purchased","Not sure or prefer not to say"),
          Q2_=NULL,
          Q3_=NULL,
          Q4_=c(seq(0,100,10),'No answer'),
          Q5_=NULL,
          Q6_=c("Strongly agree","Agree","Neutral","Disagree","Strongly disagree","Unsure/unable to answer"),
          Q7_=c("Extremely concerned","Very concerned","Moderately concerned","Slightly concerned","Not concerned","Unsure/unable to answer"),
          Q8_=NULL,
          Q9_=c("Extremely concerned","Very concerned","Moderately concerned","Slightly concerned","Not concerned","Unsure/unable to answer"),
          Q10_=NULL,
          Q11_=c("Strongly agree","Agree","Neutral","Disagree","Strongly disagree","Unsure/unable to answer"))

Show.Answers=list(Q1_='x.axis',Q2_='x.axis',Q3_='x.axis',Q4_='plain',Q5_='x.axis',Q6_='x.axis',
                  Q7_='plain',Q8_='x.axis',Q9_='plain',Q10_='x.axis',Q11_='x.axis')

Coded.answer=list(Q1_=NULL,
                  Q2_='YES',
                  Q3_=NULL,
                  Q4_=NULL,
                  Q5_='YES',
                  Q6_=NULL,
                  Q7_=NULL,  
                  Q8_=NULL,
                  Q9_=NULL,
                  Q10_=NULL,
                  Q11_=NULL)

Drop.answers=list(Q1_=NULL,
                  Q2_=NULL,
                  Q3_='_TEXT',
                  Q4_=NULL,
                  Q5_='_TEXT',
                  Q6_=NULL,
                  Q7_=NULL,  
                  Q8_=NULL,
                  Q9_=NULL,
                  Q10_=NULL,
                  Q11_=NULL)

Res.vars=list(Q1_='Question.short',
              Q2_='Answer',
              Q3_='Question.short',
              Q4_='Answer',
              Q5_='Answer',
              Q6_='Answer',
              Q7_='Answer',  
              Q8_='Answer',
              Q9_='Answer',
              Q10_='Answer',
              Q11_='Answer')

Right.Answer=list(Q1_='',
              Q2_='',
              Q3_='',
              Q4_="70",
              Q5_="Gummy shark or rig",
              Q6_='',
              Q7_='',  
              Q8_='',
              Q9_='',
              Q10_='',
              Q11_='')

Mod.Type=list(Q1_='ordinal.logistic.reg',
              Q2_='multinomial.logistic.reg',
              Q3_='multinomial.logistic.reg',
              Q4_='binomial',  #'glm'
              Q5_='binomial',  #'multinomial.logistic.reg'
              Q6_='ordinal.logistic.reg',
              Q7_='ordinal.logistic.reg',  
              Q8_='multinomial.logistic.reg',
              Q9_='ordinal.logistic.reg',
              Q10_='multinomial.logistic.reg',
              Q11_='ordinal.logistic.reg')

XLABS=list(Q1_='',Q2_='',Q3_='',Q4_='Perceived percentage of imported seafood',Q5_='',Q6_='',Q7_='',Q8_='',Q9_='',Q10_='',Q11_='')

STRIP.s=list(Q1_=12,Q2_=12,Q3_=12,Q4_=12,Q5_=12,Q6_=11,Q7_=11,Q8_=11,Q9_=11,Q10_=11,Q11_=8)

WIDTH=list(Q1_=7,Q2_=6,Q3_=6,Q4_=6,Q5_=10,Q6_=10,Q7_=10,Q8_=10,Q9_=10,Q10_=10,Q11_=12)

LENGTH=list(Q1_=8,Q2_=8,Q3_=8,Q4_=7,Q5_=7,Q6_=7,Q7_=7,Q8_=7,Q9_=7,Q10_=7,Q11_=7)

# display distribution of responses --------------------------------------------------------------------
theme_PA=function(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
         cap.siz=10,lgT.siz=14,leg.siz=12,axs.t.siz=11,axs.T.siz=16)
{
  #font<-windowsFonts("Arial" = windowsFont("Arial"))
  #font="Arial"  #"TT Courier New"  "TT Arial"  "TT Times New Roman"
  theme_bw()%+replace% 
    theme(
      
      #panel
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1.15),
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.line = element_line(colour = "black"),
      #axis.ticks = element_line(),          #strip axis ticks
      
      # strip background
      strip.background = element_rect(
        fill = "grey90",
        colour = "grey90"),
      
      #text elements
      #title
      plot.title = element_text(             
        # family = font,                           
        size = Ttl.siz,                         
        face = 'bold',                           #bold typeface
        hjust = 0,                               #left align
        vjust = 2),                              #raise slightly
      
      #subtitle
      plot.subtitle = element_text(          
        # family = font,                           
        size = Sbt.siz,
        hjust = 0,                               #left align
        vjust = 2),                         
      
      #strip legend
      strip.text = element_text(
        # family = font,
        size = str.siz),
      strip.text.x = element_text(
        # family = font,
        size = strx.siz,
        margin = margin(.1,0,.1,0, "cm")),
      
      
      #caption
      plot.caption = element_text(          
        # family = font,                           
        size = cap.siz,                          
        hjust = 1),                             #right align
      
      #legend
      legend.title=element_text(
        # family = font,
        size=lgT.siz),
      legend.text=element_text(
        # family = font,
        size=leg.siz),
      
      #axis titles
      axis.title = element_text(             
        # family = font,                          
        size = axs.T.siz),                     
      
      #axis text
      axis.text = element_text(              
        # family = font,                          
        size = axs.t.siz)                       
    )
}


HNDL=handl_OneDrive('Students/2021_Robiul Hasan/4. Questionnaire/Outputs/')
le.paste=function(x) paste(HNDL,x,sep='')

display.q=function(DATA,QQ,METADATA,levls,XLAB,YLAB,show.answer,drop.answers,coded.answer,
                   STRIP.size,forced.strip,Preds,RV,model.type)
{
  id=grep(paste(QQ,collapse='|'),colnames(DATA))
  idMeta=grep(paste(QQ,collapse='|'),METADATA$Link)
  
  if("Q8"%in%QQ) #conditional responses
  {
    DATA=DATA%>%filter(Q7_1%in%c('Extremely concerned','Very concerned'))
  }
  if("Q10"%in%QQ) 
  {
    DATA=DATA%>%filter(!Q9_1%in%c('Unsure/unable to answer','Not concerned'))
  }
  
  dat=DATA[,c(match('ResponseId',names(DATA)),id)]%>%
    gather(Link,Answer,-ResponseId)%>%
    filter(!is.na(Answer))
  
  metadat=METADATA[idMeta,1:2]%>%
            mutate(Question.short=sub('.*- ', '', Question))
  dat=dat%>%
    left_join(metadat,by='Link')
  
  specified_TEXT=dat%>%filter(grepl('_TEXT',Link))%>%
                  mutate(Question.short1=Answer,
                         Link=sub('_TEXT', '', Link),
                         Link=sub('_8', '', Link),
                         Link=sub('_20', '', Link))%>%
                  dplyr::select(ResponseId,Link,Question.short1)
  dat=dat%>%
    filter(!grepl('_TEXT',Link))
  dat=left_join(dat,specified_TEXT,by=c('ResponseId','Link'))%>%
    mutate(Question.short=ifelse(Question.short=='Other please specify',Question.short1,Question.short))%>%
    dplyr::select(ResponseId,Link,Answer,Question.short,Question.short1)%>%
    filter(!grepl('Other',Question.short))

  dat=dat%>%
        mutate(Question.short=gsub("\\s*\\([^\\)]+\\)","",Question.short))

  if(!is.null(drop.answers))
  {
    dat=dat%>%
      filter(!grepl("please explain",Question.short))%>%
      filter(!grepl("please",Answer))
  }

  comma.split=dat%>%filter(grepl(",",Answer))
  if(nrow(comma.split)>0)
  {
    dat=dat%>%filter(!grepl(",",Answer))
    
    dumi=vector('list',nrow(comma.split))
    for(nn in 1:nrow(comma.split))
    {
      dd=data.frame(Answer=unlist(str_split(comma.split$Answer[nn], ',')))
      dumi[[nn]]=dd%>%mutate(ResponseId=comma.split$ResponseId[nn],
                             Link=comma.split$Link[nn],
                             Question.short=comma.split$Question.short[nn],
                             Question.short1=comma.split$Question.short1[nn])%>%
                      relocate(names(comma.split[nn,]))
    }
    dat=rbind(dat,do.call(rbind,dumi))
  }
  if(!is.null(levls))
  {
    if(any(levls%in%dat$Question.short)) dat=dat%>%mutate(Question.short=factor(Question.short,levels=levls))
    if(any(levls%in%dat$Answer)) dat=dat%>%mutate(Answer=factor(Answer,levels=levls))
    
    if("Extremely concerned"%in%levels(dat$Answer))
    {
      dat=dat%>%
        mutate(Answer=recode(Answer, 
             "Not concerned" = "Not\nconcerned",
             "Slightly concerned"="Slightly\nconcerned",
             "Moderately concerned"="Moderately\nconcerned",
             "Very concerned"="Very\nconcerned",
             "Extremely concerned"="Extremely\nconcerned",
             "Unsure/unable to answer"="Unsure/unable\n to answer"))
    }
    
    if("Strongly agree"%in%levels(dat$Answer))
    {
      dat=dat%>%
        mutate(Answer=recode(Answer, 
                             "Strongly agree" = "Strongly\nagree",
                             "Strongly disagree" = "Strongly\ndisagree",
                             "Unsure/unable to answer"="Unsure/unable\n to answer"))
    }
    
    if("No answer"%in%levels(dat$Answer))
    {
      dat=dat%>%
        mutate(Answer=recode(Answer, 
                             "No answer" = "No\nanswer"))
    }
    
  }
  if(is.null(levls))
  {
    dat=dat%>%
      mutate(Answer=ifelse(Answer=='Other please specify',Question.short1,Answer))%>%
      filter(!Answer=='Other please specify')%>%
      mutate(Answer=capitalize(tolower(Answer)),
             Answer=case_when(Answer=="Bronze whaler/bronzie"~"Bronze whaler",
                              Answer=="Wa dhufish"~"WA dhufish",
                              TRUE~Answer))
  }
  
  if("Fish and chip shop"%in%unique(dat$Answer) & !"Q1_"%in%QQ)
  {
    dat=dat%>%
      filter(!is.na(Question.short))%>%
      mutate(Answer=case_when(Answer=='Fish and chip shop'~'Fish and\nchip shop',
                              Answer=='Restaurant'~'Restaurant',
                              Answer=='Supermarket/fish monger'~'Supermarket/\nfish monger'))
  }
  
  Tabl=NULL
  if("Q2"%in%QQ)
  {
    Tabl=dat%>%group_by(Answer)%>%tally()%>%arrange(-n)%>%data.frame%>%mutate(Cum=cumsum(n),Cu.percent=Cum/sum(n))
    common.product=Tabl%>%filter(Cu.percent<0.98)%>%pull(Answer)
    dat=dat%>%
          mutate(Answer.old=Answer,
                 Answer=ifelse(!Answer%in%common.product,'Other',Answer))
  }
  
  if("Q3_"%in%QQ) dat=dat%>%filter(Answer%in%1:5)
  
  if(any("Q6_"%in%QQ,"Q11_"%in%QQ)) dat=dat%>%mutate(Question.short=sub("\\.", "", Question.short))
  
  if(!forced.strip=='') dat=dat%>%mutate(Question.short=forced.strip)
  
  if(show.answer=='strip')
  {
    Ncol=length(unique(dat$Answer))
    if(Ncol<25) Ncol=3
    if(Ncol>25) Ncol=4
    p=dat%>%
      ggplot(aes(Question.short))+
      geom_bar(aes(fill = Answer), position = "dodge")+
      facet_wrap(~Answer,ncol=Ncol)
    
  }
  if(show.answer=='x.axis')
  {
    Ncols=1
    if(length(unique(dat$Question.short))>6) Ncols=2

    if(any("Q1_"%in%QQ,"Q3_"%in%QQ))
    {
      p=dat%>%
        filter(!is.na(Question.short))%>%
        ggplot(aes(Question.short))+
        geom_bar(aes(fill = Question.short), position = "dodge")+
        facet_wrap(~Answer,ncol=1)+
        coord_flip()
    }else
    {
      if("Q5"%in%QQ)
      {
        dat=dat%>%mutate(FILL=as.factor(ifelse(!Answer=='Gummy shark or rig',1,2)),
                         Answer=case_when(Answer=="Certain species of scalefish but i do not know which ones"~"Certain species of scalefish\n but i do not know which ones",
                                          Answer=="Certain species of shark but i do not know which ones"~"Certain species of shark but\n i do not know which ones",
                                          TRUE~Answer))
      }else
      {
        dat=dat%>%mutate(FILL=as.factor(1))
      }
         
      p=dat%>%
        filter(!is.na(Question.short))%>%
        ggplot(aes(Answer))+
        geom_bar(aes(fill = FILL), position = "dodge")+
        facet_wrap(~Question.short,ncol=Ncols)
    }
    if(!is.null(coded.answer))p=p+coord_flip()
  }
  if(show.answer=='plain')
  {
    if("Q4"%in%QQ) 
    {
      dat=dat%>%
        mutate(FILL=ifelse(Answer=='70',2,1))
    }else
    {
      dat=dat%>%mutate(FILL=1)
    }
    if(!forced.strip=='')
    {
      p=dat%>%
        ggplot(aes(Answer,fill = as.factor(FILL)))+ 
        geom_bar(position = "dodge")+
        scale_x_discrete(drop = FALSE)+
        facet_wrap(~Question.short)
    }else
    {
      p=dat%>%
        ggplot(aes(Answer,fill = as.factor(FILL)))+ 
        geom_bar(position = "dodge")+
        scale_x_discrete(drop = FALSE)
    }

    if(!is.null(coded.answer))p=p+coord_flip()
  }
  p=p+theme_PA(strx.siz=STRIP.size)+
    theme(legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    xlab(XLAB)+ylab(YLAB)
  
  print(p)
  
  
  #Data for Stats  
  d.stats=dat%>%
            left_join(DATA[,c(match(c('ResponseId',Preds),names(DATA)))],
                      by='ResponseId')%>%
    filter(!is.na(Q12))%>%
    filter(!is.na(Q14))%>%
    filter(!Q13=='Prefer not to say')
  
  
  #Return stuff
  return(list(Tabl=Tabl,d.stats=d.stats))

}

    #Overall
Store.dat=vector('list',length(Q.list))
names(Store.dat)=names(Q.list)
for(i in 1:length(Q.list))
{
  print(paste('Plotting Question -----------------',names(Q.list)[i]))
  x=display.q(DATA=use.this.data.set,
              QQ=Q.list[[i]],
              METADATA=Snowball.survey.header,
              levls=LVLS[[i]],
              XLAB=XLABS[[i]],
              YLAB='Frequency',
              show.answer=Show.Answers[[i]],
              drop.answers=Drop.answers[[i]],
              coded.answer=Coded.answer[[i]],
              STRIP.size=STRIP.s[[i]],
              forced.strip=Q.forced.strip[[i]],
              Preds=Predictors,
              RV=Res.vars[[i]],
              model.type=Mod.Type[[i]])
  ggsave(le.paste(paste0(names(Q.list)[i],"Frequency distribution",".tiff")),width = WIDTH[[i]],height = LENGTH[[i]],compression = "lzw")
  if(!is.null(x$Tabl)) write.csv(x$Tabl,le.paste(paste0(names(Q.list)[i],"table.csv")),row.names = F)
  Store.dat[[i]]=x
}

    #By predictor (all predictors in single figure)
do.this=FALSE
if(do.this)
{
  Q.list.single=Q.list[match(c("Q4_","Q5_","Q7_","Q8_","Q9_","Q10_"),names(Q.list))]
  for(i in 1:length(Q.list.single))
  {
    for(x in 1:length(Predictors))
    {
      print(paste('Plotting Question -----------------',names(Q.list.single)[i],'-----Predictor----',names(Predictors)[x]))
      this=unique(use.this.data.set[,match(Predictors[x],names(use.this.data.set))])
      this=subset(this,!this%in%c("Prefer not to disclose",'Prefer not to say'))
      if(is.factor(this)) this=sort(this)
      dd=vector('list',length(this))
      for(l in 1:length(this))
      {
        dd[[l]]=display.q(DATA=use.this.data.set%>%filter((!!as.symbol(Predictors[x]))==this[l]),
                          QQ=Q.list.single[[i]],
                          METADATA=Snowball.survey.header,
                          levls=LVLS[[i]],
                          XLAB=XLABS[[i]],
                          YLAB='Frequency',
                          show.answer=Show.Answers[[i]],
                          drop.answers=Drop.answers[[i]],
                          coded.answer=Coded.answer[[i]],
                          STRIP.size=STRIP.s[[i]])
        dd[[l]]=dd[[l]]+ggtitle(this[l])
      }
      NKOL=1
      if(length(this)>6) NKOL=2
      ggarrange(plotlist = dd,ncol=NKOL,common.legend=TRUE)
      ggsave(le.paste(paste0("By predictor/",names(Q.list.single)[i],"Frequency distribution",'_',names(Predictors[x]),".tiff")),width = WIDTH[[i]],height = LENGTH[[i]],compression = "lzw")
      
    }
  }
  
  #By predictor (one figure per predictor
  Q.list.multi.figs=Q.list[-which(names(Q.list)%in%names(Q.list.single))]
  for(i in 1:length(Q.list.multi.figs))
  {
    for(x in 1:length(Predictors))
    {
      print(paste('Plotting Question -----------------',names(Q.list.multi.figs)[i],'-----Predictor----',names(Predictors)[x]))
      this=unique(use.this.data.set[,match(Predictors[x],names(use.this.data.set))])
      this=subset(this,!this%in%c("Prefer not to disclose",'Prefer not to say'))
      if(is.factor(this)) this=sort(this)
      dd=vector('list',length(this))
      for(l in 1:length(this))
      {
        dd[[l]]=display.q(DATA=use.this.data.set%>%filter((!!as.symbol(Predictors[x]))==this[l]),
                          QQ=Q.list.multi.figs[[i]],
                          METADATA=Snowball.survey.header,
                          levls=LVLS[[i]],
                          XLAB=XLABS[[i]],
                          YLAB='Frequency',
                          show.answer=Show.Answers[[i]],
                          drop.answers=Drop.answers[[i]],
                          coded.answer=Coded.answer[[i]],
                          STRIP.size=STRIP.s[[i]])
        dd[[l]]+ggtitle(this[l])
        dumy=this[l]
        if(dumy=='TAFE/trade') dumy='TAFE or trade'
        ggsave(le.paste(paste0("By predictor/",names(Q.list.multi.figs)[i],"Frequency distribution",'_',names(Predictors[x]),"_",dumy,".tiff")),width = WIDTH[[i]],height = LENGTH[[i]],compression = "lzw")
        
      }
      
    }
  }
  
}

#Display predictors
p1=use.this.data.set%>%
  ggplot(aes(Q12))+ 
  geom_bar(fill = "#F8766D",position = "dodge")+
  scale_x_discrete(drop = FALSE)+
  theme_PA(strx.siz=STRIP.size)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab('Gender')+ylab('')

p2=use.this.data.set%>%
  ggplot(aes(Q13))+ 
  geom_bar(fill = "#F8766D",position = "dodge")+
  scale_x_discrete(drop = FALSE)+
  theme_PA(strx.siz=STRIP.size)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab('Age')+ylab('Frequency')

p3=use.this.data.set%>%
  ggplot(aes(Q14))+ 
  geom_bar(fill = "#F8766D",position = "dodge")+
  scale_x_discrete(drop = FALSE)+
  theme_PA(strx.siz=STRIP.size)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab('Highest education')+ylab('')

ggarrange(plotlist = list(p1,p2,p3),ncol=1)
ggsave(le.paste(paste0("Predictors_Frequency distribution",".tiff")),
       width = 6,height = 8,compression = "lzw")


# Stats --------------------------------------------------------------------
for(i in 1:length(Q.list))
{
  d.stats=Store.dat[[i]]$d.stats
  RV=Res.vars[[i]]
  model.type=Mod.Type[[i]]
  QQ=Q.list[[i]]
  Q.out=names(Store.dat)[i]
  
  if("Q1_"%in%QQ)
  {
    d.stats.list=list(fish.chips=d.stats%>%filter(Answer=='Fish and chip shop'),
                      Restaurant=d.stats%>%filter(Answer=='Restaurant'),
                      Supermarket=d.stats%>%filter(Answer=='Supermarket/fish monger'))
  } else if("Q3_"%in%QQ)
  {
    d.stats.list=list(rank_1=d.stats%>%filter(Answer==1),
                      rank_2=d.stats%>%filter(Answer==2),
                      rank_3=d.stats%>%filter(Answer==3),
                      rank_4=d.stats%>%filter(Answer==4),
                      rank_5=d.stats%>%filter(Answer==5))
  } else if("Q6_"%in%QQ)
  {
    d.stats.list=list(umbrella=d.stats%>%filter(Question.short=="In Australian fish and chip shops menus, umbrella labelling of seafood products is very rare"),
                      mislabelling=d.stats%>%filter(Question.short=="In Australian fish and chip shops menus, mislabelling of seafood products is very rare"),
                      accurate=d.stats%>%filter(Question.short=="In Australian fish and chip shops menus, product labels should contain accurate information about the species and origin of the product"))
  } else if("Q11_"%in%QQ)
  {
    d.stats.list=list(expect_sustainable=d.stats%>%filter(Question.short=="When purchasing seafood in Australia, I expect the product to be sustainable"),
                      expect_ethical=d.stats%>%filter(Question.short=="When purchasing seafood in Australia, I expect the product to be ethically sourced"),
                      expect_healthy=d.stats%>%filter(Question.short=="When purchasing seafood in Australia, I expect the product to be 'healthy'"),
                      know_species=d.stats%>%filter(Question.short=="When purchasing seafood in Australia, I want to know what species I am purchasing"),
                      know_origin=d.stats%>%filter(Question.short=="When purchasing seafood in Australia, I want to know where the product comes from"),
                      not.purchase_sust=d.stats%>%filter(Question.short=="I would not purchase seafood that is not sustainable"),
                      not.purchase_ethical=d.stats%>%filter(Question.short=="I would not purchase seafood that is not ethically sourced"),
                      not.purchase_health=d.stats%>%filter(Question.short=="I would not purchase seafood that is not good for my health"),
                      pay.extra.10=d.stats%>%filter(Question.short=="I am willing to pay an extra 10% for a sustainable, ethically-sourced and 'healthy' fillet of fish"),
                      pay.extra.20=d.stats%>%filter(Question.short=="I am willing to pay an extra 20% for a sustainable, ethically-sourced and 'healthy' fillet of fish"),
                      assume.good=d.stats%>%filter(Question.short=="I assume that  seafood products purchased in Australia are mainly sustainable, ethically-sourced and 'healthy'"),
                      local.imported=d.stats%>%filter(Question.short=="I prefer to purchase Australian seafood over imported seafood"))
  }else
  {
    d.stats.list=list(d.stats=d.stats)
  }
  for(j in 1:length(d.stats.list))
  {
    print(paste('Stats for Question -----------------',names(Q.list)[i],'----------',names(d.stats.list)[j]))
    dd=d.stats.list[[j]]%>%
      mutate(response=!!sym(RV),
             Q12_Q13=paste(Q12,Q13))%>%
      filter(!is.na(response))%>%
      filter(!response%in%c("Not sure or prefer not to say","Unsure/unable to answer","No\nanswer"))
    if(is.factor(dd$response) & "Not sure or prefer not to say"%in%levels(dd$response))
    {
      dd$response=droplevels(dd$response)
    }
    if(is.factor(dd$response) & "Unsure/unable to answer"%in%levels(dd$response))
    {
      dd$response=droplevels(dd$response)
    }
    if(is.factor(dd$response) & "No\nanswer"%in%levels(dd$response))
    {
      dd$response=droplevels(dd$response)
    }
    if(is.character(dd$response)) dd$response=as.factor(dd$response)

    if(model.type%in%c('multinomial.logistic.reg','ordinal.logistic.reg'))
    {
      #fit model
      if(model.type=='ordinal.logistic.reg') m <- polr(paste('response', '~', paste(Predictors,collapse='+')), data = dd, Hess=TRUE)
      if(model.type=='multinomial.logistic.reg') m <- multinom(paste('response', '~', paste(Predictors,collapse='+')), data = dd)
      
      #display response variable
      WD=10
      AnglE=0
      if(length(unique(dd$response))>10)
      {
        WD=14
        AnglE=90
      }
        
      dd%>%
        ggplot(aes(x = response)) +
        geom_bar(aes(fill=Q14)) +
        facet_wrap(~ Q12+Q13, ncol=2,scales='free_y') +
        theme(legend.position = 'top',
              axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
      ggsave(le.paste(paste0("Stats/",Q.out,"_",names(d.stats.list)[j],"_","1_raw data_histogram.tiff")),
             width = WD,height = 7,compression = "lzw")
      
      #Model summary
      Summary.model=summary(m)
      Summary.stats <- coef(Summary.model)
      ANOVA=car::Anova(m)%>%data.frame
      
      if(model.type=='ordinal.logistic.reg')
      {
        p <- pnorm(abs(Summary.stats[, "t value"]), lower.tail = FALSE) * 2
        names(p)=paste0('p value_',names(p))
      }
      if(model.type=="multinomial.logistic.reg")
      {
        std_errors <- Summary.model$standard.errors
        z_scores <- Summary.stats / std_errors
        p <- 2 * (1 - pnorm(abs(z_scores)))
        colnames(p)=paste0('p value_',colnames(p))
      }
      Summary.stats <- cbind(Summary.stats, p)
      Summary.stats=Summary.stats%>%data.frame
      
      #Get predicted probabilities for all terms
      newdat=dd[,Predictors]%>%distinct()
      newdat <- cbind(newdat, predict(m, newdat, type = "probs"))
      lnewdat <-  newdat%>%
        gather(Level,Probability,-c(Predictors))%>%
        mutate(Level=factor(Level,levels=levels(dd$response)))
      if(any(grepl('\n',unique(lnewdat$Level))))
      {
        lnewdat=lnewdat%>%
          mutate(Level=factor(as.character(str_replace(str_replace(Level,'\n',' '), " +", " ")),
                              levels=str_replace(str_replace(levels(dd$response),'\n',' '), " +", " ")))
      }
      lnewdat%>%
        ggplot(aes(x = Q12, y = Probability)) +
        geom_bar(aes(fill =Q13),stat='identity', position = "dodge",width=.5)+
        facet_grid(Q14~Level)+
        #facet_grid(Q12 ~ Q14, labeller="label_both")+ 
        theme_PA(strx.siz=10)+
        theme(legend.position = 'top',
              legend.title=element_blank(),
              axis.text.x = element_text(angle = AnglE, vjust = .5, hjust=0.5))+
        xlab('')
      #geom_jitter(width = 0.15)+
      #coord_flip()
      ggsave(le.paste(paste0("Stats/",Q.out,"_",names(d.stats.list)[j],"_","2_model_predictions.tiff")),
             width = WD,height = 7,compression = "lzw")
      
      #Get predicted probabilities for all significant terms only
      dis.preds=ANOVA%>%filter(Pr..Chisq.<0.05)%>%rownames()
      if(length(dis.preds)>0 & length(dis.preds)<length(Predictors))
      {
         a=lnewdat%>%
          group_by_at(c('Level',dis.preds))%>%
          summarise(Probability=mean(Probability))
        Kls=colnames(a)[which(!colnames(a)%in%c('Probability','Level'))]
        Wi=5
        NKL=1
        if(length(unique(lnewdat[,Kls[1]]))>3) Wi=6
        if(length(unique(dd$response))>10) NKL=3
        p=a%>%
          ggplot(aes_string(x = Kls[1], y = 'Probability')) +
          facet_wrap(~Level,ncol=NKL)
        if(length(Kls)==1)  p=p+ geom_bar(stat='identity', position = "dodge",width=.5)
        if(length(Kls)==2) p=p+geom_bar(aes_string(fill =Kls[2]),stat='identity', position = "dodge",width=.5)
        p+
          theme_PA()+
          theme(legend.position = 'top',
                legend.title=element_blank(),
                axis.text.x = element_text(angle = 0, vjust = .5, hjust=0.5))+
          xlab('')
        ggsave(le.paste(paste0("Stats/",Q.out,"_",names(d.stats.list)[j],"_","2_model_predictions_significant terms.tiff")),
               width = Wi,height = 7,compression = "lzw")
        
      }
      
    }
    
    if(model.type=='glm')  
    {
      dd$response=as.numeric(as.character(dd$response))
      
      dd%>%
        ggplot(aes(x = Q13, y = response)) +
        geom_boxplot(size = .75) +
        geom_jitter(alpha = .5) +
        facet_grid(Q12 ~ Q14, labeller="label_both") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
      ggsave(le.paste(paste0("Stats/",Q.out,"_",names(d.stats.list)[j],"_","1_raw data_boxplot.tiff")),width = 7,height = 7,compression = "lzw")
      
      
      dd%>%
        ggplot(aes(x = response,fill=Q12)) +
        geom_bar(position='dodge') +
        facet_grid(Q14 ~ Q13, labeller="label_both",scales='free') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      ggsave(le.paste(paste0("Stats/",Q.out,"_",names(d.stats.list)[j],"_","1_raw data_histogram.tiff")),width = 7,height = 7,compression = "lzw")
      
      
      m <- glm(paste('response', '~', paste(Predictors,collapse='+')), data = dd)
      
      ANOVA=anova(m,test="Chisq")%>%data.frame
      Summary.stats=summary(m)
      Summary.stats=Summary.stats$coefficients
      
      newdat=dd[,Predictors]%>%distinct()
      PRED=predict(m, newdat,"response",se.fit = T)
      newdat <- cbind(newdat, Response=PRED$fit,SE=PRED$se.fit)
      
      ggplot(newdat, aes(x =  Q13, y = Response)) +
        geom_point() +
        geom_errorbar(aes(ymin = Response-1.96*SE, ymax = Response+1.96*SE),width = 0.3)+
        facet_grid(Q12 ~ Q14)+ 
        #facet_grid(Q12 ~ Q14, labeller="label_both")+ 
        theme_PA()+
        theme(legend.position = 'top',
              legend.title=element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ylim(0,100)+xlab('')+
        coord_flip()
      ggsave(le.paste(paste0("Stats/",Q.out,"_",names(d.stats.list)[j],"_","2_model_predictions.tiff")),width = 8,height = 7,compression = "lzw")
      
    }
    
    if(model.type=='binomial')   
    {
      dd=dd%>%
        mutate(response=ifelse(response==Right.Answer[[i]],1,0))
      dd%>%
        mutate(response=as.character(response))%>%
        ggplot(aes(x = response,fill=Q12)) +
        geom_bar() +
        facet_grid(Q14 ~ Q13, labeller="label_both",scales='free') 
      ggsave(le.paste(paste0("Stats/",Q.out,"_",names(d.stats.list)[j],"_","1_raw data_histogram.tiff")),width = 7,height = 7,compression = "lzw")
      
      
      m <- glm(paste('response', '~', paste(Predictors,collapse='+')), data = dd,family = binomial)
      
      ANOVA=anova(m,test="Chisq")%>%data.frame
      Summary.stats=summary(m)
      Summary.stats=Summary.stats$coefficients
      
      #Get predicted probabilities for all terms
      newdat=dd[,Predictors]%>%distinct()
      lnewdat <- cbind(newdat, Probability=predict(m, newdat,"response"))
      
      lnewdat%>%
        ggplot(aes(x = Q12, y = Probability)) +
        geom_bar(aes(fill =Q13),stat='identity', position = "dodge",width=.5)+
        facet_wrap(~Q14,ncol=1)+
        theme_PA(strx.siz=10)+
        theme(legend.position = 'top',
              legend.title=element_blank(),
              axis.text.x = element_text(angle = AnglE, vjust = .5, hjust=0.5))+
        xlab('')
      ggsave(le.paste(paste0("Stats/",Q.out,"_",names(d.stats.list)[j],"_","2_model_predictions.tiff")),
             width = 6,height = 7,compression = "lzw")
      
      #Get predicted probabilities for all significant terms only
      dis.preds=ANOVA%>%filter(Pr..Chi.<0.05)%>%rownames()
      if(length(dis.preds)>0 & length(dis.preds)<length(Predictors))
      {
        a=lnewdat%>%
          group_by_at(dis.preds)%>%
          summarise(Probability=mean(Probability))
        Kls=colnames(a)[which(!colnames(a)%in%c('Probability','Level'))]
        Wi=5
        NKL=1
        if(length(unique(lnewdat[,Kls[1]]))>3) Wi=6
        #if(length(unique(dd$response))>10) NKL=3
        p=a%>%
          ggplot(aes_string(x = Kls[1], y = 'Probability')) 
        if(length(Kls)==1)  p=p+ geom_bar(stat='identity', position = "dodge",width=.5)
        if(length(Kls)==2) p=p+geom_bar(aes_string(fill =Kls[2]),stat='identity', position = "dodge",width=.5)
        p+
          theme_PA()+
          theme(legend.position = 'top',
                legend.title=element_blank(),
                axis.text.x = element_text(angle = 0, vjust = .5, hjust=0.5))+
          xlab('')
        ggsave(le.paste(paste0("Stats/",Q.out,"_",names(d.stats.list)[j],"_","2_model_predictions_significant terms.tiff")),
               width = Wi,height = 7,compression = "lzw")
        
      }
      
    }
    
    
    #export anova table
    write.csv(Summary.stats, le.paste(paste0("Stats/",Q.out,names(d.stats.list)[j],"_","model summary.csv")), 
                row.names = TRUE)
    write.csv(ANOVA, le.paste(paste0("Stats/",Q.out,names(d.stats.list)[j],"_","ANOVA.csv")), 
              row.names = TRUE)
    
    
  }
  rm(d.stats,RV,model.type,QQ,Q.out,dd,m,Summary.stats,ANOVA)
}