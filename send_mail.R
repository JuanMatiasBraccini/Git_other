library(mail)
MAIL=function(eMail,Subject,Message) sendmail(recipient=eMail,subject=Subject,message=Message)

#How to use it
# Value=1
# Result=ifelse(Value>0,"Well done","Crap, back to the drawing board")
# 
# MAIL(eMail="Matias.Braccini@fish.wa.gov.au",
#      Subject="Simulation result",
#      Message=paste("Hey dude,",Value,"is the output of that endless simulation you run all day.\n",Result))