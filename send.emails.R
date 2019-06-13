
#library("devtools")
#install_github('omegahat/RDCOMClient')
library(RDCOMClient)
function.send.email=function(to,subject,body,Attachment)
{
  OutApp <- COMCreate("Outlook.Application")
  outMail = OutApp$CreateItem(0)
  outMail[["To"]] = to
  outMail[["subject"]] = subject
  outMail[["body"]] = body
  if(!is.null(Attachment))outMail[["Attachments"]]$Add(Attachment)
  outMail$Send()
}