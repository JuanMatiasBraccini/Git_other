
#devtools::install_github("dkyleward/RDCOMClient")

# Load the DCOM library
library(RDCOMClient)

send.email=function(TO,CC="matias.braccini@dpird.wa.gov.au",BCC="",Subject,Body,Attachment=NULL)
{
  # Open Outlook
  Outlook <- COMCreate("Outlook.Application")
  
  # Create a new message
  Email = Outlook$CreateItem(0)
  
  # Set the recipient, subject, and body
  Email[["to"]] = TO
  Email[["cc"]] = CC
  Email[["bcc"]] = BCC
  Email[["subject"]] = Subject
  Email[["body"]] = Body
  if(!is.null(Attachment))Email[["attachments"]]$Add(Attachment)  
  
  # Send the message
  Email$Send()
  
  # Close Outlook, clear the message
  rm(Outlook, Email)
}
