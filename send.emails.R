
#devtools::install_github("dkyleward/RDCOMClient")

# Load the DCOM library
library(RDCOMClient)

send.email=function(TO,Subject,Body,Attachment=NULL)
{
  # Open Outlook
  Outlook <- COMCreate("Outlook.Application")
  
  # Create a new message
  Email = Outlook$CreateItem(0)
  
  # Set the recipient, subject, and body
  Email[["to"]] = TO
  Email[["cc"]] = "matias.braccini@dpird.wa.gov.au"
  Email[["bcc"]] = ""
  Email[["subject"]] = Subject
  Email[["body"]] = Body
  if(!is.null(Attachment))Email[["attachments"]]$Add(Attachment)  
  
  # Send the message
  Email$Send()
  
  # Close Outlook, clear the message
  rm(Outlook, Email)
}
