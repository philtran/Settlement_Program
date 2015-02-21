library("tcltk")
rm(list=ls())
tt <- tktoplevel()
Name <- tclVar("")
settlebutton <- tkbutton(tt,text="Settlement Main",command=function()source("settlement_main_alt.R"))
entry1 <- tkentry(tt,textvariable=Name)
OnOk <- function() {
  NameVal <- as.numeric(tclvalue(Name))
#   msg <- paste("Blah",NameVal)
#   tkmessageBox(message=msg)
  val <- NameVal*2
  print(val)
}
OK.but <- tkbutton(tt,text="OK",command=OnOk)
tkbind(entry1,"<Return>",OnOk)
tkpack(settlebutton,entry1,OK.but)
#tkdestroy(tt)
#tkl <- tk_select.list(c("Hello","World"))