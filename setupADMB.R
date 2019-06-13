fn.create.bat.file=function(FILE)
{
  lista=list("rem",
  "rem Ensure that the home path to ADMB is set within the environment",
  as.character("set ADMB_HOME=C:\\Program Files (x86)\\ADMB"),
  "rem",
  "rem Save a copy of the current path, such that this may be reset later in the run",
  "set TempPath=%PATH%",
  "rem",
  "rem Insert a path to the folders containing the files required by ADMB at the",
  "rem front of the existing path",
  as.character("rem set PATH=C:\\Program Files (x86)\\ADMB\\bin;C:\\Program Files (x86)\\ADMB\\utilities;C:\\Program Files (x86)\\ADMB\\utilities\\mingw64\\bin;%PATH%"),
  "rem",
  "rem Compile the specified ADMB model template",
  "call admb %1",
  "rem",
  "rem Reset the path to its original state",
  "set PATH=%TempPath%")
  
  write(lista[[1]],file = FILE)
  for(l in 2:length(lista)) write(lista[[l]],file = FILE,sep = "\t",append=T)
 }