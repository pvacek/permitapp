
source("handler.R")
source(paste0(PATH,"/scripts/dropbox/drop_init.R"))

wipe_directory<-function(option=NULL){
  if(is.null(option)){
    return()
  }
  else if(option=="dropbox"){
    setwd(paste0(PATH,"data/"))
    system("rm -rf ./dropbox; mkdir ./dropbox")
    setwd("..")
  }
  else if(option=="examples"){
    setwd(paste0(PATH,"data/"))
    system("rm -rf ./examples; mkdir ./examples")
    setwd("..")
  }
}

parser<-ArgumentParser()
parser$add_argument('--dropbox',type='logical',default=TRUE)
parser$add_argument('--populate',type='logical',default=TRUE)
parser$add_argument('--statecheck',type='logical',default=TRUE)
parser$add_argument('--dropdel',type='logical',default=FALSE)

if(parser$args$dropbox){
  print("DROP2SRV: Download files from dropbox folder...")
  source("./scripts/dropbox/drop2srv.R")
  print("...Done")
}

if(parser$args$populate){
  print("SRV2DB: Running dropbox files through handler script...")
  source("./scripts/dropbox/srv2db.R")
  print("...Done")
}

if(parser$args$statecheck){
  print("STATECHECK: Updating permits for state detection...")
  print("...Resetting folder...")
  wipe_directory("examples")
  source("./scripts/statecheck/expl_init.R")
  print("...Sending files from Dropbox to Examples...")
  source("./scripts/statecheck/srv2expl.R")
  print("...Creating new term-document matrix...")
  source("./scripts/statecheck/expl2tdm.R")
  print("...Done")
}

if(parser$args$dropdel){
  print("DROPDEL: Resetting dropbox folder...")
  wipe_directory("dropbox")
  print("...Done")
}

rm(list="wipe_directory")
