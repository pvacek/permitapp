
config<-function(){
  config<-readLines("config.txt")
  
  PATH<-gsub("^PATH\\=","",config[grep("^PATH",config)])
  
  print("Checking for required packages...")
  
  PKG_checker<-function(package,PKGS=installed.packages()[,1]){
    if(any(grepl(package,PKGS))){
      print(paste0(package," is already installed."))
      require(package,character.only=TRUE)
    }
    else{
      print(paste0(package," not found. Installing ",package))
      install.packages(package)
      require(package,character.only=TRUE)
    }
    print(paste0(package," loaded."))
  }
  
  PKG_checker("stringr")
  
  req_PKGS<-str_split(gsub("^.*?\\[|\\].*?$","",config[grep("PKGS",config)]),",")[[1]]
  sapply(req_PKGS,PKG_checker)
  
  return(PATH)
}

if(any(ls()=="PATH")==FALSE){
  PATH<-config()
}

rm("config")

setwd(PATH)
