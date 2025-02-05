##############################################################################
#By Leon Pheng
##############################################################################

#' Install Packages (Internal Use)
#'
#' Install packages
#'
#'
#' @keywords ipak
#' @export
#' @examples
#' ipak(list of functions)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#' Install Packages (First Installation)
#'
#' To install required packages. Use ipak function to install desired packages.
#' @param packages pre-define packages list.
#' @keywords install.pack
#' @export
#' @examples
#' install.packages()

install.packages<-function(...){
  packages <- c("SASxport", "reshape", "Hmisc", "tidyr","plyr","downloader","officer","flextable")
  ipak(packages)

  require(downloader)
  dir.create("c:/lhtemplate")
  wf<-"c:/lhtemplate"
  url <- sprintf("https://github.com/%s/%s/archive/master.zip", "leonpheng","logo.style")
  tmp <- tempfile(fileext = "style1.zip",tmpdir=wf)
  download(url, tmp)
  unzip(tmp,exdir=wf)

  #download_repo("logo.style","leonpheng")
  zipF<-paste0(wf,"/logo.style-master/style1.zip")
  unzip(zipF,exdir=wf)
  zipF<-paste0(wf,"/logo.style-master/logostyle.zip")
  unzip(zipF,exdir=wf)
  zipF<-paste0(wf,"/logo.style-master/PCSmisc_0.9.2.zip")
  dir.create("c:/lhtemplate/package")
  file.copy(zipF, "c:/lhtemplate/package")
  frm<-dir(wf)
  index<-c(grep("zip",frm))
  frm<-frm[index]
  for(i in frm){
    file.remove(paste(wf,i,sep="/"))
  }
  unlink(paste0(wf,"/logo.style-master"), recursive = T)

  }


#Load needed packages: Install if not available
#' load.pack1
#'
#' To install require packages. Use ipak function to install desired packages.
#' @param packages pre-define packages list.
#' @keywords load.pack1
#' @export
#' @examples
#' load.pack1()

load.pack1<-function(...){
  library(officer)
  library(flextable)
  library(plyr)
  require(reshape)     # To format summary
  #require(PCSmisc)     #
  require(SASxport)   #This package is needed to add a dataset label,it can only be used  with the 32bit version of R. If dataset label is not needed, the package SASxport can be used
  require(Hmisc)
  require(tidyr)
}

##########################
#-----USEFUL FUNTIONS->>>>#
#' nodup
#'
#' internal used function.
#'
#' @keywords nodup
#' @export
#' @examples
#' nodup()

nodup<-function(data,var,all,item){
  if(all=="all"){d1<-data[!duplicated(data[,var]),names(data)]}else{
    if(all=="var"){d1<-data[!duplicated(data[,var]),var]}else{
      d1<-data[!duplicated(data[,var]),c(var,item)]}}
  d1
}

#-----USEFUL FUNTIONS->>>>#
#' chclass
#'
#' internal used function.
#'
#' @keywords chclass
#' @export
#' @examples
#' chclass()
chclass<-function(data,var,class="char"){
  for(i in var){
    if (class=="num"){
      data[,i]<-as.numeric(as.character(data[,i]))}
    else {data[,i]<-as.character(data[,i])}
  }
  data
}

#' autoclass
#'
#' internal used function.
#'
#' @keywords autoclass
#' @export
#' @examples
#' autoclass()
autoclass<-function(dat){
  dat<-chclass(dat,names(dat),"char")
  var<-names(dat)
  for(i in var){
    # i="othername"
    numt<-as.numeric(dat[,i])
    if(all.is.numeric(dat[,i]))#,what = c("test","vector"),extras=c('.','NA',"NaN"," ","  ",NA)))
      #nrow(dat[!is.na(as.numeric(as.character(dat[,i]))),])==0)
    {dat[,i]<-unlist(as.numeric(as.character(dat[,i])))
    }else{
      dat[,i]<-paste0("",unlist(dat[,i]))}

  }
  dat
}

#' mod
#'
#' internal used function.
#'
#' @keywords mod
#' @export
#' @examples
#' mod()

mod<-function(filename,var){
  dat<-read.csv(filename)
  dat<-dat[nchar(names(dat))!=0,]
  asnum<-var
  write.csv(chclass(dat,asnum,"num"),filename,row.names=F)
}
#################

#' importfiles
#'
#' internal used function.
#'
#' @keywords importfiles
#' @export
#' @examples
#' importfiles()
 

importfiles<-function(...){
  #lst<- read.xls(paste(pathwork,"list of files.xlsx",sep="/"), sheet = 1)
  library(haven)
  library(tidyverse)
  library(officer)
  library(flextable)
  library(PCSmisc)
  
  setwd(working.folder)
  mainDir<-getwd()
  subDir<-c("input","output","Backup")
  
  for(i in 1:length(subDir)){
    dir.create(file.path(mainDir, subDir[i]), showWarnings = FALSE)
  }
  
  mainDir<-paste0(getwd(),"/output")
  
  subDir<-c("datasets","programs")
  for(i in 1:length(subDir)){
    dir.create(file.path(mainDir,subDir[i]), showWarnings = FALSE)
  }
  
  lst<- read.csv("list of files.csv")
  lst$rename<-gsub("\\_","",lst$rename)
  lst$rename<-gsub(" ","",lst$rename)
  write.csv(lst,"list of files.csv",row.names = F)
  
  subDir2<-unique(lst$subfolder[lst$type=="txt"])
  mainDir2<-paste0(getwd(),"/output/programs")
  
  if(agency=="PMDA"){
  for(i in 1:length(subDir2)){
    dir.create(file.path(mainDir2,subDir2[i]), showWarnings = FALSE)
  }}
  
  lst<- read.csv("list of files.csv")
  lst$type1<-"nonmem"
  lst$type1[grep(".csv",lst$filename)]<-"csv"
  lst$type1[lst$type!="dataset"]<-"txt"
  
  lst$extension=".csv"
  lst$extension[lst$type=="txt"]<-".txt"
  conv<-with(lst,paste0(rename,".",type))
  
  #lst<-lst[lst$xptconvert==1,]
  
for(i in 1:nrow(lst)){
odir1<-paste0(getwd(),"/input/")
#PMDA  
  if(agency=="PMDA"){
    odir2<-as.character(paste0(getwd(),"/output/programs/",lst$subfolder[i]))
  }else{
    odir2<-as.character(paste0(getwd(),"/output/programs/"))
  }  
  
sour<-as.character(paste0(lst$sourcepath[i],"\\",lst$filename[i]))


if(lst$type[i]%in%c("xpt","csv")){

if(grepl("\\.csv$", lst$filename[i])){
    file.copy(from=sour,
    to=paste0(odir1,conv[i]),overwrite=T)}
      
  if(grepl("\\.xpt$", lst$filename[i])){
      tt<-as.data.frame(read_xpt(sour))
      write.csv(tt,paste0(odir1,conv[i]))}
    
  if(grepl("\\.html$", lst$filename[i])){
        library(rvest)
        url <- sour
        url %>%
          read_html %>%
          html_table(header=T) %>%
          .[[1]] -> df
        df<-as.data.frame(df)
        write.csv(df,paste0(odir1,conv[i]))}
      
if(grepl("\\.sas7bdat$", lst$filename[i])){
  tt<-as.data.frame(read_sas(sour))
  write.csv(tt,paste0(odir1,conv[i]))} 
 
if(!grepl("\\.sas7bdat$", lst$filename[i])&!grepl("\\.csv$", lst$filename[i])&
   !grepl("\\.xpt$", lst$filename[i])&!grepl("\\.html$", lst$filename[i])){
        tt<-read.nonmem.table(sour)
        write.csv(tt,paste0(odir1,conv[i]))}        
}else{
       if(grepl("\\.csv$", lst$filename[i])){  
        file.copy(from=sour,to=paste0(odir2,"/",rename1),overwrite=T)}
    
          if(grepl("\\.xpt$", lst$filename[i])){
            tt<-as.data.frame(read_xpt(sour))
            write.table(tt,paste0(odir2,"/",conv[i]),sep=",",row.names=FALSE)}
            
            if(grepl("\\.html$", lst$filename[i])){
              library(rvest)
              url <- sour
              url %>%
                read_html %>%
                html_table(header=T) %>%
                .[[1]] -> df
              tt<-as.data.frame(df)
              write.table(tt,paste0(odir2,"/",conv[i]),sep=",",row.names=FALSE)
              }
            
            if(grepl("\\.sas7bdat$", lst$filename[i])){
              tt<-as.data.frame(read_sas(sour))
              write.table(tt,paste0(odir2,"/",conv[i]),sep=",",row.names=FALSE)}
    
    if(!grepl("\\.sas7bdat$", lst$filename[i])&!grepl("\\.csv$", lst$filename[i])&
       !grepl("\\.xpt$", lst$filename[i])&!grepl("\\.html$", lst$filename[i])){
      file.copy(from=sour,to=paste0(odir2,"/",conv[i]),overwrite=T)
    } 
        
  }
}
}



#' definelist
#'
#' internal used function.
#'
#' @keywords definelist
#' @export
#' @examples
#' definelist()

definelist<-function(...){

  read.csv.custom <- function(filename) {
    x <- read.csv( filename, stringsAsFactors=F )
    names(x) <- gsub( x=names(x), pattern="[.]", replacement="_" )
    return(x)
  }

  setwd(working.folder)
  pathwork<-getwd()
  odir1<-paste0(pathwork,"/input/")
  var<-NULL
  for(i in dir(odir1)){
    fi<-read.csv.custom(paste0(odir1,i))
    #var1<-data.frame(orivar=unlist(names(read.csv(paste0(odir1,i)))),file=i)
    var1<-data.frame(orivar=unlist(names(fi)),file=i)
    var<-rbind(var,var1)
  }

  if(define.library!="no"){
    lib<-read.csv(define.library,stringsAsFactors=F)
    lib<-lib[,c("Variable","Unit","Detailed.description","Enter.label.here")]
    names(var)[1]<-"Variable"
    var[,"Variable"]<-tolower(var[,"Variable"]);lib[,"Variable"]<-tolower(lib[,"Variable"])
    var<-dplyr::left_join(var,lib)
    head(var)
    lib<-with(var,data.frame(
      Variable=Variable,
      Unit=Unit,
      Detailed.description=Detailed.description,
      Enter.label.here=Enter.label.here,
      Max.40.char=nchar(as.character(Enter.label.here)),
      file=file))
  }else{
    lib<-with(var,data.frame(
      Variable=orivar,
      Unit="",
      Detailed.description="",
      Enter.label.here="",
      Max.40.char="USE FUNCTION LEN()",
      file=file))
    }
  keep<-names(lib)

  lib1<-lib#plyr::join(var,lib)
  #lib1$"SAS.label"<-lib1$"Enter.label.here"
  lib1$"Max.40.char"<-nchar(as.character(lib1$"Enter.label.here"))
lib1$labelsize<-nchar(lib1$Enter.label.here)#nchar(as.character(lib1$"Variable"))

keep<-c("Variable","Unit","Detailed.description","Enter.label.here","Max.40.char","file")

lib1$Variable<-toupper(lib1$Variable)
lib1$Enter.label.here<-as.character(lib1$Enter.label.here)
lib1$labelsize<-nchar(lib1$Enter.label.here)
lib1$ISSUE<-""
lib1$ISSUE<-ifelse(nchar(lib1$Variable)>8,"VarName>8",lib1$ISSUE)
lib1$ISSUE<-ifelse(nchar(lib1$Enter.label.here)>40,paste0(lib1$ISSUE,"; ","Label>40"),lib1$ISSUE)
lib1$ISSUE<-ifelse(is.na(lib1$Enter.label.here),paste0(lib1$ISSUE,"; Missing label"),lib1$ISSUE)
lib1$RESOLUTION<-""
lib1$Max.40.char<-NULL
#lib2<-lib1[nchar(lib1$Variable)>8,]

#  lib3<- lib1[grepl(".", lib1$Variable,
#                    fixed = T),]
if(length(lib1$ISSUE[lib1$ISSUE!=""])>0){
    #lib2<-rbind(lib2,lib3)
    #lib2$change.name<-""
    #lib2<-nodup(lib2,names(lib2),"all")
    #write.csv(lib2[,c("Variable","file","labelsize","change.name")],paste(pathwork,"Var_name_GT8.csv",sep="/"),row.names=F)
    print("WARNING:")
    print(paste(length(lib1$ISSUE[lib1$ISSUE!=""]),"issues encountered. Please fix them in studydefinelist.csv then have it QC'd before running step4"))
}else{print("Everything looks fine. The QC of the define list may be performed prior to running Step3")}
write.csv(nodup(lib1,c("Variable","file"),"all"),paste(pathwork,"studydefinelist.csv",sep="/"),row.names=F)
}

#' Update library
#'
#' Optional used function.
#'
#' @keywords definelist
#' @export
#' @examples
#' create.library()

lib.update<-function(prevlib="//certara.com/sites/S02-Cary/Consulting/Projects/ras/Nath team/library/xptlibrary.csv",newlib="studydefinelist.csv"){
  keep<-c("Variable","Unit","Detailed.description","Enter.label.here")
  if(!is.null(prevlib)){
  pl<-read.csv(prevlib)
  lib<-read.csv(newlib)
  pl1<-rbind(lib[,intersect(names(pl),names(lib))],pl[,intersect(names(pl),names(lib))])
  pl1<-pl1[!duplicated(pl1$Variable),]
  write.csv(pl1,prevlib)
  write.csv(pl1,paste0(prevlib,"_",Sys.Date(),".csv"))
  }else{
    pl1<-read.csv(newlib)[,keep]
    pl1<-pl1[!duplicated(pl1$Variable),]
    write.csv(pl1,"xptlibrary.csv")
    write.csv(pl1,paste0("xptlibrary.csv","_",Sys.Date(),".csv"))
    }
  }


#####
######
#>>>>project title>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

###FILE NAMES TO BE CONVERTED (Required as well as the order to be displayed in define document)

#' generateXPT
#'
#' internal used function.
#'
#' @keywords generateXPT
#' @export
#' @examples
#' generateXPT()

generateXPT<-function(range.character=NULL){

read.csv.custom <- function(filename) {
    x <- read.csv( filename, stringsAsFactors=F )
    names(x) <- gsub( x=names(x), pattern="[.]", replacement="_" )
    return(x)
  }
setwd(working.folder)
  pathwork<-getwd()
  pathdir<-pathwork
  load.pack1()

  input<-paste0(pathwork,"/input")
  progdir <- "./output/programs"# This folder should contain all program and output .txt
  outputdir<-paste0(pathdir,"/output/datasets") # This folder for resulted output

  checkclass=class#NULL or "auto"

  lst<- read.csv(paste(pathwork,"list of files.csv",sep="/"),stringsAsFactors=F)
  
  lst$type[lst$type=="txt"]<-"program"
  
  #lst<-lst[lst$xptconvert==1,]
  head(lst)
  csv<-lst[lst$type%in%c("xpt","csv"),]
  lstype<-csv$type
  dir<-unlist(paste0(csv$rename,".csv"))
  oriname<-unlist(paste0(csv$filename))
  description<-unlist(paste0(csv$description))
  key<-unlist(paste0(csv$keyvar))

  ##PROGRAMS TABLE
  prog<-unlist(paste0(lst$renam[lst$type=="txt"],".txt"))
  progdes<-unlist(paste0(lst$renam[lst$description=="program"]))
  ####### NOT TO BE EDITED ##################################
  location<-paste0("#programs#",prog)
  ###############################
  labdir<-gsub(".csv","",dir)#
  #labdir<-gsub(".xpt","",labdir)#
  outdir<-dir#

  inp<-data.frame(
    lstype=lstype,
    input=dir,
    lab=labdir,#
    outp=sub(".csv","",outdir),
    prog="NA")#
  
  dataset<-lst[lst$type%in%c("xpt","csv"),]
  inp1<-data.frame(
    Dataset=paste0(dataset$rename,".",dataset$type),
    Orinalname=oriname,
    Description=description,#sub(".csv","",dir),  # Label for xpt output, may be required by FDA
    Keyvariables=key,
    Datasetfullname=dir)

  ####### NOT TO BE EDITED ##################################
  #setwd(outputdir)#
  ######################################################################
  # Convetert csv to XPT
  #############################################
  #detail<- read.xls(paste(sourcepath,definelib,sep="/"), sheet = 1)
definelib<-"studydefinelist.csv"
  detail1<- read.csv(paste(pathwork,definelib,sep="/"),stringsAsFactors=F)
  detail<-detail1
  #numkeep<-detail$Variable[detail$Numflg==1]
  detail$"Variable"<-as.character(detail$"Variable")
  detail$Enter.label.here<-capitalize(as.character(detail$Enter.label.here))
  detail$SAS.label<-capitalize(as.character(detail$Enter.label.here))
  subfold<-unlist(paste0(csv$subfolder))
  ################## START LOOP #################
for (j in 1:nrow(inp)){

if(agency=="PMDA"){
    outputdir<-paste0(pathdir,"/output/datasets/",subfold[j])
    dir.create(outputdir)
  }else{outputdir<-paste0(pathdir,"/output/datasets")} # This folder for resluted output
  
  
require(SASxport)
    #numkeep<-detail$Variable[detail$Numflg==1]
  #read.csv.custom(paste(pathwork,definelib,sep="/"))
    detail<- read.csv(paste(pathwork,definelib,sep="/"),stringsAsFactors=F)
    detail<-detail[detail$file==as.character(inp$input[j]),]
    detail$"Variable"<-as.character(detail$"Variable")
    detail$Enter.label.here<-capitalize(as.character(detail$Enter.label.here))
    detail$SAS.label<-capitalize(as.character(detail$Enter.label.here))
    detail$"Detailed.description"[nchar(detail$Variable)>8]<-paste0(detail$"Detailed.description"[nchar(detail$Variable)>8],". Note: original variable name = ",detail$Variable[nchar(detail$Variable)>8],".")

    pkdata <- read.csv(paste(input,inp$input[j],sep="/"),stringsAsFactors = FALSE)
    pkdata <-read.csv.custom(paste(input,inp$input[j],sep="/"))
    pkdata<-chclass(pkdata,names(pkdata),"char")
    names(pkdata)<-toupper(names(pkdata))
    pkdata<-chclass(pkdata,names(pkdata),"char")

    if(!is.null(checkclass)){
      pkdata<-autoclass(pkdata)
    }
##APPLY CHANGES
    #setwd(working.folder)
lib<-detail
    lib2<-chclass(lib,names(lib),"char")
    lib2$RESOLUTION[is.na(lib$RESOLUTION)|lib$RESOLUTION==""]<-"unchanged"
    #lib$dum<-paste(lib$Variable,lib$file,sep="-")
    #lib2$dum<-paste(lib2$Variable,lib2$file,sep="-")
    rem<-lib2[lib2$RESOLUTION=="remove",]
    unch<-lib2[lib2$RESOLUTION=="unchanged",]
    rnm<-lib2[!lib2$RESOLUTION%in%c("unchanged","remove"),]

    lib3<-lib[!lib$Variable%in%rem$Variable,]
    lib3$Variable[lib3$Variable%in%rnm$Variable]<-rnm$RESOLUTION

    #for(i in unique(lib2$file)){
      dc<-pkdata#read.csv(paste0("./input/",i))
      rem1<-rem
      rnm1<-rnm
      names(dc)<-toupper(names(dc))
      ind<-!names(dc)%in%rem1$Variable
      dc<-dc[,ind]
      names(dc)[names(dc)%in%rnm1$Variable]<-rnm1$RESOLUTION
    #write.csv(dc,paste0("./input/",i),row.names=F)}
    #write.csv(lib3,"studydefinelist.csv",row.names=F)
###############
range.fun<-function(data,var){
if(is.numeric(data[,var])){rg=range(data[,var],na.rm=T)
rg<-paste0(round(rg[1],3),"-",round(rg[1],3))
}else{
if(!is.null(range.character)){
rg=unique(data[,var])[1:4]
rg=paste0(rg[1],", ",rg[2],", ",rg[3],", ",rg[4],",")
rg=gsub("NA,","",rg)}else{rg=""}
}
rg
}
    
if(inp$lstype[j]=="csv"){
  
write.csv(dc,file.path(outputdir,inp$input[j]),row.names = F)

#Define csv
detailcsv<-detail

rg1<-NULL
for(i in names(dc)){
rg1<-c(rg1,range.fun(data=dc,var=i))
}

list(str(d1))
length(unlist(lapply(dc,class)))
length(d1$Variable)

def<-with(detailcsv[detailcsv$RESOLUTION!="remove",],data.frame(Variable=Variable,SAS.label="",Type=unlist(lapply(dc,class)),
          Range=rg1,Unit=Unit,Detailed=Detailed.description))
row.names(def)<-NULL
head(def)
write.csv(def,file.path(outputdir,paste(inp$outp[j],"define.csv",sep="")),row.names=F)
}

if(inp$lstype[j]=="xpt"){
  require(SASxport)
  if(nrow(lib3[is.na(lib3$Enter.label.here),])>0) stop(list("Error due to missing label. Please update the define list.",lib3[is.na(lib3$Enter.label.here),c("Variable")]))
    detail<-lib3
    pkdata<-dc
    for (h in 1:nrow(detail)){
    tryCatch(label(pkdata[,paste(detail$Variable[h])])<- paste(detail$SAS.label[h]),error=function(e) NULL )
    }
    label(pkdata)<-inp$lab[j]
    rangepkdat <- vardefine(pkdata,maxlevel=10,labels=T,digits=6,vartype=T,exp.csv=T) # same as previous, to export to csv
    detail1<- detail #read.xls(paste(location,"/make defines library.xls",sep=""),sheet=1,method="tab")
    detail1<-detail1[!duplicated(detail1$Variable),]
    detail1<-detail1[detail1$Variable%in%unique(rangepkdat$Variable),]
    rangepkdat<-merge(rangepkdat,detail1[,c("Variable","Unit","Detailed.description")],by=c("Variable","Variable"),all.x=T,sort=F)
    names(rangepkdat)
    #File name should not exceed 8 characters- NOTE that all variable names will be limited to 8 characters
    name<-as.character(inp$outp[j])
    assign(name,pkdata)
    
    if(agency=="PMDA"){
      sf<-paste0(getwd(),"/output/datasets/",subfold[j])
      dir.create(sf)
      file1<-paste(sf,"/",inp$outp[j],".xpt",sep="")
      sav<-paste0("write.xport(",inp$outp[j],",file=file1,autogen.formats=FALSE)")
    }else{
      file1<-paste(outputdir,"/",inp$outp[j],".xpt",sep="")
      sav<-paste0("write.xport(",inp$outp[j],",file=file1,autogen.formats=FALSE)")}
  #setwd(outputdir)
    eval(parse(text=sav))

    require(SASxport)   # to be able to read the xport files
    data<-SASxport::read.xport(paste(outputdir,"/",inp$outp[j],".xpt",sep=""),name.chars=c("_","-","&","@"))
    definedataset <- data.frame(Variable=colnames(data))
    range<-vardefine(data,maxlevel=10,labels=T,digits=6,vartype=T,exp.csv=T)
    data1<-data.frame(var=colnames(data))
    
    rangepkdat$Type<-range$Type
    rangepkdat$Variable<-range$Variable
    rangepkdat$"Code/Range"<-range$"Code/Range"
    names(rangepkdat)<-c("SAS Variable","SAS Label","Type","Code/Range","Unit","Detailed.description")
    rangepkdat<-rangepkdat[,c("SAS Variable","SAS Label","Type","Code/Range","Unit","Detailed.description")]
    #rangepkdat$"SAS Variable"<-gsub(rangepkdat$"SAS Variable",pattern="\\_",replacement="XXXX")
    #rangepkdat$"SAS Variable"<- data1
    detach("package:SASxport", unload = T)
    
ind<-as.numeric(row.names(rangepkdat[rangepkdat$"Code/Range"=="-",]))
    if(length(ind[!is.na(ind)])>0){
      rgd<-rangepkdat
    for(i in 1:length(ind)){
        x<-ifelse(length(unique(data[,ind[i]]))>5,paste0(paste(as.character(unique(data[,ind[i]])[1:5]),collapse=", ",sep=""),", ..."),
        paste(as.character(unique(data[,ind[i]])),collapse=", ",sep=""))
        rgd[ind[i],"Code/Range"]<-x}}else{rgd<-rangepkdat}
    if(!is.null(range.character)){
      write.csv(rgd,file.path(outputdir,paste(inp$outp[j],"define.csv",sep="")),row.names=F)}else{
        write.csv(rangepkdat,file.path(outputdir,paste(inp$outp[j],"define.csv",sep="")),row.names=F)
      }
    }
}
  setwd(working.folder)  
getwd()

}
  

  #END LOOP #

#END of function

######################
# OUTPUT DEFINE
######################
#setwd(sourcepath)
#source("./functions/html export.R")
#source("./functions/label fun.R")


#' generateDEF1
#'
#' internal used function.
#'
#' @keywords generateDEF1
#' @export
#' @examples
#' generateDEF1()
generateDEF1<-function (title = "Add title here", 
                        xpt.location = ".\\",
                        prog.location = "\\programs\\", 
                        define.location = "\\datasets\\")
{

  lapply(paste("package:", names(sessionInfo()$otherPkgs), sep=""), 
         detach, 
         character.only = TRUE, 
         unload = TRUE)
  
  
  library(officer)
  library(flextable)

  ##FUNCTION
 
  multi_hyperlink_text<-function(labels, links){
    out <- mapply(
      function(text, url){
        dat <- hyperlink_text(text, url = url)
        dat <- split(dat, seq_along(text))
        as_paragraph(list_values = dat)
      },
      text = strsplit(labels, split = ","),
      url = strsplit(links, split = ","), SIMPLIFY = FALSE, USE.NAMES = FALSE
          )
    # the following is necessary to comply with expected
    # flextable structure!
  Reduce(append, out)
  }

  ###############
  setwd(working.folder)
  pathwork <- getwd()
  pathdir <- pathwork
  
  #sourcepath <- paste0(pathdir, "/functions")
  
  input <- paste0(pathdir, "/input")
  progdir <- paste0(pathdir, "/output/programs")
  outputdir <- paste0(pathdir, "/output/datasets")
  
  lst <- read.csv(paste(pathwork, "list of files.csv",
                        sep = "/"), stringsAsFactors = F)
  head(lst)
  
  lst$type[!lst$type%in%c("csv","xpt")] <- "program"
  csv <- lst[lst$type%in%c("csv","xpt"), ]
  dir <- unlist(paste0(csv$rename,".",csv$type))
  
  typefile<-unlist(paste0(csv$rename,".",csv$type))
    
  oriname <- unlist(paste0(csv$filename))
  description <- unlist(paste0(csv$description))
  key <- unlist(paste0(csv$keyvar))
  struct <- unlist(paste0(csv$Structure))
  usedprog <- unlist(paste0(csv$Program))
  
  labdir <- csv$rename#gsub(".csv", "", dir)
  outdir <- dir
  
  inp <- data.frame(input = dir, lab = labdir, outp = sub(".csv",
                                                          "", outdir), prog = "NA")
  #
  
  if(agency=="PMDA"){
    inp1 <- data.frame(Dataset =typefile, `Original Name` = oriname,
                       Description = description, Keyvariables = key, 
                       Datasetfullname = paste0(xpt.location,lst$subfolder[lst$type%in%c("csv","xpt")],"\\", 
                        outdir))}else{
  inp1 <- data.frame(Dataset =typefile, `Original Name` = oriname,
                     Description = description, Keyvariables = key, 
                     Datasetfullname = paste0("\\datasets\\", 
                                              outdir))}
  
  inp1$Original.Name <- gsub(inp1$Original.Name, pattern = "\\_",
                             replacement = "_")
  
  chclass <- function(data, var, class = "char") {
    for (i in var) {
      if (class == "num") {
        data[, i] <- as.numeric(as.character(data[, i]))
      }      else {
        data[, i] <- as.character(data[, i])
      }
    }
    data
  }

inp1 <- chclass(inp1, names(inp1), "char")
  tbname <- "Define"
  maintit <- data.frame(tit = c("TABLE OF CONTENTS",
                                "DATASETS TABLE OF CONTENTS", "VARIABLE DEFINITION TABLES",
                                "PROGRAMS TABLE OF CONTENTS"))

tabn <- c("Dataset", "Original Name", "Description",
            "Key Variables", "Location")
  hyp0 <- inp1[, "Datasetfullname"]
  inp1[, "Datasetfullname"] <- ""

if(agency=="PMDA"){
      data.location=paste0(xpt.location,lst$subfolder[lst$type%in%c("csv","xpt")],"\\")}else{
        data.location=paste0(xpt.location)}  
  
  hyp1 <- outdir#paste0(sub(".csv", "", outdir), ".xpt")
  loc1 <- paste0(data.location,hyp1)

names(inp1)<-tabn
  inp1$Location<-seq(length(loc1))

inp1$A<-hyp0
inp1$link<-loc1

##DATA LIST TABLE    
  ft <- flextable(data = inp1)
  ft <- compose(x = ft, j=5, value = multi_hyperlink_text(hyp0,loc1))
  ft <- color(x = ft, j =5,color = "#0000EE")
  ft <- void(ft, ~A+link)
  ft <- void(ft, ~A+link,part="header")
  ft<-bold(ft,part="header")
  ft<-merge_at(ft,j=5:7,part="header")

for(i in 1:nrow(inp1)){
  ft<-merge_at(ft,j=5:7,i=i)
  }

  ft<-border_outer(ft)
  ft<-border_inner(ft)
  ft<-font(ft, fontname = "Times New Roman", part = "all")
  ft<-set_table_properties(ft, width = .7, layout = "autofit")

  tabxx<-NULL
  tabxx[[1]]<-ft
  ft<-NULL

  #doc<-read_docx()
  #START LOOP FOR VARIABLE DEFINE LIST
tab1data <- inp
  
##PREPARE DEFINE TABLE
subdir<-lst$subfolder[lst$type%in%c("csv","xpt")]
if(agency=="PMDA"){
  outputdir1<-paste0(outputdir,"/",subdir)
}else{outputdir1<-rep(outputdir,nrow(inp))
}

tabdatset<-NULL
for (j in 1:nrow(inp)) {
    t1<-as.character(tab1data$outp[j])
    
    data <- read.csv(file.path(outputdir1[j],paste(inp$lab[j],
                      "define.csv", sep = "")))
    
    data[,1] <- toupper(data[,1])
    head(data)
  tw1a <- as.data.frame(matrix(ncol = length(names(data)),
                         nrow = 5))
    tw1a[1:5, 1] <- c(as.character(inp1$Description[j]),
                      "Name of original version: ", "Structure: ",
                      "Dataset: ", "Program: ")
    tw1a[,2:6] <- ""
    names(tw1a) <- names(data)
    
  tw1bn <- c("Variable", "Label", "Type",
           "Code Range", "Unit", "Detailed Description")
  
  
  names(tw1a) <- names(data)
    tw1b <- as.data.frame(matrix(ncol = length(names(data)),
                                 nrow = 1, data =tw1bn))
    names(tw1b) <- names(data)
    tw2 <- rbind(tw1a, tw1b, data)
    names(tw2)
    #------->header
    dts1 <-as.character(inp1$Dataset[j]) #gsub(".xpt","",as.character(inp1$Dataset[j]))
    orin= as.character(inp1$"Original Name"[j])
    struc= as.character(struct[j])
    dts2 <- as.character(inp1$Dataset[j])
    link1 <- as.character(inp1$link[j])#color = "#0000EE"
    no_prog<-usedprog[j] == "NA" | usedprog[j] == ""
  if (no_prog) {
      prog = as.character(usedprog[j])
      link2 <-as.character(usedprog[j])
    } else {
      prog <- as.character(paste0(usedprog[j], ".txt"))
      link2 <- paste0(prog.location,as.character(paste0(usedprog[j], ".txt")))
    }

    names(tw2)
    #------->header
tw2$B<-"."
tw2$C<-"."
    head(tw2)
names(tw2)[2]<-"SAS.Label"    
    tw2$B[4:5]<-c(dts1,"")
    tw2$C[4:5]<-c(link1,"")
    tw2$SAS.Label[c(1:3)]<-c("",orin,struc)
    tw2$SAS.Label[c(4:5)]<-c(1,"")
    head(tw2)

if (!no_prog) {
      tw2[2,5]<-2
      tw2$B[5]<-c(prog)
      tw2$C[5]<-c(link2)
    }

    tw2$B<-as.character(tw2$B)
    tw2$C<-as.character(tw2$C)
head(data.frame(tw2))
length(tw2$SAS.Variable)
length(tw2$Detailed.description)
names(tw2)
#tw2$B<-NULL
#tw2$C<-NULL
head(tw2)
ft1<- flextable(data =tw2)
    if(no_prog){
      ft1 <- compose(x = ft1, j=2,i=4, value = multi_hyperlink_text(B,C))
      ft1 <- color(x = ft1,i=4, j =2,color = "#0000EE")
    }else{
      ft1 <- compose(x = ft1, j=2,i=4:5, value = multi_hyperlink_text(B,C))
      ft1 <- color(x = ft1,i=4:5, j =2,color = "#0000EE")
    }

    ft1<-bold(ft1,i=1,j=1:2)
    ft1<-bold(ft1,i=6)
    ft1<-bold(ft1,i=4,j=2)
    ft1<-bold(ft1,i=5,j=2)
    ft1 <- delete_part(x = ft1, part = "header")
    ft1 <- void(x = ft1,~B+C, part = "body")
    for(i in 2:5){
      ft1<-merge_at(ft1,j=2:7,i=i)
    }

    ft1<-merge_at(ft1,j=1:8,i=1)
    ft1<-merge_at(ft1,j=2:8,i=2)
    ft1<-merge_at(ft1,j=2:8,i=3)
    ft1<-merge_at(ft1,j=2:8,i=4)
    ft1<-merge_at(ft1,j=2:8,i=5)

    lr<-nrow(data)+6
    for(i in 6:lr){
      ft1<-merge_at(ft1,j=6:8,i=i)
    }
    bord<-fp_border(color="black")
    #ft1<-border(ft1,border.right=bord)
    ft1<-border_outer(ft1,border=bord)
    #ft1<-border_outer(ft1,i=2:4,j=1:2,border=bord)
    ft1<-hline(ft1,i=5,border=bord)
    ft1<-vline(ft1,i=2:5,j=2,border=bord)
    ft1<-vline(ft1,i=1,j=1,border=bord)
    ft1<-vline(ft1,j=1:6, i=6:lr,border=bord)
    ft1<-hline(ft1, i=6:lr,border=bord)
    ft1<-font(ft1, fontname = "Times New Roman", part = "all")
    #ft1<-set_table_properties(ft1, width = .7, layout = "autofit")
    tabdatset[[j]]<-ft1
    ft1<-NULL
  }

###PREPARE PROGRAMS LIST
prog <- unlist(paste0(lst$renam[lst$type == "program"],
              ".txt"))
  progdes <- unlist(paste0(lst$description[lst$type == "program"]))
  origprog <- unlist(paste0(lst$filename[!lst$type%in%c("csv","xpt")]))

  #location <- paste0("\\programs\\", prog)
  dir(progdir)
  include <- paste0(lst$rename[lst$progNo != "" & !is.na(lst$progNo)],
                    ".txt")
  IOD <- lst[lst$progNo %in% lst$progNo[lst$progNo != "" &
        !is.na(lst$progNo)] & !is.na(lst$progNo), ]
  ind <- lst[rownames(lst) %in% rownames(IOD), ]
  prono <- lst[rownames(lst) %in% rownames(IOD), "progNo"]
  
#FOR FDA
if (agency!="PMDA") {
  subdir<-lst$subfolder[lst$type%in%c("program")]
  #if(agency=){
  # location<-paste0(prog.location,subdir)
  location <- paste0(prog.location,prog)#else{location <- paste0(prog.location,prog)} 
   
if(length(origprog)!=0){
  
    tab3data <- data.frame(Original = origprog, Program = prog,
                           Description = progdes, Location = location)

    hyp2 <- paste0("..",tab3data[, "Location"])
   
    hyp11 <- hyp0#paste0(prog.location, hyp1[, "Program"])
   
    tab3data$D<-tab3data[, "Location"]#gsub("..","",hyp0)#"test"#hyp0
    
    tab3data$E<-hyp2#hypmod
    
    ft2 <- flextable(data = tab3data)
    ft2 <- compose(x=ft2,j=4,value = multi_hyperlink_text(D,E))
    
    ft2 <- color(x = ft2,j =4,color = "#0000EE")
    ft2 <- void(x = ft2,~D+E, part = "all")
    for(i in 1:nrow(tab3data)){
      ft2<-merge_at(ft2,j=4:6,i=i)
    }
    ft2<-merge_at(ft2,j=4:6,part="header")
    ft2<-bold(ft2,part="header")
    bord<-fp_border(color="black")
    #ft1<-border(ft1,border.right=bord)
    ft2<-border_outer(ft2,border=bord)
    ft2<-border_inner(ft2,border=bord)
    bord<-fp_border(color="black")
    ft2<-vline(ft2,j=4,border=bord)
    ft2<-font(ft2, fontname = "Times New Roman", part = "all")
    ft2<-set_table_properties(ft2)
    }else{tab3data <- data.frame(Original = "origprog", Program = "prog",
                                 Description = "progdes", Location = "location")
    ft2 <- flextable(data = tab3data)}
    ft2<-set_table_properties(ft2) 
}
#FOR PMDA
if (agency=="PMDA") {
  lstxt<-lst[lst$type=="program",]
    prog <- unlist(paste0(ind$renam[ind$type=="program"],".txt"))
    progdes <- unlist(paste0(ind$Purpose[ind$type=="program"]))
    origprog <- unlist(paste0(ind$filename[ind$type=="program"]))
    software.used <- unlist(paste0(ind$Software.version[ind$type=="program"]))
    tab3data <- data.frame(Program_Original_names = "",
                           Description = "", Input_Output_log_file_original_names = "")
    tab<-as.data.frame(matrix(ncol=7,nrow=length(origprog)))
    names(tab)<-c("Program#Original names","Description#Purpose","Input#Output#log file#original_names","A","B","C","D")
    subfolder<-lst$subfolder[lst$type=="program"]
    #tab = FlexTable(data=tab3data, header.columns = T)

table1<-NULL        
for (i in 1:length(origprog)) {
  tab1<-tab[1,]
  tab2<-tab[1,]
      op <- paste0(".\\",subfolder,"\\", prog[i])
      space = "\n  "
      tab1[,1] = prog[i]#paste(prog[i], "\n(original:",
                       #rigprog[i],")")
      tab1[,"A"]<-prog[i]
      tab1[,"B"]<-paste0(".\\",lstxt$subfolder[i],"\\",prog[i])
      tab1[,2]<-paste0("Purpose:\n",lstxt$description[i])
      tab2[,1]<-paste0("\n(original:",
                  origprog[i],")")
      tab2[,2]<-paste0("Software Version:\n",lstxt$Software.version[i])
      #tab[i,2] = paste("Software used:\n ",software.used[i],"\n\nPurpose:\n ", progdes[i])
      lst$type[lst$type=="program"]<-"txt"
      io1 <- lst[grep(prono[i],lst$proNo.input), ]
      io2 <- lst[grep(prono[i],lst$proNo.output), ]
      io3 <- lst[grep(prono[i],lst$progNo.dependent),]
      tab1<-rbind(tab1,tab2)
           
if (nrow(io1) == 0) {
  tab3<-tab[i,]
  tab3[,3] <- "[INPUT]"
      } else {
        tab3<-tab[i,]
        tab3[,3] <- "[INPUT]"
        
        for (io in 1:nrow(io1)) {
          ext1 <- ifelse(io1$type[io] == "program",
                         ".txt", ".xpt")
          txtins <- paste0(io1$rename[io], ext1)

          ext2 <- ifelse(io1$type[io] == "program",
                         "./programs/", "./datasets/")
          insert <- paste0(ext2, txtins)
          orf <- paste0("(original:",io1$filename[io],
                        ")")
          col = "#0000EE"
          space = "\n  "
          
          fn<-paste0(io1$rename[io],".",io2$type[io])
          tab4<-tab[i,]
          tab4[,"C"]<-fn
          if(io1$type[i]%in%c("csv","xpt")){
          tab4[,"D"]<-paste0("..\\datasets\\",io1$subfolder[io],"\\",fn)}else{
          tab4[,"D"]<-paste0(".\\",io1$subfolder[io],"\\",fn) 
          }
          tab4a<-tab[i,]
          tab4a[,3]<-orf
          tab3<-rbind(tab3,tab4,tab4a)
        }
      }
if (nrow(io2) == 0) {
        tab4<-tab[i,]
        tab4[,3] <- "[OUTPUT]"
      } else {
        tab4<-tab[i,]
        tab4[,3] <- "[OUTPUT]"
        
        for (io in 1:nrow(io2)) {
          ext1 <- ifelse(io2$type[io] == "program",
                         ".txt", ".xpt")
          txtins <- paste0(io2$rename[io], ext1)
          
          ext2 <- ifelse(io2$type[io] == "program",
                         "./programs/", "./datasets/")
          insert <- paste0(ext2, txtins)
          orf <- paste0("(original:",io2$filename[io],
                        ")")
          col = "#0000EE"
          space = "\n  "
          
          fn<-paste0(io2$rename[io],".",io2$type[io])
          tab4b<-tab[i,]
          tab4b[,"C"]<-fn
          if(io2$type[i]%in%c("csv","xpt")){
            tab4b[,"D"]<-paste0("..\\datasets\\",io2$subfolder[io],"\\",fn)}else{
              tab4b[,"D"]<-paste0(".\\",io2$subfolder[io],"\\",fn) 
            }
          tab4c<-tab[i,]
          tab4c[,3]<-orf
          tab4<-rbind(tab4,tab4b,tab4c)
        }
      }
           
 if (nrow(io3) == 0) {
        tab5<-tab[i,]
        tab5[,3] <- "[DEPENDENCY]"
      } else {
        tab5<-tab[i,]
        tab5[,3] <- "[DEPENDENCY]"
        
        for (io in 1:nrow(io3)) {
          ext1 <- ifelse(io3$type[io] == "program",
                         ".txt", ".xpt")
          txtins <- paste0(io3$rename[io], ext1)
          
          ext2 <- ifelse(io3$type[io] == "program",
                         "./programs/", "./datasets/")
          insert <- paste0(ext2, txtins)
          orf <- paste0("(original:",io3$filename[io],
                        ")")
          col = "#0000EE"
          space = "\n  "
          
          fn<-paste0(io3$rename[io],".",io3$type[io])
          tab4b<-tab[i,]
          tab4b[,"C"]<-fn
          if(io3$type[i]%in%c("csv","xpt")){
            tab4b[,"D"]<-paste0("..\\datasets\\",io3$subfolder[io],"\\",fn)}else{
              tab4b[,"D"]<-paste0(".\\",io3$subfolder[io],"\\",fn) 
            }
          tab4c<-tab[i,]
          tab4c[,3]<-orf
          tab5<-rbind(tab5,tab4b,tab4c)
        }
      }
table1<-rbind(table1,tab1,tab3,tab4,tab5)
}

table1$rn<-seq(nrow(table1))
a<-as.numeric(table1$rn[table1$A!=""])
a<-a[!is.na(a)]
b<-as.numeric(table1$rn[table1$C!=""])
b<-b[!is.na(b)]

table1$rn1<-ifelse(table1$A=="","NA",table1$A)
c<-as.numeric(table1$rn[!is.na(table1$rn1)])-1

c2<-as.numeric(table1$rn[!is.na(table1$rn1)])-1
c1<-as.numeric(table1$rn[!is.na(table1$rn1)])
lastline<-as.numeric(nrow(table1))
c2<-c(c2[-1],lastline)

pm <- flextable(data =table1)
pm <- compose(x=pm,j=1,i=a,value = multi_hyperlink_text(A,B))
pm <- compose(x=pm,j=3,i=b,value = multi_hyperlink_text(C,D))
pm <- color(x = pm,i=a, j =1,color = "#0000EE")
pm <- color(x = pm,i=a, j =1,color = "#0000EE")
pm <- color(x = pm,i=b, j =3,color = "#0000EE")
pm <- hline(x = pm,i=c[c!=0])
pm <- vline(x = pm)
pm<-border_outer(pm,border=bord)
#pm<-border_inner(pm,border=bord)
pm<-bold(pm,part="header")
pm<-font(pm, fontname = "Times New Roman", part = "all")
pm<-set_table_properties(pm, width = .8, layout = "autofit")
pm<-delete_columns(pm,4:9)
pm<-merge_v(pm,j=1:3, part = "body")
}
  
  
#PREPARE DEFINE WORD DOCUMENT
img_in_par <- fpar(
    #external_img(src ="c:/lhtemplate/logo.png",width=3.5,height=1.44),
  fp_p = fp_par(text.align = "center"))
  text_style <- fp_text(font.size =14,bold=T,font.family ="Times New Roman")
  text_style1 <- fp_text(font.size =12,bold=T,font.family ="Times New Roman")
  par_style <- fp_par(text.align = "center")

##Create WORD DOC
#DEFINE
doc<-read_docx()
  an_fpar <- fpar("", run_linebreak())
  doc <- body_add_fpar(doc,fpar(ftext(title, prop = text_style), fp_p = par_style ))
  doc <- body_add(doc, an_fpar)
  doc <-body_add_break(doc)
  t1<-fpar(ftext("TABLE OF CONTENTS", prop = text_style1))
  #doc <- body_add_par(doc,"DATASETS TABLE OF CONTENTS",style = c("heading 1"))
  doc <- body_add_fpar(doc,t1,style = c("heading 1"))
  doc <- body_add_toc(doc,level=2)
  doc <-body_add_break(doc)
  t1<-fpar(ftext("DATASETS TABLE OF CONTENTS", prop = text_style))
  #doc <- body_add_par(doc,"DATASETS TABLE OF CONTENTS",style = c("heading 1"))
  doc <- body_add_fpar(doc,t1,style = c("heading 1"))
  doc <- body_add(doc, an_fpar)
  tabx1<-tabxx[[1]]
  tabx1<-autofit(tabx1)
  doc <- body_add_flextable(doc,tabx1,align="center",split=T)
  doc <-body_add_break(doc)
  #fname<-paste0(define.location,"define.docx")
 # print(doc,fname)
  #doc<-read_docx("c:/lhtemplate/style.docx")
  t1<-fpar(ftext("VARIABLE DEFINITION TABLES", prop = text_style))
  #doc <- body_add_par(doc,"VARIABLE DEFINITION TABLES",style = c("heading 1"),font.family ="Times New Roman")
  doc <- body_add_fpar(doc,t1,style = c("heading 1"))
  #doc <- body_add(doc, an_fpar)
  
for(i in 1:nrow(inp)){
  doc <-body_add_break(doc)
    t1<-as.character(tab1data$outp[i])
    t1<-fpar(ftext(t1, prop = text_style1))
    #doc <- body_add_par(doc,t1,style = c("heading 2"))
    doc <- body_add_fpar(doc,t1,style = c("heading 2"))
   # doc <- body_add(doc, an_fpar)
    tabx<-tabdatset[[i]]
    #tabx<-autofit(tabx)
    doc <- body_add_flextable(doc,tabx)
    #doc1 <-body_add_break(doc)
  }

#fname<-paste0(".\\output",define.location,"define.docx")
#print(doc,fname)
  
if(agency!="PMDA"){
    an_fpar <- fpar("", run_linebreak())
    doc <- body_add_fpar(doc,fpar(ftext(title, prop = text_style), fp_p = par_style ))
    doc <- body_add(doc, an_fpar)
    doc <-body_add_break(doc)
   # doc<-read_docx()
    t1<-fpar(ftext("PROGRAMS TABLE OF CONTENTS", prop = text_style1))
    #doc <- body_add_par(doc,t1,style = c("heading 2"))
    doc <- body_add_fpar(doc,t1,style = c("heading 1"))
    #doc <- body_add_par(doc,"PROGRAMS TABLE OF CONTENTS",style = c("heading 1"))
    doc <- body_add(doc, an_fpar)
    ft2<-autofit(ft2)
    doc2 <- body_add_flextable(doc,ft2,align="center",split=T)
    doc2<-body_end_section_landscape(doc2, w = 21/2.54, h = 29.7/2.54)
    fname<-paste0(".\\output",define.location,"define.docx")
    print(doc2,fname)
    }

#fname<-paste0(".\\output\\datasets\\define.docx")
#print(doc1,fname)
  
  
if(agency=="PMDA"){
    doc3<-read_docx()
    doc3 <- body_add_par(doc3,"PROGRAMS TABLE OF CONTENTS",style = c("heading 1"))
    doc3 <- body_add(doc3, an_fpar)
    pm<-autofit(pm) 
    doc3 <- body_add_flextable(doc3,pm)
fname<-paste0(".\\output\\datasets\\","define.docx")
doc<-body_end_section_landscape(doc, w = 21/2.54, h = 29.7/2.54)
print(doc,fname)

fname<-paste0(".\\output\\programs\\program_procedure.docx")
doc3<-body_end_section_landscape(doc3, w = 21/2.54, h = 29.7/2.54)
print(doc3,fname)
}
}


#' clearALL
#'
#' Internal use.
#' @keywords clearALL
#' @export
#' @examples
#' clearALL()

clearALL<-function(...){
  pathwork<-getwd()
  pathdir<-pathwork
  #sourcepath<-paste0(pathdir,"/functions") #This folder should contain sources (html export.R and label fun.R), libray excel fil (ex: label libraryAUSPECT.xls)
  input<-paste0(pathdir,"/input")# This folder should contain all csv data
  progdir <- paste0(pathdir,"/output/programs")# This folder should contain all program and output .txt
  outputdir<-paste0(pathdir,"/output/datasets") # This folder for resluted output
  frm<-dir(progdir)
  for(i in frm){
    file.remove(paste(progdir,i,sep="/"))
  }
  frm<-dir(input)
  for(i in frm){
    file.remove(paste(input,i,sep="/"))
  }
  frm<-dir(outputdir)
  for(i in frm){
    file.remove(paste(outputdir,i,sep="/"))
  }
  #file.remove(paste(sourcepath,"tmp modlab.csv",sep="/"))
}

#' cleardefCSV1
#'
#' Internal use.
#' @keywords cleardefCSV1
#' @export
#' @examples
#' cleardefCSV1()

cleardefCSV1<-function(...){
  pathwork<-getwd()
  pathdir<-pathwork
  dir(pathwork)
  lst<-read.csv(paste0(pathdir,"/","list of files.csv"))
  if(agency=="PMDA"){
    for(i in 1:length(lst$subfolder)){
      outputdir<- paste0(pathdir,"/output/datasets/",lst$subfolder[i])
      frm<-dir(paste0(pathdir,"/output/datasets/",lst$subfolder[i]))
      index<-grep("define.csv",frm)
      frm<-frm[index]
      for(j in frm){
        file.remove(paste(outputdir,j,sep="/"))
      }}
  }else{
    outputdir<- paste0(pathdir,"/output/datasets/")
    frm<-dir(paste0(pathdir,"/output/datasets/"))
    index<-grep("define.csv",frm)
    frm<-frm[index]
    for(j in frm){
      file.remove(paste(outputdir,j,sep="/"))
    }
  }
  }


#' helps
#'
#' Packages required: "SASxport", "reshape", "Hmisc", "tidyr","ReporteRs","plyr".
#' Run each function in the order listed below. Steps 1, 2 and 3, will provide
#' additional settings in excel spreadsheet. PLEASE COMPLETE THE SETTINGS IN EACH SPREADSHEET BEFORE RUNNING NEXT STEP
#' #'
#' @param install.pack first time package user, run this function to install all required packages.
#' @param working.folder Mandatory. Define the main working path or folder (ex: working.folder<-"C:/Users/lpheng/Desktop/xptdefine").
#' @param define.library Mandatory. If library not available, type define.library<- "no". If library is available, copy define library file to main working folder and set
#'                     define.library<-"(library file name).csv"
#' @param step1() funtion create "list of files.csv" spreadsheet if not exist.?step1 for details
#' @param step2() create subfolders, import files in the "list of files.csv" to input folder and create studydefinelist.csv.
#' Note that a WARNING message may appear if one or more labels are longer than 8 characters. These labels will be reported in an Excel
#' file called Var_name_GT8.csv. You can make changes to the label to get appropriate length.
#' Note also that you can flag variables for remove in "list of files.csv". If you want to make changes to your data, run step3().
#' @param step3()  This step is optional. Run this option to apply the changes to label name or lable removal.
#' @param step4()  Convert csv to xpt and create define document
#' @keywords helps
#' @export
#' @examples
#' Copy these lines below to script editor
#' library(xptdef)
#' working.folder<-"C:/Users/lpheng/Desktop/packdev/test" #set the working path
#' step1(working.folder)
#' define.library="no" # or enter the name of the library
#' step2()
#' title="Test" # Mandatory. this title will appear in define
#' step4(title)

helps<-function(...){
  x<-data.frame(stepBYstep=seq(8),
                Procedure=c("require(xptdef3)",
                            "working.folder= enter full path",
                            "agency=PMDA or FDA",
                            "step1()",
                            "EDIT list of files.csv and save (see html). Note: additional fields required to be completed for PMDA submission:
                            subfolder/progNo/Software.version/Purpose/proNo.input/proNo.output/progNo.dependent",
                            "define.library=no",
                            "step2()",
                            "EDIT studydefinelist.csv and save",
                            "step3()"))
  print(x)
}



#' step1
#'
#' step1 funtion will map the working folder and create excel spreadsheet file that needed to be
#' be completed before running step2.
#' @param working.folder mandatory. ex:working.folder<-"C:/Users/lpheng/Desktop/xptdefine".
#' @keywords step1
#' @export
#' @examples
#' step1(working.folder)

step1<-function(){
  setwd(working.folder)
  if(!"list of files.csv"%in%dir()){
    lf<-data.frame(
      filename=c("Enter original file name: ex: data.csv, pattab.tab, basemod.ctl,GOF.R, vpcoutput.tab, etc."),
      type=c("ASSIGNED file type to be converted. Ex: txt for program in text or csv or xpt for dataset (required define for each variable post STEP2()). 
             Note CSV is accepted. At STEP3(), the files will be converted to the assigned format"),
      rename=c("RENAME file (FDA, <=8 character PMDA, original names are acepted). Used in STEP2()"),
      keyvar=c("OPTIONAL: leave blank if txt or enter unique variables per row ex: ID,TIME"),
      Structure=c("OPTIONAL: leave blank if txt or enter unique variables per row ex: per subject per time point"),
                 
      Program=c("Leave blank if no progragram or script was used to generate the dataset. Else, enter the script or program name (renamed)"),
      description=c("ENTER DESCRIPTION ex: PK dataset, NONMEM residual output, NONMEM base model, GOF and VPC R script
                    VPC output, etc."),
      subfolder=c("ENTER the names of folder for each item. Required by PMDA. ex: 1_pk_data, 2_base_model,3_covariates, etc."),
      progNo=c("ENTER label or ID for each item. These flags will be used to map each item as input/output/or dependency.
               It should be unnested format, example:#A00,#A01,#B01,#A10. Nested labels will cause duplication or mislabeling. 
               Ex:#A1 and #A12 are nested"),
      Software.version=c("ENTER software version (Pheonix, R, NONMEM,etc.) for text model or control file or R script. 
                         Ex: NONMEM VERSION 7.4.3, R VERSION 3.0.0"),
      Purpose=c("ENTER the purpose of each program used to generate TFL for the study report. Ex: NONMEM control file of base model,
                R script for generating GOF and VPC plots for Figures 1 and 2 and Table 1 of the report (title?)"),
      proNo.input=c("MAP the input for each program with the progNo, EX: if pkdatset (progNo #A00) is the input of base model (progNo #A02) and 
                  GOF R script (progNo #A03), then enter #A02#A03 in the #A00 row "),
      proNo.output=c("MAP the output for each program with the progNo, EX: if basetab (progNo #A00) and vpctab (progNo #A01) are the outputs of base model (progNo #A02) and 
                  then enter #A00#A01 in the #A02 row"),
      progNo.dependent=c("MAP the dependency for each program with the progNo, EX: if lhtool2 (progNo #A00) is R script used in program to generate GOF figures (progNo #A02)
                         then enter #A00 in the #A02 row"),
      sourcepath=c("ENTER location of each item (full path from File explorer). 
                   At step2(), each file will be copied and converted to the assigned folders"))
    flextable::save_as_html(flextable::flextable(lf),path="./list_of_file_helps.html")

templ<-data.frame(filename= c("pd_twolast.csv","run9_Base_2CPT.mod","run9_Base_2CPT.lst","run9_Base_2CPT.tab",              
                      "run12_Base_2CPT_FULL_rm_AGEF.mod", "run12_Base_2CPT_FULL_rm_AGEF.lst", "run12_Base_2CPT_FULL_rm_AGVC.mod", "run12_Base_2CPT_FULL_rm_AGVC.lst",
                     "list_of_file_helps.html","pkdatbas.xpt"), 
                     type=c("csv", "txt", "txt", "csv", "txt", "txt", "txt" ,"txt" ,"csv" ,"csv"),
                     rename=c("erdata","baspkmod","baspklst","baspktab","run12_Base_2CPT_FULL_rm_AGEFmod",
"run12_Base_2CPT_FULL_rm_AGEFlst", "run12_Base_2CPT_FULL_rm_AGVCmod", "run12_Base_2CPT_FULL_rm_AGVClst", "testhtml","testsas" ),
keyvar=c("ID", "","","","","","","","ID", "ID"),
Structure=c("per subject", "","","","","","","","per subject", "per subject"),
Program=c("","","","","","","","","",""),
description=c("Exposure-Response dataset","NONMEM base PK model","NOMEM base model output",                       
           "Testing NONMEM table to csv","NONMEM covariate model - Age on fraction Frel","NONMEM covariate output - Age on fraction Frel",
"NONMEM covariate model - Age on Vc/F","NONMEM covariate output - Age on Vc/F","Testing conversion html to csv",                
"Testing conversion xpt to csv"),
subfolder=c("pkdata","basemod","basemod","basemod","covariat", "covariat", "covariat", "covariat", "covariat", "covariat"),
progNo=c("#A01 ", "#A02 ", "#A03 ", "#A10 ", "#A20 ", "#A30 ", "#10B ", "#C10 ", "#C01 ","#C02 "),
Software.version=c("","NOMEM version 7", "","","","","","","",""),
Purpose=c("","testing purpose" ,"testing purpose", "testing purpose" ,"testing purpose", "testing purpose" ,"testing purpose" ,"testing purpose", "testing purpose",
         "testing purpose"),
proNo.input=c("#A02#A20", "","","","","","","","",""),
proNo.output=c(rep("",8),"#A02#A20", "#A02#A20"),
progNo.dependent=c("","","","","","","#A30", "","",""),
sourcepath=rep("C:\\Users\\lpheng\\Certara\\PTCT-PMX-PTC923-4357 - Project Data\\",10))
write.csv(templ,"list of files.csv",row.names =F)
  }}


#' step2
#'
#' Create subfolders, import files in the list of files.csv to input folder and create studydefinelist.csv.
#' Note that a WARNING message may appear when one or more labels are longer than 8 characters. An excel file called Var_name_GT8.csv
#' will be generated for these labels. You can change the label to appropriate length. Note also that you can flag variables to be remove in the excel spreadsheets.
#' @keywords step2
#' @export
#' @examples
#' step2()
#'

step2<-function(){
  load.pack1()
  class="auto"
  importfiles()
  definelist()
}


# step3<-function(){
#   setwd(working.folder)
#   lib<-read.csv("studydefinelist.csv",stringsAsFactors=F)
#   filename = paste("./Backup/Original_studydefinelist-",format(Sys.time(), "%a-%b-%d-%H-%M-%S-%Y"),sep="")
#   write.csv(lib,paste0(filename,".csv"))
#   lib2<-read.csv("Var_name_GT8.csv",stringsAsFactors=F)
#   lib2<-chclass(lib2,names(lib2),"char")
#   lib2$change.name[is.na(lib2$change.name)|lib2$change.name==""]<-"unchanged"
#   lib$dum<-paste(lib$Variable,lib$file,sep="-")
#   lib2$dum<-paste(lib2$Variable,lib2$file,sep="-")
#
#   rem<-lib2[lib2$change.name=="remove",]
#   unch<-lib2[lib2$change.name=="unchanged",]
#   rnm<-lib2[!lib2$change.name%in%c("unchanged","remove"),]
#
# lib3<-lib[!lib$dum%in%rem$dum,]
# lib3$Variable[lib3$dum%in%rnm$dum]<-rnm$change.name[rnm$dum%in%lib3$dum]
#
# for(i in unique(lib2$file)){
#   dc<-read.csv(paste0("./input/",i))
#   rem1<-rem[rem$file==i,]
#   rnm1<-rnm[rnm$file==i,]
#   names(dc)<-toupper(names(dc))
#   ind<-!names(dc)%in%rem1$Variable
#   dc<-dc[,ind]
#   names(dc)[names(dc)%in%rnm1$Variable]<-rnm1$change.name
#     write.csv(dc,paste0("./input/",i),row.names=F)}
#     write.csv(lib3,"studydefinelist.csv",row.names=F)
#   }

#' Step 3
#'
#' Convert csv to xpt and create define document
#' @param logo Add logo by copying logo.jpg to the main working folderfirst time package user, run this function to install all required packages.
#' Can be downloaded from my Github
#' @param style Load document style by copying word template and rename as style.docx to the main working folder.
#' Can be downloaded from my Github
#' @param title Project title
#' @keywords step3
#' @export
#' @examples
#' step4(title="Add title here")

step3<-function(project_title="Project Title",xpt_location = ".\\",
                prog_location = "\\programs\\", 
                define_location = "\\datasets\\",
                range_character="yes"){
  class="auto"
  generateXPT(range.character=range_character)
  generateDEF1(title=project_title,xpt.location=xpt_location,prog.location=prog_location,define.location=define_location)
  cleardefCSV1()
}


#' Step 4a
#'
#' Convert csv to xpt and create define document
#' @param logo Add logo by copying logo.jpg to the main working folderfirst time package user, run this function to install all required packages.
#' Can be downloaded from my Github
#' @param style Load document style by copying word template and rename as style.docx to the main working folder.
#' Can be downloaded from my Github
#' @param title Project title
#' @keywords step4
#' @export
#' @examples
#' step4a(title="Add title here")
step4a<-function(title="Title"){
  create.library(name="library.csv")
  class="auto"
  generateXPT(range.character="yes")
  generateDEF1(title=title)
  cleardefCSV1()
}

#' vardefine
#'
#' Internal use.
#' @keywords vardefine
#' @export
#' @examples
#' vardefine()

vardefine <- function (data,maxlevel=7,vartype=T,labels=FALSE,digits = max(3, getOption("digits")-3),
                       exp.csv=FALSE) {
  definedataset <- data.frame(Variable=colnames(data))
  definedataset$Label <- ""

  for (i in 1:length(names(data))) {
    definedataset$Type[i] =  class(data[,i])[2]
    definedataset$Type[i] <- ifelse(definedataset$Type[i]=="integer","Num",
                                    ifelse(definedataset$Type[i]=="numeric","Num",
                                           ifelse(definedataset$Type[i]=="factor","Char",
                                                  ifelse(definedataset$Type[i]=="character","Char","Char"))))

    levels=ifelse(length(unique(data[,i]))>maxlevel,"",list(unique(as.character(data[,i]))))
    definedataset$code.range[i]  = ifelse(definedataset$Type[i]=="Num", paste(signif(min(data[,i],na.rm=T),digits=digits),
                                                                              signif(max(data[,i],na.rm=T),digits=digits),sep=" - "),
                                          ifelse(definedataset$Type[i]=="Char","-",
                                                 ifelse(signif(min(data[,i]))==signif(max(data[,i])),"-","")))
    if(labels==TRUE)
      definedataset$Label[i] = ifelse(is.character(label(data[,i]))==F,"no label",label(data[,i]))
    else
      definedataset$Label = NULL
  }
  if(vartype==FALSE)
    definedataset$Type=NULL

  definedataset$code.range <-gsub("NA,", "'.',", definedataset$code.range) # SASxport will convert the NA values into "."
  # the following 3 lines aim to remove the following characters:
  # '"', 'c(' and ')' at the beginning and at the end of the string, respectively

  definedataset$code.range  <- gsub("c(", "", definedataset$code.range,fixed=T)
  definedataset$code.range  <- gsub("[\"]", "", definedataset$code.range)
  definedataset$code.range  <- gsub("[$)]", "", definedataset$code.range)
  if (exp.csv==TRUE)
    definedataset$code.range  <- definedataset$code.range #paste("zzz",definedataset$code.range,sep="")
  names(definedataset)      <- gsub("code.range", "Code/Range", names(definedataset))

  return(definedataset)
}
