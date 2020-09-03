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

#' Install Package (First Installation)
#'
#' To install required packages. Use ipak function to install desired packages.
#' @param packages pre-define packages list.
#' @keywords install.pack
#' @export
#' @examples
#' install.pack()

install.pack<-function(...){
  packages <- c("SASxport", "reshape", "Hmisc", "tidyr","ReporteRs","plyr","downloader")
  ipak(packages)}


#' Download Template Logo in C drive
#'
#' To install require packages. Use ipak function to install desired packages.
#' @param repo leonpheng
#' @param user logo.style
#' @keywords template
#' @export
#' @examples
#' template()
template <- function(){
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
  library("ReporteRs")
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
    if(all.is.numeric(dat[,i],what = c("test","vector"),extras=c('.','NA',"NaN"," ","  ",NA)))
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
  setwd(working.folder)
  mainDir<-getwd()
  subDir<-c("input","output","Backup csv")

  for(i in 1:length(subDir)){
    dir.create(file.path(mainDir, subDir[i]), showWarnings = FALSE)
  }
  mainDir<-paste0(getwd(),"/output")
  subDir<-c("datasets","programs")
  for(i in 1:length(subDir)){
    dir.create(file.path(mainDir, subDir[i]), showWarnings = FALSE)
  }
  dir()
  lst<- read.csv("list of files.csv")
  #lst<-lst[lst$xptconvert==1,]
  for(i in 1:nrow(lst)){
    odir1<-paste0(getwd(),"/input/")
    odir2<-as.character(paste0(getwd(),"/output/programs/"))
    sour<-as.character(paste0(lst$sourcepath[i],"\\",lst$filename[i]))
    conv<-with(lst,paste0(rename[i],".",extension[i]))

    if(lst$type[i]=="data"){
      #dat <-read.csv(sour)
      #write.csv(dat,paste0(odir1,conv),row.names=F)
      file.copy(from=sour,
               to=paste0(odir1,conv),overwrite=T)
    }else{
      if(!lst$type[i]%in%c("data","prog")){
        dat <-read.nonmem.table(paste0(sour))
        write.csv(dat,paste0(odir1,conv),row.names=F)
      }else{
        if(lst$type[i]%in%c("prog")){
        file.copy(from=sour,
                  to=paste0(odir2,conv),overwrite=T)
      }}
    }
  }}


#' definelist
#'
#' internal used function.
#'
#' @keywords definelist
#' @export
#' @examples
#' definelist()

definelist<-function(...){
  setwd(working.folder)
  pathwork<-getwd()
  odir1<-paste0(pathwork,"/input/")
  var<-NULL
  for(i in dir(odir1)){
    var1<-data.frame(orivar=unlist(names(read.csv(paste0(odir1,i)))),file=i)
    var<-rbind(var,var1)
  }
  #lib<-read.xls(paste(pathwork,"label library.xls",sep="/"), sheet = 1)
  if(define.library!="no"){
    lib<-read.csv(paste(pathwork,define.library,sep="/"))}else{
      lib<-as.data.frame(matrix(ncol=7,nrow=1))
      names(lib)<-c("Variable","Unit","Detailed.description", "Enter.label.here",
                    "Max.40.char","Enter.label.here.1","Numflg")
    }
  row.names(lib)<-NULL
  lib$X<-NULL
  keep<-names(lib)
  lib$tmp<-toupper(lib$Variable)
  lib$Variable<-NULL
  var$tmp<-toupper(var$orivar)
  lib1<-plyr::join(var,lib)

  lib1$Variable<-lib1$orivar
  lib1$tmp<-NULL
  lib1$"SAS.label"<-lib1$"Enter.label.here"
  lib1$"Max.40.char"<-nchar(as.character(lib1$"SAS.label"))

  lib1$file<-lib1$file
  lib1$labelsize<-nchar(as.character(lib1$"Variable"))

  #if(is.null(lib1$"to.remove.type.1")){
  #  lib1$"to.remove.type.1"<-0
  #}
  keep<-c("Variable","Unit","Detailed.description","Enter.label.here","Max.40.char","file")

  #lib1$Numflg<-1
  lib1$Variable<-toupper(lib1$Variable)
  lib1$orivar<-toupper(lib1$orivar)
  lib1<-nodup(lib1,names(lib1),"all")
  lib1[,setdiff(keep,names(lib1))]<-NA
  lib1<-lib1[,c(keep)]

  lib2<-lib1[nchar(lib1$Variable)>8,]
  lib3<- lib1[grepl(".", lib1$Variable,
                    fixed = T),]
  if(nrow(lib2)>=1|nrow(lib3)>=1){
    lib2<-rbind(lib2,lib3)
    lib2$change.name<-"Enter new name"
    lib2<-nodup(lib2,names(lib2),"all")
    write.csv(lib2,paste(pathwork,"Var_name_GT8.csv",sep="/"),row.names=F)
    print("WRANING!!")
    print("!!Variable with name longer than 8 characters or containing dot (.) were found !!")
    print("Solution:")
    print("1) Complete studydefinelist.csv")
    print("2) Rename the variable in Var_name_GT8.csv")
    print("3) Run Change.variable.name () function to apply the change")
  }
  write.csv(lib1,paste(pathwork,"studydefinelist.csv",sep="/"),row.names=F)
}


#' create.library
#'
#' internal used function.
#'
#' @keywords definelist
#' @export
#' @examples
#' create.library()

create.library<-function(name){
  lib<-read.csv("studydefinelist.csv")
  filename = paste("./Backup csv/modified deflist-",format(Sys.time(), "%a-%b-%d-%H-%M-%S-%Y"),sep="")
  write.csv(lib,paste0(filename,".csv"))
  lib2<-with(lib,data.frame(
    Variable=Variable,
    Unit=Unit,
    Detailed.description=Detailed.description,
    Enter.label.here=Enter.label.here,
    Max.40.char=Max.40.char,
    SAS.label=Enter.label.here#,
    #Numflg=Numflg
    ))
  lib2<-nodup(lib2,"Variable","all")
  lib2$Max.40.char<-nchar(as.character(lib2$SAS.label))
  lib2<-lib2[!is.na(lib2$SAS.label),]
  # write.csv(lib2,name,row.names=F)
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
  setwd(working.folder)
  pathwork<-getwd()
  pathdir<-pathwork
  load.pack1()
  #sourcepath<-paste0(pathdir,"/functions") #This folder should contain sources (html export.R and label fun.R), libray excel fil (ex: label libraryAUSPECT.xls)
  #input<-paste0(pathdir,"/input")# This folder should contain all csv data
  input<-paste0(pathwork,"/input")
  progdir <- "./output/programs"# This folder should contain all program and output .txt
  outputdir<-paste0(pathdir,"/output/datasets") # This folder for resluted output
  #setwd(sourcepath)
  #source(file.path(sourcepath,"html export.R"))
  #source(file.path(sourcepath,"label fun.R"))
  checkclass=class#NULL or "auto"
  #setwd(input)
  #lst<- read.xls(paste(pathwork,"list of files.xlsx",sep="/"), sheet = 1)
  lst<- read.csv(paste(pathwork,"list of files.csv",sep="/"))
  #lst<-lst[lst$xptconvert==1,]
  head(lst)
  csv<-lst[lst$extension=="csv",]
  dir<-unlist(paste0(csv$rename,".csv"))
  oriname<-unlist(paste0(csv$filename))
  description<-unlist(paste0(csv$description))
  key<-unlist(paste0(csv$keyvar))

  ##PROGRAMS TABLE
  prog<-unlist(paste0(lst$renam[lst$type=="prog"],".txt"))
  progdes<-unlist(paste0(lst$renam[lst$description=="prog"]))
  ####### NOT TO BE EDITED ##################################
  location<-paste0("#programs#",prog)
  ###############################
  labdir<-gsub(".csv","",dir)#
  outdir<-dir#

  inp<-data.frame(
    input=dir,
    lab=labdir,#
    outp=sub(".csv","",outdir),
    prog="NA")#

  inp1<-data.frame(
    Dataset=paste0(sub(".csv","",outdir),".xpt"),
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
  detail<- read.csv(paste(pathwork,definelib,sep="/"))
  #numkeep<-detail$Variable[detail$Numflg==1]
  detail$"Variable"<-as.character(detail$"Variable")
  detail$Enter.label.here<-capitalize(as.character(detail$Enter.label.here))
  detail$SAS.label<-capitalize(as.character(detail$Enter.label.here))
  ################## START LOOP #################
  for (j in 1:nrow(inp)){
    require(SASxport)
    #numkeep<-detail$Variable[detail$Numflg==1]
    detail<- read.csv(paste(pathwork,definelib,sep="/"))
    detail<-detail[detail$file==as.character(inp$input[j]),]
    detail$"Variable"<-as.character(detail$"Variable")
    detail$Enter.label.here<-capitalize(as.character(detail$Enter.label.here))
    detail$SAS.label<-capitalize(as.character(detail$Enter.label.here))
    pkdata <- read.csv(paste(input,inp$input[j],sep="/"),stringsAsFactors = FALSE)
    pkdata<-chclass(pkdata,names(pkdata),"char")
    names(pkdata)<-toupper(names(pkdata))
    pkdata<-chclass(pkdata,names(pkdata),"char")
    #keepnum<-intersect(names(pkdata),numkeep)
    if(!is.null(checkclass)){
      pkdata<-autoclass(pkdata)
    }
    #else{pkdata<-chclass(pkdata,keepnum,"num")}

    for (h in 1:nrow(detail)){
      tryCatch(label(pkdata[,paste(detail$Variable[h])])<- paste(detail$SAS.label[h]),error=function(e) NULL )
    }
    label(pkdata)<-inp$lab[j]

    rangepkdat <- vardefine(pkdata,maxlevel=10,labels=T,digits=6,vartype=T,exp.csv=T) # same as previous, to export to csv
    detail1<- detail #read.xls(paste(location,"/make defines library.xls",sep=""),sheet=1,method="tab")
    detail1<-detail1[!duplicated(detail1$Variable),]
    detail1<-detail1[detail1$Variable%in%unique(rangepkdat$Variable),]
    rangepkdat<-merge(rangepkdat,detail1[,c("Variable","Unit","Detailed.description")],by=c("Variable","Variable"),all.x=T,sort=F)
    #File name should not exceed 8 characters- NOTE that all variable names will be limited to 8 characters
    name<-as.character(inp$outp[j])
    assign(name,pkdata)
    file1<-paste(inp$outp[j],".xpt",sep="")
    sav<-paste0("write.xport(",inp$outp[j],",file=file1,autogen.formats=FALSE)")
    setwd(outputdir)
    eval(parse(text=sav))

    require(SASxport)   # to be able to read the xport files
    data<-read.xport(paste(outputdir,"/",inp$outp[j],".xpt",sep=""))
    definedataset <- data.frame(Variable=colnames(data))
    range<-vardefine(data,maxlevel=10,labels=T,digits=6,vartype=T,exp.csv=T)
    data1<-data.frame(var=colnames(data))
    rangepkdat$Type<-range$Type
    rangepkdat$Variable<-range$Variable
    rangepkdat$"Code/Range"<-range$"Code/Range"
    names(rangepkdat)<-c("SAS Variable","SAS Label","Type","Code/Range","Unit","Detailed.description")
    rangepkdat<-rangepkdat[,c("SAS Variable","SAS Label","Type","Code/Range","Unit","Detailed.description")]
    rangepkdat$"SAS Variable"<-gsub(rangepkdat$"SAS Variable",pattern="\\_",replacement="XXXX")
    detach("package:SASxport", unload = T)

    ind<-as.numeric(row.names(rangepkdat[rangepkdat$"Code/Range"=="-",]))
    if(length(ind)>0){
      rgd<-rangepkdat
      for(i in 1:length(ind)){
        x<-ifelse(length(unique(data[,ind[i]]))>5,paste(paste(as.character(unique(data[,ind[i]])[1:5]),collapse=", ",sep=""),",..."),paste(as.character(unique(data[,ind[i]])),collapse=", ",sep=""))
        rgd[ind[i],"Code/Range"]<-x}}else{rgd<-rangepkdat}
    if(!is.null(range.character)){
      write.csv(rgd,paste(inp$outp[j],"define.csv",sep=""),row.names=F)}else{
        write.csv(rangepkdat,paste(inp$outp[j],"define.csv",sep=""),row.names=F)
      }
    setwd("../../")
    getwd()
  }
  #END LOOP #
}
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

generateDEF1<-function(title="Add title here"){
  setwd(working.folder)
  pathwork<-getwd()
  pathdir<-pathwork
  sourcepath<-paste0(pathdir,"/functions") #This folder should contain sources (html export.R and label fun.R), libray excel fil (ex: label libraryAUSPECT.xls)
  input<-paste0(pathdir,"/input")# This folder should contain all csv data
  progdir <- paste0(pathdir,"/output/programs")# This folder should contain all program and output .txt
  outputdir<-paste0(pathdir,"/output/datasets") # This folder for resulted output
  #setwd(outputdir)
  dir()
  #lst<- read.xls(paste(pathwork,"list of files.xlsx",sep="/"), sheet = 1)
  lst<- read.csv(paste(pathwork,"list of files.csv",sep="/"))
  #lst<-lst[lst$xptconvert==1,]

  csv<-lst[lst$extension=="csv",]
  dir<-unlist(paste0(csv$rename,".csv"))
  oriname<-unlist(paste0(csv$filename))
  description<-unlist(paste0(csv$description))
  key<-unlist(paste0(csv$keyvar))
  struct<-unlist(paste0(csv$Structure))
  usedprog<-unlist(paste0(csv$Program))

  ###############################
  labdir<-gsub(".csv","",dir)#
  outdir<-dir#

  inp<-data.frame(
    input=dir,
    lab=labdir,#
    outp=sub(".csv","",outdir),
    prog="NA")#

  inp1<-data.frame(
    Dataset=paste0(sub(".csv","",outdir),".xpt"),
    "Original Name"=oriname,
    Description=description,#sub(".csv","",dir),  # Label for xpt output, may be required by FDA
    Keyvariables=key,
    Datasetfullname=paste0("#datasets#",paste0(sub(".csv","",outdir),".xpt")))

  inp1$"Original.Name"<-gsub(inp1$"Original.Name",pattern="\\_",replacement="XXXX")
  chclass<-function (data, var, class = "char")
  {
    for (i in var) {
      if (class == "num") {
        data[, i] <- as.numeric(as.character(data[, i]))
      }  else {
        data[, i] <- as.character(data[, i])
      }
    }
    data
  }
  inp1<-chclass(inp1,names(inp1),"char")

  ##### SETTING for COLUMN HEADERS
  tbname<-"Define"
  ##SECTION TITLES
  maintit<-data.frame(tit=c("TABLE OF CONTENTS",
                            "DATASETS TABLE OF CONTENTS",
                            "VARIABLE DEFINITION TABLES",
                            "PROGRAMS TABLE OF CONTENTS"))
  #COLUMN HEADERs for VARIABLE DEFINITION TABLE 2
  style1<-textProperties(color = "black", font.size =12,
                         font.weight = "bold", font.style = "normal", underlined = FALSE,
                         font.family = getOption("ReporteRs-default-font")

  )
  if("style.docx"%in%dir("c:/lhtemplate")){
    doc<-docx(template = "c:/lhtemplate/style.docx", empty_template = TRUE)
    doc = map_title(doc, stylenames = c("Heading1", "Heading2", "Heading3") )
  }else{doc<-docx()}

tabn<-c("Dataset", "Original Name",   "Description",     "Key Variables",    "Location")#as.character(names(inp1[1:nrow(inp1),]))
  #HYPERLINK1
  hyp0<-inp1[,"Datasetfullname"]
  inp1[,"Datasetfullname"]<-""
  tab = FlexTable( data = inp1[1:nrow(inp1),], header.columns= FALSE)
  tab = addHeaderRow( tab, text.properties = textBold(),
                      value = tabn)
  #tab[,5] = textProperties(color = 'blue' )

  hyp1<-paste0(sub(".csv","",outdir),".xpt")
  for(i in 1:length(hyp1)){
    hyp11<-paste0("./datasets/",hyp1[i])
    tab[i,5] = pot( hyp0[i], hyperlink = hyp11,
                    textBold( color = '#0000EE', underline = F ) ) }

  if("logo.jpg"%in%dir("c:/lhtemplate")){
    doc<-doc%>%addImage("c:/lhtemplate/logo.jpg", par.properties = parProperties(text.align = "center"),width = 3.35, height = 1.6)
  }
  doc<-doc %>%
    addParagraph(pot(title,style1),par.properties = parProperties(text.align = "center")) %>%
    addPageBreak()%>%
    addParagraph(pot("TABLE OF CONTENTS",style1),par.properties = parProperties(text.align = "left"))%>%
    addTOC(level_max = 3)%>%
    addPageBreak()%>%
    addTitle("DATASETS TABLE OF CONTENTS",level=1)%>%
    addFlexTable(tab)%>%addPageBreak()%>%
    addTitle("VARIABLE DEFINITION TABLES",level=1)

  tab1data<-inp
  for (j in 1:nrow(inp)){
    doc<-addTitle(doc, as.character(tab1data$outp[j]),level=2)
    data<-read.csv(file.path(outputdir,paste(inp$outp[j],"define.csv",sep="")))
    data$"SAS.Variable"<-toupper(data$"SAS.Variable")
    head(data)

    #  txt2<-paste("Name of original version:",tab1data$input[j],"\nStructure:",struct[j],"\nDataset: ",inp1$Dataset[j],
    #           "\nProgram:",tab1data$prog[j])

    tw1a<-as.data.frame(matrix(ncol=length(names(data)),nrow=5))
    tw1a[1:5,1]<-c(as.character(inp1$Description[j]),"Name of original version: ","Structure: ","Dataset: ","Program: ")
    tw1a[,3:6]<-""
    names(tw1a)<-names(data)
    tw1bn<-c("Variable","Label","Type", "Code Range", "Unit", "Detailed Description")#names(data)
    tw1b<-as.data.frame(matrix(ncol=length(names(data)),nrow=1,data=tw1bn))

    names(tw1b)<-names(data)
    tw2<-rbind(tw1a,tw1b,data)
    tab = FlexTable( data = tw2, header.columns= FALSE)
    tab[6,1:6] = textProperties(font.weight = 'bold' )
    tab[1,1] = textProperties(font.weight = 'bold' )
    tab[4:5,2] = textProperties(color ="blue")
    tab=spanFlexTableColumns( tab, i = 1:5, from = 1, to = 6 )
    tab=spanFlexTableColumns( tab, i = 2, from = 1, to = 6 )
    tab=spanFlexTableColumns( tab, i = 3, from = 1, to = 6 )
    tab[2,1, text.properties = textNormal(color = 'black')] =as.character(inp1$Original.Name[j])
    tab[3,1, text.properties = textNormal(color = 'black')] =as.character(struct[j])
    hyp2<- as.character(inp1$Dataset[j])
    hyp11<-paste0("./datasets/",as.character(inp1$Dataset[j]))
    tab[4,1] = pot(hyp2, hyperlink = hyp11,
                   textBold( color = '#0000EE', underline = F ) )
    #as.character(inp1$Dataset[j])
    if(usedprog[j]=="NA"){
      tab[5,1] =as.character(usedprog[j])}else{
        hyp2<- as.character(paste0(usedprog[j],".txt"))
        hyp11<-paste0("./programs/",hyp2)
        tab[5,1] = pot(hyp2, hyperlink = hyp11,
         textBold( color = '#0000EE', underline = F ) )}
    border_ <- borderProperties( style = 'none' )
    tab[1:4,1:6, side = 'bottom'] <- border_
    doc <- addFlexTable(doc,tab)
    doc<-doc%>%addPageBreak()
  }

##Tab 3
##PROGRAMS TABLE
prog<-unlist(paste0(lst$renam[lst$type=="prog"],".txt"))
progdes<-unlist(paste0(lst$description[lst$type=="prog"]))
origprog<-unlist(paste0(lst$filename [lst$type=="prog"]))

####### NOT TO BE EDITED ##################################
  location<-paste0("#programs#",prog)
  doc<-addTitle(doc, "PROGRAMS TABLE OF CONTENTS",level=1)
  dir(progdir)

  include<-paste0(lst$rename[lst$progNo!=""],".txt")
  IOD<-lst[lst$progNo%in%lst$progNo[lst$progNo!=""]&!is.na(lst$progNo),]
  ind<-lst[rownames(lst)%in%rownames(IOD),]
  prono<-lst[rownames(lst)%in%rownames(IOD),"progNo"]

if(nrow(IOD)>0){
    prog<-unlist(paste0(ind$renam[ind$type=="prog"],".txt"))
    progdes<-unlist(paste0(ind$description[ind$type=="prog"]))
    origprog<-unlist(paste0(ind$filename[ind$type=="prog"]))

tab3data<-data.frame(Program="",
                         Description=IOD$description,
                         Input_Output_Dependency="")
tab = FlexTable(data = tab3data, header.columns=T)

for(i in 1:length(origprog)){
op<-paste0("./programs/",prog[i])
tab[i,1] = pot(prog[i],hyperlink =op,
textBold( color = '#0000EE', underline = F ) )+ " \n(original:" +pot(origprog[i])+")"

      io1<-lst[grep(prono[i],lst$proNo.input),]
      io2<-lst[grep(prono[i],lst$proNo.output),]
      io3<-lst[grep(prono[i],lst$progNo.dependent),]

if(nrow(io1)==0){
tt =  pot("Input",textBold())
}else{
  zz0<-""
  for(io in 1:nrow(io1)){
    ext1<-ifelse(io1$type[io]=="prog",".txt",".xpt")
    txtins<-paste0(io1$rename[io],ext1)
    ext2<-ifelse(io1$type[io]=="prog","./programs/","./datasets/")
    insert<-paste0(ext2,txtins)
    orf<-paste0("(original:",io1$filename[io],")")
    col='#0000EE'
    space="\n  "
    zz0<-zz0+pot(txtins,hyperlink=insert,textBold(color=col,underline = F ))+space+pot(orf,textNormal())+space}}

if(nrow(io2)==0){
  zz1 =  pot("",textBold())
}else{
zz1<-""
for(io in 1:nrow(io2)){
  ext1<-ifelse(io2$type[io]=="prog",".txt",".xpt")
  txtins<-paste0(io2$rename[io],ext1)
  ext2<-ifelse(io2$type[io]=="prog","./programs/","./datasets/")
  insert<-paste0(ext2,txtins)
  orf<-paste0("(original:",io2$filename[io],")")
  col='#0000EE'
  space="\n  "
zz1<-zz1+pot(txtins,hyperlink=insert,textBold(color=col,underline = F ))+space+pot(orf,textNormal())+space}
  }

if(nrow(io3)==0){
  zz2 =  pot("",textBold())
  }else{
    zz2<-""
    for(io in 1:nrow(io3)){
      ext1<-ifelse(io3$type[io]=="prog",".txt",".xpt")
      txtins<-paste0(io3$rename[io],ext1)
      ext2<-ifelse(io3$type[io]=="prog","./programs/","./datasets/")
      insert<-paste0(ext2,txtins)
      orf<-paste0("(original:",io3$filename[io],")")
      col='#0000EE'
      space="\n  "
      zz2<-zz2+pot(txtins,hyperlink=insert,textBold(color=col,underline = F ))+space+pot(orf,textNormal())+space}
  }

  tab[i,3] =pot("Input",textBold())+"\n "+zz0+"\n  "+pot("Output",textBold())+"\n  "+zz1+"\n  "+pot("Dependency",textBold())+"\n  "+zz2
}}else{
if(length(dir(progdir))>=1&nrow(IOD)==0){
  tab3data<-data.frame(Original=origprog,
                       Program=prog,
                       Description=progdes,
                       Location=location)
}else{
    tab3data<-data.frame(Original="",
                         Program="",
                         Description="",
                         Location="")}}

if(nrow(IOD)==0){
orig<-origprog
hyp0<-paste0("#program#",tab3data[,"Program"])
hyp1<-tab3data
hyp2<-hyp1[,"Location"]
hyp1[,"Location"]<-""
tab = FlexTable(data = hyp1[1:nrow(hyp1),], header.columns= FALSE)
  tab = addHeaderRow(tab, text.properties = textBold(),
                      value = as.character(names(tab3data)))
for(i in 1:length(hyp0)){
    hyp11<-paste0("./programs/",hyp0[i])
    tab[i,4] = pot(hyp0[i],hyperlink = hyp11,
                   textBold( color = '#0000EE', underline = F ) ) }}


  doc <- addFlexTable(doc,tab)
  writeDoc(doc, file = "./output/define.docx")
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
  outputdir<-paste0(pathdir,"/output/datasets") # This folder for resluted output
  frm<-dir(outputdir)
  index<-c(grep("csv",frm),grep("tex",frm))
  frm<-frm[index]
  for(i in frm){
    file.remove(paste(outputdir,i,sep="/"))
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
#' title="Test" # Mondatory. this title will appear in define
#' step4(title)

helps<-function(...){
  x<-data.frame(stepBYstep=seq(11),
                Procedure=c("working.folder= path",
                            "run step1(working.folder)",
                            "edit list of files.csv and save",
                            "***For classic define, leave the progNo columns blank****",
                            "define.library= set to no or add library name if available",
                            "run step2()",
                            "edit and save studydefinelist.csv and Var_name_GT8.csv if applicable",
                            "run step3() to apply the corrections requested in Var_name_GT8.csv",
                            "***The variable name longer than 8 characters will be corrected in the CSV file and studydefinelist***",
                            "run step4(title) #enter project title",
                            "edit and format the define.doc"))
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

step1<-function(working.folder){
  setwd(working.folder)
  if(!"list of files.csv"%in%dir()){
    lf<-data.frame(
      filename=c("data.csv","nonmem (PCSmisch package needed)","prog1"),
      type=c("data","nnm","prog"),
      extension=c("csv","","txt"),
      rename=c("pkdata","patab","phxsetting"),
      keyvar=c("USUBJID,TIME","ID,TIME",""),
      Structure=c("per subject per time point","per subject per time point",""),
      #xptconvert=c("1","1","1"),
      Program=c("NA","phxsetting",""),
      description=c("PK dataset","Posthoc","Phoenix settings")	,
      sourcepath=c("copy/paste path","",""),
      progNo=c("unique number","ex.#1a",""),
      proNo.input=c("could be multiple","#2b#3a",""),
      proNo.output=c("could be multiple","#2b#3a",""),
      progNo.dependent=c("could be multiple","#2b#3a","NOTE:that these R functions\n should be included as program\n but without progNo, i.e leave blank cell"))
    write.csv(lf,"list of files.csv")
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


#' Step 3
#'Optional. Run this option to apply the changes to label name or lable removal.
#' @keywords step3
#' @export
#' @examples
#' step3()


step3<-function(){
  setwd(working.folder)
  dir.create("Backup csv")
  lib<-read.csv("studydefinelist.csv")
  filename = paste("./Backup csv/Original studydefinelist-",format(Sys.time(), "%a-%b-%d-%H-%M-%S-%Y"),sep="")
  write.csv(lib,paste0(filename,".csv"))
  lib2<-read.csv("Var_name_GT8.csv")
  filename = paste("./Backup csv/Original Var_name_GT8-",format(Sys.time(), "%a-%b-%d-%H-%M-%S-%Y"),sep="")
  write.csv(lib2,paste0(filename,".csv"))

  isnam<-unique(lib2$file)

  rem<-lib[lib$to.remove.type.1==1&!is.na(lib$to.remove.type.1),c("Variable","file")]
  rem<-rbind(rem,lib2[lib2$to.remove.type.1==1&!is.na(lib2$to.remove.type.1),c("Variable","file")])
  rem1<-lib2[lib2$to.remove.type.1==0&!is.na(lib2$to.remove.type.1),c("Variable","file","change.name")]

  lib<-chclass(lib,c("Variable","file"),"char")

  rem<-chclass(rem,c("Variable","file"),"char")
  rem1<-chclass(rem1,c("Variable","file","change.name"),"char")

  for(d in unique(c(rem$file,rem1$file))){
    dc<-read.csv(paste0("./input/",d))
    names(dc)<-toupper(names(dc))
    var<-c(rem$Variable[rem$file==d])
    for(d1 in var){
      dc[,names(dc)==d1]<-NULL
      lib<-lib[lib$Variable!=d1,]}
    #change name  }
    var2<-lib2[lib2$file==d&lib2$change.name!="Enter new name",]
    for(d2 in var2$Variable){
      names(dc)[names(dc)==d2]<-as.character(var2$change.name[var2$Variable==d2])
      lib$Variable[lib$Variable==d2&lib$file==d]<-as.character(var2$change.name[var2$Variable==d2])
    }

    write.csv(dc,paste0("./input/",d),row.names=F)
    write.csv(lib,"studydefinelist.csv",row.names=F)
  }}

#' Step 4
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
#' step4(title="Add title here")
step4<-function(title="Title"){
  create.library(name="library.csv")
  class="auto"
  generateXPT(range.character="no")
  generateDEF1(title=title)
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
