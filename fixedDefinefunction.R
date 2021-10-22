generateDEF1<-function (title = "Add title here", xpt.location = "./", 
          prog.location = "../programs/", define.location = "./output/datasets/") 
{
  setwd(working.folder)
  pathwork <- getwd()
  pathdir <- pathwork
  sourcepath <- paste0(pathdir, "/functions")
  input <- paste0(pathdir, "/input")
  progdir <- paste0(pathdir, "/output/programs")
  outputdir <- paste0(pathdir, "/output/datasets")
  lst <- read.csv(paste(pathwork, "list of files.csv", 
                        sep = "/"), stringsAsFactors = F)
  head(lst)
  lst$type[lst$type != "dataset"] <- "program"
  csv <- lst[lst$type == "dataset", ]
  dir <- unlist(paste0(csv$rename, ".csv"))
  oriname <- unlist(paste0(csv$filename))
  description <- unlist(paste0(csv$description))
  key <- unlist(paste0(csv$keyvar))
  struct <- unlist(paste0(csv$Structure))
  usedprog <- unlist(paste0(csv$Program))
  labdir <- gsub(".csv", "", dir)
  outdir <- dir
  inp <- data.frame(input = dir, lab = labdir, outp = sub(".csv", 
                                                          "", outdir), prog = "NA")
  inp1 <- data.frame(Dataset = paste0(sub(".csv", "", 
                                          outdir), ".xpt"), `Original Name` = oriname, 
                     Description = description, Keyvariables = key, Datasetfullname = paste0("#datasets#", 
                                                                                             paste0(sub(".csv", "", outdir), ".xpt")))
  inp1$Original.Name <- gsub(inp1$Original.Name, pattern = "\\_", 
                             replacement = "XXXX")
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
  style1 <- textProperties(color = "black", font.size = 12, 
                           font.weight = "bold", font.style = "normal", 
                           underlined = FALSE, font.family = getOption("ReporteRs-default-font"))
  if ("style.docx" %in% dir("c:/lhtemplate")) {
    doc <- docx(template = "c:/lhtemplate/style.docx", 
                empty_template = TRUE)
    doc = map_title(doc, stylenames = c("Heading1", 
                                        "Heading2", "Heading3"))
  } else {
    doc <- docx()
  }
  
  tabn <- c("Dataset", "Original Name", "Description", 
            "Key Variables", "Location")
  hyp0 <- inp1[, "Datasetfullname"]
  inp1[, "Datasetfullname"] <- ""
  tab = FlexTable(data = inp1[1:nrow(inp1), ], header.columns = FALSE)
  tab = addHeaderRow(tab, text.properties = textBold(), value = tabn)
  hyp1 <- paste0(sub(".csv", "", outdir), ".xpt")
  for (i in 1:length(hyp1)) {
    loc1 <- paste0(xpt.location, hyp1[i])
    hyp11 <- loc1
    tab[i, 5] = pot(hyp0[i], hyperlink = hyp11, textBold(color = "#0000EE", 
                                                         underline = F))
  }
  if ("logo.png" %in% dir("c:/lhtemplate")) {
    doc <- doc %>% addImage("c:/lhtemplate/logo.png", 
                            par.properties = parProperties(text.align = "center"), 
                            width = 3.35, height = 1.6)
  }
  doc <- doc %>% addParagraph(pot(title, style1), par.properties = parProperties(text.align = "center")) %>% 
    addPageBreak() %>% addParagraph(pot("TABLE OF CONTENTS", 
                                        style1), par.properties = parProperties(text.align = "left")) %>% 
    addTOC(level_max = 3) %>% addPageBreak() %>% addTitle("DATASETS TABLE OF CONTENTS", 
                                                          level = 1) %>% addFlexTable(tab) %>% addPageBreak() %>% 
    addTitle("VARIABLE DEFINITION TABLES", level = 1)
  tab1data <- inp
  for (j in 1:nrow(inp)) {
    doc <- addTitle(doc, as.character(tab1data$outp[j]), 
                    level = 2)
    data <- read.csv(file.path(outputdir, paste(inp$outp[j], 
                                                "define.csv", sep = "")))
    data$SAS.Variable <- toupper(data$SAS.Variable)
    head(data)
    tw1a <- as.data.frame(matrix(ncol = length(names(data)), 
                                 nrow = 5))
    tw1a[1:5, 1] <- c(as.character(inp1$Description[j]), 
                      "Name of original version: ", "Structure: ", 
                      "Dataset: ", "Program: ")
    tw1a[, 3:6] <- ""
    names(tw1a) <- names(data)
    tw1bn <- c("Variable", "Label", "Type", 
               "Code Range", "Unit", "Detailed Description")
    tw1b <- as.data.frame(matrix(ncol = length(names(data)), 
                                 nrow = 1, data = tw1bn))
    names(tw1b) <- names(data)
    tw2 <- rbind(tw1a, tw1b, data)
    tab = FlexTable(data = tw2, header.columns = FALSE)
    tab[6, 1:6] = textProperties(font.weight = "bold")
    tab[1, 1] = textProperties(font.weight = "bold")
    tab[4:5, 2] = textProperties(color = "blue")
    tab = spanFlexTableColumns(tab, i = 1:5, from = 1, to = 6)
    tab = spanFlexTableColumns(tab, i = 2, from = 1, to = 6)
    tab = spanFlexTableColumns(tab, i = 3, from = 1, to = 6)
    tab[2, 1, text.properties = textNormal(color = "black")] = as.character(inp1$Original.Name[j])
    tab[3, 1, text.properties = textNormal(color = "black")] = as.character(struct[j])
    hyp2 <- as.character(inp1$Dataset[j])
    hyp11 <- paste0(xpt.location, as.character(inp1$Dataset[j]))
    tab[4, 1] = pot(hyp2, hyperlink = hyp11, textBold(color = "#0000EE", 
                                                      underline = F))
    if (usedprog[j] == "NA" | usedprog[j] == "") {
      tab[5, 1] = as.character(usedprog[j])
    }    else {
      hyp2 <- as.character(paste0(usedprog[j], ".txt"))
      hyp11 <- paste0(prog.location, hyp2)
      tab[5, 1] = pot(hyp2, hyperlink = hyp11, textBold(color = "#0000EE", 
                                                        underline = F))
    }
    border_ <- borderProperties(style = "none")
    tab[1:4, 1:6, side = "bottom"] <- border_
    doc <- addFlexTable(doc, tab)
    doc <- doc %>% addPageBreak()
  }
  prog <- unlist(paste0(lst$renam[lst$type == "program"], 
                        ".txt"))
  progdes <- unlist(paste0(lst$description[lst$type == "program"]))
  origprog <- unlist(paste0(lst$filename[lst$type != "dataset"]))
  location <- paste0("#programs#", prog)
  doc <- addTitle(doc, "PROGRAMS TABLE OF CONTENTS", 
                  level = 1)
  dir(progdir)
  include <- paste0(lst$rename[lst$progNo != "" & !is.na(lst$progNo)], 
                    ".txt")
  IOD <- lst[lst$progNo %in% lst$progNo[lst$progNo != "" & 
                                          !is.na(lst$progNo)] & !is.na(lst$progNo), ]
  ind <- lst[rownames(lst) %in% rownames(IOD), ]
  prono <- lst[rownames(lst) %in% rownames(IOD), "progNo"]
  
  
  if (nrow(IOD) > 0) {
    prog <- unlist(paste0(ind$renam[ind$type != "dataset"], 
                          ".txt"))
    progdes <- unlist(paste0(ind$Purpose[ind$type != "dataset"]))
    origprog <- unlist(paste0(ind$filename[ind$type != "dataset"]))
    software.used <- unlist(paste0(ind$Software.version[ind$type != 
                                                          "dataset"]))
    tab3data <- data.frame(Program_Original_names = "", 
                           Description = "", Input_Output_log_file_original_names = "")
    tab<-as.data.frame(matrix(ncol=3,nrow=length(origprog)+1))
    names(tab)<-c("Program_Original-names","Description","Input_Output_log_file_original_names")
    #tab = FlexTable(data=tab3data, header.columns = T)
    tab = FlexTable(tab)
    
for (i in 1:length(origprog)) {
      op <- paste0("./programs/", prog[i])
      space = "\n  "
      tab[i, 1] = pot(prog[i]) + "\n(original:" + 
        pot(origprog[i]) + ")"
      tab[i, 2] = pot("Software used: ", textBold()) + 
        software.used[i] + space + pot("Purpose: ", 
                                       textBold()) + progdes[i]
      io1 <- lst[grep(prono[i], lst$proNo.input), ]
      io2 <- lst[grep(prono[i], lst$proNo.output), ]
      io3 <- lst[grep(prono[i], lst$progNo.dependent), 
      ]
if (nrow(io1) == 0) {
        tt = pot("", textBold())
      } else {
        zz0 <- ""
        for (io in 1:nrow(io1)) {
          ext1 <- ifelse(io1$type[io] == "program", 
                         ".txt", ".xpt")
          txtins <- paste0(io1$rename[io], ext1)
          ext2 <- ifelse(io1$type[io] == "program", 
                         "./programs/", "./datasets/")
          insert <- paste0(ext2, txtins)
          orf <- paste0("(original:", io1$filename[io], 
                        ")")
          col = "#0000EE"
          space = "\n  "
          zz0 <- zz0 + pot(txtins, textNormal()) + space + 
            pot(orf, textNormal()) + space
        }
      }
      if (nrow(io2) == 0) {
        zz1 = pot("", textBold())
      }      else {
        zz1 <- ""
        for (io in 1:nrow(io2)) {
          ext1 <- ifelse(io2$type[io] == "program", 
                         ".txt", ".xpt")
          txtins <- paste0(io2$rename[io], ext1)
          ext2 <- ifelse(io2$type[io] == "program", 
                         "./programs/", "./datasets/")
          insert <- paste0(ext2, txtins)
          orf <- paste0("(original:", io2$filename[io], 
                        ")")
          col = "#0000EE"
          space = "\n  "
          zz1 <- zz1 + pot(txtins, textNormal()) + space + 
            pot(orf, textNormal()) + space
        }
      }
      if (nrow(io3) == 0) {
        zz2 = pot("", textBold())
      }      else {
        zz2 <- ""
        for (io in 1:nrow(io3)) {
          ext1 <- ifelse(io3$type[io] == "program", 
                         ".txt", ".xpt")
          txtins <- paste0(io3$rename[io], ext1)
          ext2 <- ifelse(io3$type[io] == "program", 
                         "./programs/", "./datasets/")
          insert <- paste0(ext2, txtins)
          orf <- paste0("(original:", io3$filename[io], 
                        ")")
          col = "#0000EE"
          space = "\n  "
          zz2 <- zz2 + pot(txtins, textNormal()) + space + 
            pot(orf, textNormal()) + space
        }
      }
head(tab)

tab[i, 3] = pot("[Input]", textBold()) + "\n " + 
        zz0 + "\n  " + pot("[Output]", textBold()) + 
        "\n  " + zz1 + "\n  " + pot("[Dependency]", 
                                    textBold()) + "\n  " + zz2
    }
  } else {
    if (length(dir(progdir)) >= 1 & nrow(IOD) == 0) {
      tab3data <- data.frame(Original = origprog, Program = prog, 
                             Description = progdes, Location = location)
    } else {
      tab3data <- data.frame(Original = "", Program = "", 
                             Description = "", Location = "")
    }
  }
  
  if (nrow(IOD) == 0) {
    orig <- origprog
    hyp0 <- paste0("#programs#", tab3data[, "Program"])
    hyp1 <- tab3data
    hyp2 <- hyp1[, "Location"]
    hyp1[, "Location"] <- ""
    tab = FlexTable(data = hyp1[1:nrow(hyp1), ], header.columns = FALSE)
    tab = addHeaderRow(tab, text.properties = textBold(), 
                       value = as.character(names(tab3data)))
    for (i in 1:length(hyp0)) {
      hyp11 <- paste0(prog.location, hyp1[, "Program"][i])
      tab[i, 4] = pot(hyp0[i], hyperlink = hyp11, textBold(color = "#0000EE", 
                                                           underline = F))
    }
  }
doc <- addFlexTable(doc, tab)
writeDoc(doc, paste0(define.location, "define.docx"))
}