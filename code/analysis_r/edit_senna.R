# edit_senna.R
# setwd("Users/jonathanold/Documents/senna/")

library(data.table)
library(dplyr)
library(writexl)
set.seed(20231017)
load("/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/dataset_satp_complete.RData")






pushSenna<-function(ss, opts="",sennapath="Documents/senna") {
    tempin<-tempfile()
    writeLines(ss, tempin)
    tempout<-tempfile()
    opts<-ifelse(nchar(opts)==0, "",paste(" -",opts,sep=""))
    temp<-system(paste("cd ",sennapath,"; senna-osx",opts," < ",tempin,">",tempout,sep=""),
                        intern=TRUE)
    temp<-read.table(tempout, fill = TRUE)
    unlink(tempin)
    unlink(tempout)
    temp
}


returnSVO <- function(temp, col.num = 5, type = "") {
  verbs <- grep("^[^-]", temp[, col.num])
 # colx = ifelse(col.num+x<=20, col.num+x, col.num+(x-15))
  if (length(verbs) > 0) {
    if (type == "") {
      SVO <- lapply(1:length(verbs), function(x) { 
        colx= col.num + x
        #colx = ifelse(col.num+x<=20, col.num+x, col.num+(x-15))
        
        last_column = names(temp)[ncol(temp)]
        missing_rows = which(temp[[last_column]]=="")
        nonmissing_rows = which(temp[[last_column]]!="")
        
        if (length(missing_rows)==0) {
        } else {
          nc = ncol(temp)
          newvar = matrix(0,nrow=nrow(temp),ncol=(nc-7))
          temp = cbind(temp, newvar)
          temp[missing_rows, (nc-1):(nc+(nc-6)-1)] = temp[missing_rows, 6:nc]
          
          rowX = temp[missing_rows[1],]
          last_nonmissing_var = tail(which(rowX!=""),n=1)
          lnmv = names(temp)[last_nonmissing_var]
          temp[nonmissing_rows, lnmv] = temp[nonmissing_rows, last_column]
          temp[,(last_nonmissing_var+1):ncol(temp)]=NULL
        }
        
        data.frame(
        "verb" = temp[grep("-V{1}", temp[, colx]), 1],
        "subject" = paste(temp[grep("-A0", temp[, colx]), 1], collapse = " "),
        "object" = paste(temp[grep("-A[1-9]+", temp[, colx]), 1], collapse = " ")
      ) })
      SVO <- rbindlist(SVO)
    }
    else if (type == "list") {
      SVO <- lapply(1:length(verbs), function(x) data.frame(
        "id" = x,
        rbind(
          cbind("type" = "verb", "token" = temp[grep("-V{1}", temp[, colx]), 1]),
          cbind("type" = "subject", "token" = paste(temp[grep("-A0", temp[, colx]), 1], collapse = " ")),
          cbind("type" = "object", "token" = paste(temp[grep("-A[1-9]+", temp[, colx]), 1], collapse = " "))
        )
      ))
      SVO <- rbindlist(SVO)
    }
    SVO
  }
}

returnNER <- function(temp, col.num = 4) {
  temp$nertype <- gsub("([A-Z]{1})-([A-Z]*)", "\\2", temp[, col.num])
  
  ##define start and end pairs
  start <- grep("^B", temp[, col.num])
  end <- grep("^E", temp[, col.num])
  singleton <- grep("^S", temp[, col.num])
  index <- data.frame(rbind(cbind(start, end), cbind(singleton, singleton)))
  index <- index[order(index[, 1]),]
  
  if (nrow(index) > 0) {
    index$ner <- 1:nrow(index)
    NER <- lapply(1:nrow(index), function(x)
      cbind(
        index[x, ],
        ner = paste(temp[index[x, 1]:index[x, 2], 1], collapse = " "),
        type = temp[index[x, 1], "nertype"]
      )
    )
    NER <- rbindlist(NER)
    NER
  }
}



for (i in 21:30) {
  resultX = pushSenna(dataX$description[i])
  data_complete_SVO = returnSVO(resultX)
  data_complete_SVO$id = i
  data_complete_NER = returnNER(resultX)
  data_complete_NER$id = i
  print(i)
}




temp = pushSenna(dataX$description[22])
col.num=5
verbs <- grep("^[^-]", temp[, col.num])
length(verbs)


last_column = names(temp)[ncol(temp)]
missing_rows = which(temp[[last_column]]=="")
nonmissing_rows = which(temp[[last_column]]!="")

if (length(missing_rows)==0) {
  } else {
nc = ncol(temp)
newvar = matrix(0,nrow=nrow(temp),ncol=(nc-7))
temp = cbind(temp, newvar)
temp[missing_rows, (nc-1):(nc+(nc-6)-1)] = temp[missing_rows, 6:nc]

rowX = temp[missing_rows[1],]
last_nonmissing_var = tail(which(rowX!=""),n=1)
lnmv = names(temp)[last_nonmissing_var]
temp[nonmissing_rows, lnmv] = temp[nonmissing_rows, last_column]
temp[,(last_nonmissing_var+1):ncol(temp)]=NULL
}

SVO <- lapply(1:length(verbs), function(x) { 
for(x in 1:24) {
  x=1
  print(x)
  colx=col.num+x
 # colx = ifelse(temp[verbs[x],col.num+x]!="",col.num+x,col.num+(x-(colx-col.num-1)))
 
  # Problem is alleged!
  a = data.frame(
    "verb" = temp[grep("-V{1}", temp[, colx-1]), 1],
    "subject" = paste(temp[grep("-A0", temp[, colx]), 1], collapse = " "),
    "object" = paste(temp[grep("-A[1-9]+", temp[, colx]), 1], collapse = " ")
  )
 print(dim(a))
 }
  
  )
SVO <- rbindlist(SVO)
  

SVO <- lapply(1:length(verbs), function(x) { 
  colx= col.num + x
  #colx = ifelse(col.num+x<=20, col.num+x, col.num+(x-15))
  
  last_column = names(temp)[ncol(temp)]
  missing_rows = which(temp[[last_column]]=="")
  nonmissing_rows = which(temp[[last_column]]!="")
  
  if (length(missing_rows)==0) {
  } else {
    nc = ncol(temp)
    newvar = matrix(0,nrow=nrow(temp),ncol=(nc-7))
    temp = cbind(temp, newvar)
    temp[missing_rows, (nc-1):(nc+(nc-6)-1)] = temp[missing_rows, 6:nc]
    
    rowX = temp[missing_rows[1],]
    last_nonmissing_var = tail(which(rowX!=""),n=1)
    lnmv = names(temp)[last_nonmissing_var]
    temp[nonmissing_rows, lnmv] = temp[nonmissing_rows, last_column]
    temp[,(last_nonmissing_var+1):ncol(temp)]=NULL
  }
  
  data.frame(
    "verb" = temp[grep("-V{1}", temp[, colx]), 1],
    "subject" = paste(temp[grep("-A0", temp[, colx]), 1], collapse = " "),
    "object" = paste(temp[grep("-A[1-9]+", temp[, colx]), 1], collapse = " ")
  ) })
SVO <- rbindlist(SVO)
}
    
    

