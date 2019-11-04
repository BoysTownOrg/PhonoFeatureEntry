rm(list = ls(all.names = TRUE)) 
library(readxl)
library(rstudioapi)
library(here)
masterfile <-selectFile(caption = "Select study (cancel for new study)", label = "Select",
           path = here("dbfiles"),
           existing = TRUE)


if (is.null(masterfile)){ #start a new data set
  studyfile <- showPrompt(title="Give name for data file",message = "name: " )
  masterfile <- paste0(here("dbfiles"),"/",studyfile,".Rda")
  tmp_xlsx <- selectFile(caption = "Select Excel condition file", label = "Select",
             path = ".", filter = "R data (*.xlsx)",
             existing = TRUE)
  tmp_xlsx <- read_excel(tmp_xlsx)
  numCond <<- ncol(tmp_xlsx)
  conditions <<- names(tmp_xlsx)
  load(file = here("templates","Masterheader.RData"))
  numberCols <- numCond + length(Masterheader)
  Masterdata <- data.frame(matrix(ncol = numberCols,nrow = 0))
  colnames(Masterdata) <- c(conditions,Masterheader)
  save(Masterdata,file = masterfile)
} else { #load previous data set
  load(masterfile)
  conditions <<- names(Masterdata)[1:grep("O1a",names(Masterdata))[1]-1]
  numCond <- length(conditions)
}
print(masterfile)
print(conditions)
runApp()

