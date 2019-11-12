# initialize and define functions
mypageLength <- 100
print(conditions)
OverwriteFile <- function(x){
  if (x){
    Masterdata[, "PropMatch"] <-
      as.numeric(Masterdata[, "PropMatch"])
    write_xlsx(Masterdata,
               path = fileToSave,
               col_names = TRUE)
  }
}
load(file = masterfile)
load(file = "resp_seq_names.RData")
source("feat_assign.r")
Masterdata <-
  Masterdata[apply(Masterdata, 1, function(x) {
    !all(is.na(x))
  }), ]
numRowsStart <- nrow(Masterdata) + 1
info <- 1
lastinfo <- 1
lasthighlight <- 0
fileToSave <- ""

server <- function(input, output, session) {
  updateTextInput(session, 'dbfilename' , value = paste0("File: ",rev(strsplit(here(masterfile),.Platform$file.sep)[[1]])[1]))
  
  #data table
  output$tbl <- renderDT({
    DT::datatable(
      Masterdata,
      extensions = 'Scroller',
      editable = TRUE,
      selection = "single",
      options = list(
        pageLength = mypageLength,
        stateSave = TRUE,
        scroller = TRUE,
        paging = TRUE,
        scrollY = '500px',
        scrollX = '500px',
        dom = 'tip'
      )
    )
  }, server = TRUE)
  proxytbl = dataTableProxy('tbl')
  
  #when DT row is clicked
  observeEvent(input$tbl_row_last_clicked, {
    lasthighlight <<- info
    tmplist <- list()
    ii <- 1
    for (seg in colnames(Masterdata)[(numCond+1):(numCond+1+length(resp_seq_names))]) {
      tmp <-  Masterdata[lasthighlight, paste0('R', seg) ]
      if (!identical(tmp, character(0))) {
        tmplist[[ii]] <- tmp
        ii <- ii+1
      }
    }
    lastHliteword <<- paste(tmplist,collapse="")
    
    updateTextInput(session, 'lastHilight' , value = lastHliteword)
    info <<- input$tbl_row_last_clicked
    updateTextInput(session,"testDay",value = Masterdata[info,"Day"])
    for (seg in colnames(Masterdata)[(numCond+1):(numCond+1+length(resp_seq_names))]) {
      updateTextInput(session, paste0('T', seg) , value = Masterdata[info, seg])
    }
    for (seg in colnames(Masterdata)[(numCond+1):(numCond+1+length(resp_seq_names))]) {
      updateTextInput(session, paste0('R', seg) , value = Masterdata[info, seg])
    }
    
  })
  
  #update after DT state change
  observeEvent(input$tbl_state, {
    state_out <<- input$tbl_state
    for (segnum in 1:length(resp_seq_names)) {
      updateTextInput(session, paste0('T', resp_seq_names[segnum]) , value = '')
      updateTextInput(session, paste0('R', resp_seq_names[segnum]) , value = '')
      updateTextInput(session, paste0('counts', segnum) , value = '')
      
    }
  })
  
  #insert previously entered value at current line
  observeEvent(input$insertLast, {
    for (seg in colnames(Masterdata)[(numCond+1):(numCond+1+length(resp_seq_names))]) {
      updateTextInput(session, paste0('R', seg) , value = Masterdata[lastinfo, paste0('R', seg) ])
    }
  })
  #insert previously higlighted line at current line
  observeEvent(input$insertLastHighlighted, {
    for (seg in colnames(Masterdata)[(numCond+1):(numCond+1+length(resp_seq_names))]) {
      updateTextInput(session, paste0('R', seg) , value = Masterdata[lasthighlight, paste0('R', seg) ])
    }
  })
  
  #"Save Calc"  add current entry to database 
  observeEvent(input$addProp, {
    lastinfo <<- info
    tmplist <- list()
    ii <- 1
    for (segnum in 1:length(resp_seq_names)) {
      tmp <- eval(parse(text = paste0('input$R', resp_seq_names[segnum])))
      if (!identical(tmp, character(0))) {
        tmplist[[ii]] <- tmp
        ii <- ii+1
      }
    }
    lastword <- paste(tmplist,collapse="")
    updateTextInput(session, 'lasttext' , value = lastword)
  
    Masterdata[info, "PropMatch"] <<- as.numeric(input$propval)
    Masterdata[info, "Comments"] <<- input$comments
    Masterdata[info, "ID"] <<- input$subjectID
    Masterdata[info, "Day"] <<- input$testDay
    for (segnum in 1:length(resp_seq_names)) {
      Masterdata[info, paste0('R', resp_seq_names[segnum])] <<-
        eval(parse(text = paste0('input$R', resp_seq_names[segnum])))
      Masterdata[info, paste0('C', resp_seq_names[segnum])] <<-
        as.numeric(eval(parse(text = paste0('input$counts', segnum))))
    }
    replaceData(proxytbl, Masterdata, resetPaging = FALSE)
    save(Masterdata, file = masterfile)
    load(file = masterfile)
    reloadData(proxytbl, resetPaging = FALSE)
    selectRows(proxytbl,c(info))
  })
  
  #export state of current database to an Excel file
  observeEvent({
    input$saveFile
  }, {
    fileName <- input$fileName
    if (length(grep('\\.xlsx', fileName, ignore.case = TRUE)) > 0) {
      fileToSave <<- fileName
    } else {
      fileToSave <<- paste0(fileName, '.xlsx')
    }
    
    if (!file.exists(fileToSave)) {
      Masterdata[, "PropMatch"] <-
        as.numeric(Masterdata[, "PropMatch"])
      write_xlsx(Masterdata,
                 path = fileToSave,
                 col_names = TRUE)
    } else {
      shinyalert(
        title = "FileExists",
        text = "File Exists!",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "Overwrite",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "Cancel",
        timer = 0,
        imageUrl = "",
        callbackR = function(x) { OverwriteFile(x) },
        animation = FALSE
      )
    }
  })
  
  # add word sets
  observeEvent({
    input$addTargets
  },
  {
    tmp_xlsx <- read_excel(paste0(input$addTargets$datapath))
    Masterdata[info:(info + nrow(tmp_xlsx) - 1), names(tmp_xlsx)] <<- #numRowsStart:(numRowsStart + nrow(tmp_xlsx) - 1)
      tmp_xlsx
    replaceData(proxytbl, Masterdata, resetPaging = FALSE)
    save(Masterdata, file = masterfile)
    load(file = masterfile)
    reloadData(proxytbl, resetPaging = FALSE)
  })
  
  # add conditions file
  observeEvent({
    input$addConditions
  },
  {
    tmp_xlsx <- read_excel(paste0(input$addConditions$datapath))
    tmp_intersect <- intersect(names(tmp_xlsx), names(Masterdata))
    if ((length(tmp_intersect) == length(names(tmp_xlsx))) && (all.equal(names(tmp_xlsx),tmp_intersect))){
      Masterdata[numRowsStart:(numRowsStart + nrow(tmp_xlsx) - 1), names(tmp_xlsx)] <<-
        tmp_xlsx
      numRowsStart <<- numRowsStart + nrow(tmp_xlsx)
      replaceData(proxytbl, Masterdata, resetPaging = FALSE)
      save(Masterdata, file = masterfile)
      load(file = masterfile)
      reloadData(proxytbl, resetPaging = FALSE)
    } else {
      shinyalert(
        title = "Conditions Differ",
        text = "Conditions Differ between conditions file and current data",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Okay",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "Cancel",
        timer = 0,
        imageUrl = "",
        animation = FALSE
      )
    }
  })
  
  # react to input in response boxes
  observeEvent({
    input$RO1a
    input$RO1b
    input$RO1c
    input$RV1
    input$RStr1
    input$RC1a
    input$RC1b
    input$RC1c
    input$RO2a
    input$RO2b
    input$RO2c
    input$RV2
    input$RStr2
    input$RC2a
    input$RC2b
    input$RC2c
    input$RO3a
    input$RO3b
    input$RO3c
    input$RV3
    input$RStr3
    input$RC3a
    input$RC3b
    input$RC3c
    input$RO4a
    input$RO4b
    input$RO4c
    input$RV4
    input$RStr4
    input$RC4a
    input$RC4b
    input$RC4c
    input$RO5a
    input$RO5b
    input$RO5c
    input$RV5
    input$RStr5
    input$RC5a
    input$RC5b
    input$RC5c
  },
  {
    totalfeatures <- 0
    numtargetchars <- 0
    for (segnum in 1:length(resp_seq_names)) {
      tchar <-
        eval(parse(text = paste0('input$T', resp_seq_names[segnum])))
      rchar <-
        eval(parse(text = paste0('input$R', resp_seq_names[segnum])))
      if ((tchar != '') && (rchar != '')) {
        numtargetchars <- numtargetchars + 1
        t_feat <- feat_assign(tchar)
        r_feat <- feat_assign(rchar)
        thiscount <- sum(t_feat & r_feat)
        updateTextInput(session,
                        paste0('counts', segnum) ,
                        value = paste0(thiscount))
        totalfeatures <- totalfeatures + thiscount
      } else if ((tchar != '') && (rchar == '')) {
        numtargetchars <- numtargetchars + 1
        updateTextInput(session,
                        paste0('counts', segnum) ,
                        value = paste0(0))
      } else if ((tchar == '') && (rchar != '')) {
        updateTextInput(session,
                        paste0('counts', segnum) ,
                        value = paste0(0))
      } else if ((tchar == '') && (rchar == '')) {
        updateTextInput(session,
                        paste0('counts', segnum) ,
                        value = paste0(0))
      }
    }
    updateTextInput(session, 'propval' , value = totalfeatures / (3 * numtargetchars))
  })
  
}
