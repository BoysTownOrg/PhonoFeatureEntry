
library(shiny)
library(DT)
library(writexl)
library(shinyalert)
library(readxl)

load(file = "resp_seq_names.RData")
textInputRow<-function (inputId, label, value = "", mystyle="", myti="") 
{
  span(style="",
       tags$input(id = inputId, type = "text", value = value, 
                  maxlength=1, size=1,style=mystyle,tabindex=myti ))
}

textInputRow2<-function (inputId, label, value = "", mystyle="", myti="") 
{
  span(style="",
       tags$input(id = inputId, type = "text", value = value, 
                  maxlength=10, size=1,style=mystyle,tabindex=myti ))
}

ui <- fluidRow(
  useShinyalert(),
  tags$label("    Labels:",style='font-family:"Courier New"; white-space: pre'),
  lapply(1:length(resp_seq_names), function(i) {
    textInputRow(paste0('input', i), "", resp_seq_names[[i]],"text-align:center; border-color:transparent; width: 25px;",-1)
  }),
  tags$br(),
  tags$label("    Target:",style='font-family:"Courier New";white-space: pre'),
  lapply(1:length(resp_seq_names), function(i) {
    textInputRow(paste0('T', resp_seq_names[[i]]), paste0('Input', i),"","text-align:center; width: 25px;")
  }),
  tags$br(),
  tags$label("  Response:",style='font-family:"Courier New";white-space: pre'),
  lapply(1:length(resp_seq_names), function(i) {
    textInputRow(paste0('R', resp_seq_names[[i]]), paste0('Input', i),"","text-align:center; width: 25px;")
  }),
  tags$br(),
  tags$label("    Counts:",style='font-family:"Courier New"; white-space: pre'),
  lapply(1:length(resp_seq_names), function(i) {
    textInputRow(paste0('counts', i), "", '0',"text-align:center; border-color:transparent; width: 25px;",-1)
  }),
  tags$br(),
  tags$label("Proportion:",style='font-family:"Courier New"; white-space: pre'),
  textInputRow('propval', "", '0.0',"text-align:center; border-color:transparent; width: 25px;"),
  tags$br(),
  textInputRow('dbfilename', "", '-----',"text-align:left; border-color:transparent; width: 200px;"),
  tags$br(),
  actionButton("addProp", "Save Calc"),
  actionButton("insertLast", "Insert previous entry"),
  textInputRow('lasttext', "", '-----',"text-align:center; border-color:transparent; width: 100px;"),
  actionButton("insertLastHighlighted", "Insert last highlighted"),
  textInputRow('lastHilight', "", '-----',"text-align:center; border-color:transparent; width: 100px;"),
  tags$br(),
  textInput("subjectID","ID code:"),
  textInput("testDay","Test day:"),
  textInput("comments","Comments:"),
  tags$br(),
  actionButton("saveFile", "Save File"),
  textInput("fileName","File name:"),
  fileInput("addTargets","Select word sets:"),
  fileInput("addConditions","Select conditions:"),

  DTOutput('tbl')
)



