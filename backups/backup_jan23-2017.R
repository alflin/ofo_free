

#remember to set working directory

library(shiny)
library(DT)

setwd('/srv/shiny-server/free/data')

shinyApp(
  #UI stuff ---------
  ui <- fluidPage(
    wellPanel(
      h2("免费 ofo 单车密码！", align="center"),
      shinyjs::useShinyjs(),
      DT::dataTableOutput("data")
    ),
    
    wellPanel(
      h2("分享你的 ofo 单车密码， 让别人也可以享受免费自行车!", align="center"),
      numericInput("bike_id", "bike id", value = "", width = NULL),
      numericInput("password", "bike password", value = "", width = NULL),
      actionButton("submit", "Add")
    )
  ),
  
  #server stuff--------
  server <- function(input, output) {
    
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    
    datafile <- read.csv(file="data.csv", header=TRUE, sep=",")
    
    data <- data.frame(
      Upvote = shinyInput(actionButton, nrow(datafile), 'button_', label = "点赞", onclick = 'Shiny.onInputChange(\"hi_button\",  this.id)' ),
      Downvote = shinyInput(actionButton, nrow(datafile), 'button_', label = "差评", onclick = 'Shiny.onInputChange(\"bye_button\",  this.id)' )
      )
    
    #creating data------
    data <- cbind(datafile,data)
    #change to chinese baby
    colnames(data)<-c("自行车 ID","密码","点赞数","点赞","差评")
    df <- reactiveValues(data = data)
    
    output$data <- DT::renderDataTable(
      withProgress(message='Loading',{df$data}), server = FALSE, escape = FALSE, selection = 'none', rownames = FALSE
    )
    
    
    #点赞
    observeEvent(input$hi_button,{
      shinyjs::disable("hi_button")
      selectedRow <- as.numeric(strsplit(input$hi_button, "_")[[1]][2])
      
      df$data[selectedRow,3]<-df$data[selectedRow,3]+1
      
      outputdata <- subset(df$data, select = c("自行车 ID","密码","点赞数") )
      colnames(outputdata)<-c("bike_id","password","votes" )
      write.csv(outputdata, file = "data.csv",row.names=FALSE)
    })
    
    #downvote
    observeEvent(input$bye_button,{
      shinyjs::disable("bye_button")
      selectedRow <- as.numeric(strsplit(input$bye_button, "_")[[1]][2])
      
      df$data[selectedRow,3]<-df$data[selectedRow,3]-1
      
      outputdata <- subset(df$data, select = c("自行车 ID","密码","点赞数") )
      colnames(outputdata)<-c("bike_id","password","votes" )
      write.csv(outputdata, file = "data.csv",row.names=FALSE)
    })
    
    
    
    #submit buttons here
    
    observeEvent(input$submit,{

      goodinput = typeof(input$bike_id) == 'integer' & typeof(input$password) == 'integer'
      cat(typeof(input$bike_id))
      cat(typeof(input$password))
      
      cat(goodinput)      
      #if success---
      if (goodinput){
        outputdata <- subset(df$data, select = c("自行车 ID","密码","点赞数") )
        colnames(outputdata)<-c("bike_id","password","votes" )
        outputdata <- rbind(outputdata,c(input$bike_id,input$password,1))
        write.csv(outputdata, file = "data.csv",row.names=FALSE)
        
        showModal(modalDialog(
          title = "感谢",
          "谢谢你为人民服务!"
        ))
      }
      
      if (goodinput==FALSE){
        showModal(modalDialog(
          title = "Error",
          "只能写数据"
        ))
      }
      
    })
    
  }
)

#example of disabling a button

# library(shiny)
# runApp(shinyApp(
#   ui = fluidPage(
#     shinyjs::useShinyjs(),
#     numericInput("test", "Test", 5),
#     actionButton("submit", "Choose")
#   ),
#   server = function(input, output, session) {
#     observeEvent(input$submit, {
#       shinyjs::disable("submit")
#     })
#   }
# ))

#test other shit

# data <- data.frame(
#   Delete = shinyInput(actionButton, 10, 'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
#   Upvote = shinyInput(actionButton, 10, 'button_', label = "Fun", onclick = 'Shiny.onInputChange(\"hi_button\",  this.id)' ),
#   Bike_ID = 1:10,
#   Passcode = c("A", "B", "C", "D", "E"),
#   Likes = 1:10,
#   stringsAsFactors = FALSE,
#   row.names = 1:10
# )
