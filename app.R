

#remember to set working directory

library(shiny)
library(DT)

setwd('/srv/shiny-server/free/data')

appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

shinyApp(
  #UI stuff ---------
  ui <- fluidPage(
    
    #loading screen
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    # Loading message
    div(
      id = "loading-content",
      h2("网页正在打开，您要的密码正在路上…")
    ),
    
    # The main app code goes here
    shinyjs::hidden(
      div(
        id = "app-content"
        #,p("Add text here if necessary")
      )),
    
    wellPanel(
      h2("免费 ofo 单车密码！", align="center"),
      shinyjs::useShinyjs(),
      DT::dataTableOutput("data")
    ),
    
    wellPanel(
      h2("分享你的 ofo 单车密码， 让别人也可以享受免费自行车!", align="center"),
      numericInput("bike_id", "单车号牌", value = "", width = NULL),
      numericInput("password", "单车密码", value = "", width = NULL),
      actionButton("submit", "添加")
    )
  ),
  
  #server stuff--------
  server <- function(input, output) {
    
    # Simulate work being done for 1 second
    Sys.sleep(1)
    
    # Hide the loading message when the rest of the server function has executed
    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
    shinyjs::show("app-content")
    
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
    colnames(data)<-c("单车号牌","密码","点赞数","点赞","差评")
    df <- reactiveValues(data = data)
    
    output$data <- DT::renderDataTable(
        {df$data}, server = FALSE, escape = FALSE, selection = 'none', rownames = FALSE, 
        options = list(
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'),
          pageLength = 5)
    )
     
    
    
    #点赞
    observeEvent(input$hi_button,{
      shinyjs::disable("hi_button")
      selectedRow <- as.numeric(strsplit(input$hi_button, "_")[[1]][2])
      
      df$data[selectedRow,3]<-df$data[selectedRow,3]+1
      
      outputdata <- subset(df$data, select = c("单车号牌","密码","点赞数") )
      colnames(outputdata)<-c("bike_id","password","votes" )
      write.csv(outputdata, file = "data.csv",row.names=FALSE)
    })
    
    #downvote
    observeEvent(input$bye_button,{
      shinyjs::disable("bye_button")
      selectedRow <- as.numeric(strsplit(input$bye_button, "_")[[1]][2])
      
      df$data[selectedRow,3]<-df$data[selectedRow,3]-1
      
      outputdata <- subset(df$data, select = c("单车号牌","密码","点赞数") )
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
        outputdata <- subset(df$data, select = c("单车号牌","密码","点赞数") )
        colnames(outputdata)<-c("bike_id","password","votes" )
        outputdata <- rbind(outputdata,c(input$bike_id,input$password,1))
        write.csv(outputdata, file = "data.csv",row.names=FALSE)
        
        showModal(modalDialog(
          title = "感谢",
          "谢谢你为人民服务!",
          footer = modalButton("团结!")
        ))
      }
      
      if (goodinput==FALSE){
        showModal(modalDialog(
          title = "错误",
          "只能写数据",footer = modalButton("哼!")
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
