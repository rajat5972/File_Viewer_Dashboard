library(shiny)

ui <- fluidPage(
  
  titlePanel("File Viewer"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput(inputId = "file", label = "Upload the file"), 
      helpText("Default, max. file size is 5 MB"),
      tags$hr(), 
      helpText("Select the read.table parameters below"),
      checkboxInput( inputId = "header", label = "Header", value = FALSE),
      checkboxInput(inputId = "stringAsFactors", label = "stringAsFactors", value = FALSE),
      br(),
      radioButtons(inputId = "sep", label = "Seperator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " "), selected = ","),
      br(),
      textOutput("counter"),
      h6("Presented by Rajat"),
      tags$img(src = "gif test.gif", height = 50, width = 100)
    ),
    mainPanel(uiOutput("tb"))
  )
)

server <- function(input, output) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  
  data<- reactive({
    file1<-input$file
    if(is.null(file1)){return()}
    read.table(file = file1$datapath, sep = input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  })
  
  output$about<-renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
  output$datadf<-renderTable({
    if(is.null(data())){return()}
    data()
  })
  
  output$sum<-renderPrint({
    if(is.null(data())){return()}
    summary(data())
  })
  output$tb<-renderUI({
    if(is.null(data()))
      h5("Intro:", br(),br(), tags$video(src = "read.table.mp4", type = "video/mp4", width = "600px", height = "330px", controls = "controls", autoplay = "autoplay", muted = "muted"), br(), "Credit : DataCamp")
    else
      tabsetPanel(type = "tab",
                  tabPanel('Reference', tags$iframe(style="height:400px;width:100%;scrolling=yes", src = "https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/read.table")),
                  tabPanel('About file', tableOutput("about")),
                  tabPanel("Summary", verbatimTextOutput("sum")),
                  tabPanel("Data", tableOutput("datadf")))
  })
  #Hit counter code
  output$counter<- renderText({
    if(!file.exists("counter.Rdata"))#if the counter.Rdata file does not exists
      counter<-0#then create a variable and set the value to be 0
    else#else if the file already exists then
      load(file = "counter.Rdata")#load the file counter.Rdata(which means we have read the variable stored in the file)
    counter<-counter+1# out of the if-else statment, we increment the value by 1
    save(counter, file = "counter.Rdata")#and savea file counter.Rdata with variable counter (if file dont exist then it creates one, else it will overwrite already existing counter.Rdata file)
    paste0("Hits : ", counter)
  })
}

shinyApp(ui, server)

