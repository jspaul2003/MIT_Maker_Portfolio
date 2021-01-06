#KNOWN ISSUES:

#TO DO (big things):
#-javascript cookies?


#PACKAGES:
library(shiny)
library(beepr)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinyAce)
library(RMySQL)
library(DBI)
library(pool)
library(dplyr)
library(stringr)
library(openssl)


#SOURCES:
source("EuclidToolkit.R")
source("SphericalToolkit.R")


#GLOBAL VARIABLES:
numSlides <- 5
maxSlides <- 10

#FUNCTIONS:
#isValidEmail
#Checks if input is an email
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}


#Length
#Takes in 2 points and finds length between them
Length<-function(A,B){
  return( ((A[1]-B[1])^2+(A[2]-B[2])^2)^0.5 )
}


#StringVecInit
#Initializes an empty string vetor of length N
StringVecInit <- function(N){
  vec = vector(,N);
  vec[1:N]=""
  return(vec)
}


#RunCode
#Takes a String (which is code) and runs it
RunCode <- function(code){
  toRun=parse(text=code)
  eval(toRun)
}


#Previewer
#Previews code for a plot
#Takes as input: slide object and code history 
Previewer<-function(slides, user_code){  
  slidenumber=strtoi(strsplit(slides,"#")[[1]][2])
  return(renderPlot({
    RunCode(user_code$lines[1:slidenumber])
  }))  
}


#Plotter
#Plots code for a plot
#Takes as input: slide number and code history 
Plotter<-function(slidenumber, user_code){  
  return(renderPlot({
    RunCode(user_code$lines[1:slidenumber])
  }))  
}


#PreviewerBack
#Reverts a Preview, running all code up to current slide
#Takes as input: slide object and code history
PreviewerBack<-function(slides, user_code){
  slidenumber=strtoi(strsplit(slides,"#")[[1]][2])
  if(slidenumber>1){
    return(renderPlot({
      RunCode(user_code$lines[1:slidenumber-1])
    }))  
  }
  else{
    return(renderPlot({
      blank.euclid.plot(3, TRUE)
    }))  
  }
}





# Define UI for application that draws a histogram
header <- dashboardHeader(title = "Greek Propositions")

sidebar <- dashboardSidebar(  
  tags$div(
    id = "SP_Edit",
    condition = "input.Present%2==0",
    textInput("title", "Make Title", placeholder = "Untitled"),
    #actionButton("titleButton", "Make Title"),
    sliderInput("numSlides", "Number of slides", min=1, max=10, value=5, ticks=FALSE),
    actionButton("addSlidesButton", "Add Slide +"),
    checkboxInput("SecondPlot", label = "Second Plot?", value = F),
  ),  
  shinyjs::hidden(
    tags$div(
      id="SP_Pres",
      tags$div(id="editing_div",
               actionButton("Edit","Edit mode")
      ),  
    )
  ),
  
  tags$div(
    id="SignUp",
    textInput("email","Email", placeholder = "Email"),
    passwordInput("password","Password", placeholder = "Password"),
    textOutput("ErrorSU"),
    actionButton("SignUpBTN", "Sign Up"),
  ),

  tags$div(
    id="Login",
    textInput("Email","Email", placeholder = "Email"),
    passwordInput("Password","Password", placeholder = "Password"),
    textOutput("ErrorLI"),
    actionButton("LogInBTN", "Log In"),
  ),

  shinyjs::hidden(
    #We can display users saved presentations here
    tags$div(
      id="Dashboard",
      tags$label( class="control-label", id="acclab","Account"),
      actionButton("LogOutBTN", "Log Out"),
      tags$div(id="SavePres2",
        actionButton("SavePres", "Save New Presentation"),
      ),
      selectInput("LoadPres", label = h3("Load Presentation"), 
                  choices = "New", 
                  selected = "New"),
      shinyjs::hidden(
        tags$div(
          id="dash",
          actionButton("UpdateP", "Update Presentation"),
          actionButton("DeleteP", "Delete Presentation")
        )
      )
    )
  )
  
  
)

body <- dashboardBody(
  
  #HTML, JS, CSS General Linking
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.quilljs.com/1.3.6/quill.snow.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "stuff.css"),
    tags$script(src="https://cdn.quilljs.com/1.3.6/quill.js"),
    tags$script(src="http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.js")
  ),
  
  # Application title
  titlePanel(uiOutput("proposition")),
  
  tags$div(
    id="MP_Edit",
    
    tags$div(id="presenting_div",
             actionButton("Present","Presentation Mode!")
    ),  
    
    #Slide selection input
    selectInput("slides", label = h3("Slides"), 
                choices = "", 
                selected = 1),
    
    #HTML editor for greek text 
    h3("Insert Greek text here:"),
    
    includeHTML("www/stuff.html"),
    
    #HTML editor for english translation
    #TO DO: (when presenting need an extra button to show translation or hide)
    h3("Insert English Translation here:"),
    includeHTML("www/stuff2.html"),
    
    h3("Insert Code for this slide's plot here:"),
    includeHTML("www/ace.html"),
    
    tags$br(),
    
    tags$ul(
      class="horul",
      tags$li(class="horli", actionButton("Preview", "Preview Plot")),
      tags$li(class="horli", id="horliright", actionButton("GoBack", "Undo")),
      tags$li(class="horli", id="horliright", actionButton("SaveSlide", "Save Slide"))
    ),  
    
    
    # Show plot and text
    #verbatimTextOutput("message"),
    htmlOutput("message"),
    verbatimTextOutput("greek"),
    tags$br(),
    
    plotOutput("pplot"),
    
    
    conditionalPanel(
      condition = "input.SecondPlot == 1",
      h3("Insert Code for this slide's other plot here:"),
      includeHTML("www/ace2.html"),
      
      tags$ul(
        class="horul",
        tags$li(class="horli", actionButton("Preview2", "Preview Plot")),
        tags$li(class="horli", id="horliright", actionButton("GoBack2", "Go Back"))
      ),  
      
      tags$br(),
      
      plotOutput("pplot2"),
      
    ),
  ),
  
  
  shinyjs::hidden(
    tags$div(
      id="MP_Pres",
      
      tags$div(
        id="greek_text",
        htmlOutput("GreekText")
      ),
      
      tags$br(),
      
      tags$div(
        id="plot1",
        plotOutput("ppplot"),
      ),
      
      tags$br(),
      
      conditionalPanel(
        condition = "input.SecondPlot == 1",
        tags$div(
          id="plot2",
          plotOutput("ppplot2"),
        )
      ),
      
      tags$br(),
      
      tags$div(
        id="ShowTransContainer",
        actionButton("ShowTrans", "Show English Translation")
      ),
      
      tags$br(),
      
      shinyjs::hidden(
        tags$div(
          id="english_translation",
          actionButton("HideTrans", "Hide"),
          tags$br(),
          htmlOutput("EnglishText")
        )
      ),
      
      tags$br(),
      uiOutput("presentationSlideCounter"),
      tags$ul(
        class="horul",
        tags$li(class="horli", actionButton("PreviousP", "←")),
        tags$li(class="horli", id="horliright", actionButton("NextP", "→"))
      ),  
    )
  ),
  
  
  tags$div(
    # id="console",
    # 
    # tags$br(),
    # 
    # #Console should be removed/hidden on final version
    useShinyjs(),
    # h3("R Console:"),
    # tags$div(
    #   class="codewrapper",
    #   runcodeUI(code = "", type="textarea")
    # ),
    # tags$br(),
  )
  
  
  
  
  
)

ui <- dashboardPage(header, sidebar, body)

code <- "Enter Code Here"

server <- function(session, input, output) {
  
  greek_lines=reactiveValues()
  english_lines=reactiveValues()
  user_code=reactiveValues()
  user_code2=reactiveValues()
  
  #DEFAULT SLIDE SETTINGS
  
  output$pplot <- renderPlot(blank.euclid.plot(3, TRUE))
  
  output$pplot2 <- renderPlot(blank.euclid.plot(3, TRUE))
  
  output$proposition <- renderUI("Untitled")
  
  greek_lines$lines=StringVecInit(maxSlides)
  english_lines$lines=StringVecInit(maxSlides)
  user_code$lines=StringVecInit(maxSlides)
  user_code2$lines=StringVecInit(maxSlides)
  
  updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides))
  
  runcodeServer();
  
  #Presentation code:
  
  #Tracks which slide user is on
  Pres_track=reactiveValues()
  Pres_track$slide=1
  
  # #Initiate Presenting mode
  observeEvent(input$Present,{
    shinyjs::hide("SP_Edit")
    shinyjs::hide("MP_Edit")
    shinyjs::show("SP_Pres")
    shinyjs::show("MP_Pres")
    Pres_track$slide=1
  })
  
  #Show english translation
  observeEvent(input$ShowTrans,{
    shinyjs::show("english_translation")
    shinyjs::hide("ShowTransContainer")
  })
  
  #Hide english translation
  observeEvent(input$HideTrans,{
    shinyjs::hide("english_translation")
    shinyjs::show("ShowTransContainer")
  })
  
  #Initiate Editing mode
  observeEvent(input$Edit,{
    shinyjs::show("SP_Edit")
    shinyjs::show("MP_Edit")
    shinyjs::hide("SP_Pres")
    shinyjs::hide("MP_Pres")
  })
  
  #Display Text
  output$GreekText <- renderUI({
    HTML(greek_lines$lines[Pres_track$slide])
  })
  
  output$EnglishText <- renderUI({
    HTML(english_lines$lines[Pres_track$slide])
  })
  
  #Do the plots at start
  output$ppplot<-Plotter(Pres_track$slide, user_code)
  output$ppplot2<-Plotter(Pres_track$slide, user_code2)
  
  #Move a slide 
  observeEvent(input$NextP,{
    if(Pres_track$slide < numSlides) {
      Pres_track$slide=(Pres_track$slide+1)%%(length(greek_lines$lines)+1) + 1*((Pres_track$slide+1)%%(length(greek_lines$lines)+1)==0)
      output$ppplot<-Plotter(Pres_track$slide, user_code)
      output$ppplot2<-Plotter(Pres_track$slide, user_code2)
      output$presentationSlideCounter <- renderUI(paste("Slide", Pres_track$slide, "out of", numSlides))
    }
  })
  
  observeEvent(input$PreviousP,{
    if(Pres_track$slide > 1) {
      Pres_track$slide=(Pres_track$slide-1)%%(length(greek_lines$lines)+1) + length(greek_lines$lines)*((Pres_track$slide-1)%%(length(greek_lines$lines)+1)==0)
      output$ppplot<-Plotter(Pres_track$slide, user_code)
      output$ppplot2<-Plotter(Pres_track$slide, user_code2)
      output$presentationSlideCounter <- renderUI(paste("Slide", Pres_track$slide, "out of", numSlides))
    }
  })
  
  #Rest of the code
  
  tracke=reactiveValues()
  tracke$count=-1

  
  #Make title
  observeEvent(input$title,{
    if(input$title==""){
      output$proposition<-renderUI("Untitled")
    }
    else{
      output$proposition <- renderUI(input$title)
    }
  })
  
  #Adding Slides
  observeEvent(input$addSlidesButton, {
    if(numSlides < 10) {
      numSlides <<- numSlides + 1
    }
    output$pplot <- renderPlot(blank.euclid.plot(3, TRUE))
    updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides))
    updateSliderInput(session, "numSlides", value = numSlides)

  })
  
  #Slider
  observeEvent(input$numSlides, {
    numSlides <<- input$numSlides
    output$pplot <- renderPlot(blank.euclid.plot(3, TRUE))
    updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides))

  })
  
  #Changing which slide number user is on
  #sends message to js code which changes js editors
  observeEvent(input$slides,{
    
    #update plots
    if(!is.na(strtoi(strsplit(input$slides,"#")[[1]][2]))){
      output$pplot<-PreviewerBack(input$slides, user_code)
      output$pplot2<-PreviewerBack(input$slides, user_code2)
    }
    
    slidenumber=strtoi(strsplit(input$slides,"#")[[1]][2])
    session$sendCustomMessage("greek_lines_stuff", greek_lines$lines[slidenumber])
    session$sendCustomMessage("english_lines_stuff", english_lines$lines[slidenumber])
    session$sendCustomMessage("user_code_stuff", user_code$lines[slidenumber])
    session$sendCustomMessage("user_code_stuff2", user_code2$lines[slidenumber])
    presentations$wason2=slidenumber
  })
  
  #Saving a slide
  #Saves the Greek, English and Code input
  observeEvent(input$SaveSlide, {
    slidenumber=strtoi(strsplit(input$slides,"#")[[1]][2])
    #Need to avoid a replacement length = 0 error
    if(length(input$mydata)>0){
      greek_lines$lines[slidenumber]=input$mydata 
    }
    if(length(input$englishtrans)>0){
      english_lines$lines[slidenumber]=input$englishtrans
    }
    if(length(input$user_code)>0){
      user_code$lines[slidenumber]=input$user_code
    }  
    if(length(input$user_code)>0){
      user_code2$lines[slidenumber]=input$user_code2
    }  
  })
  
  #PLOT 1
  
  #Previewing code
  observeEvent(input$Preview, {
    
    if(length(input$user_code)>0){
      user_code$lines[strtoi(strsplit(input$slides,"#")[[1]][2])]=input$user_code
    }  
    output$pplot<-Previewer(input$slides, user_code)
  })
  
  #Going back from preview
  observeEvent(input$GoBack, {
    output$pplot<-PreviewerBack(input$slides, user_code)
  })
  
  #PLOT 2
  
  #Previewing code
  observeEvent(input$Preview2, {
    
    if(length(input$user_code2)>0){
      user_code2$lines[strtoi(strsplit(input$slides,"#")[[1]][2])]=input$user_code2
    }  
    output$pplot2<-Previewer(input$slides, user_code2)
  })
  
  #Going back from preview
  observeEvent(input$GoBack2, {
    output$pplot2<-PreviewerBack(input$slides, user_code2)
  })
  
  
  #SQL STUFF:
  
  sqlstuff=reactiveValues()
  sqlstuff$dbname = ""
  sqlstuff$host = ""
  sqlstuff$username = ""
  sqlstuff$password = ""
  
  logged_in=reactiveValues()
  logged_in$user=-1
  logged_in$logged=F
  
  presentation=reactiveValues()
  presentation$saved=F
  
  

  
  observeEvent(input$SavePres, {
    
    
    slidenumber=strtoi(strsplit(input$slides,"#")[[1]][2])
    #Need to avoid a replacement length = 0 error
    if(length(input$mydata)>0){
      greek_lines$lines[slidenumber]=input$mydata 
    }
    if(length(input$englishtrans)>0){
      english_lines$lines[slidenumber]=input$englishtrans
    }
    if(length(input$user_code)>0){
      user_code$lines[slidenumber]=input$user_code
    }  
    if(length(input$user_code)>0){
      user_code2$lines[slidenumber]=input$user_code2
    }  
    
    
    
    
    
    
    General <- dbPool(drv = RMySQL::MySQL(),
                      dbname = sqlstuff$dbname,
                      host = sqlstuff$host,
                      username = sqlstuff$username,
                      password = sqlstuff$password,
                      port = 3306)
    
    if(input$title==""){
      title='Untitled'
    }
    else{
      title=input$title
    }
    
    
    #FLAW: 2 users under same email save something at same time
    #current (bad) fix is overwrite is true
    BARCODE= paste("presentation", 1 , sub(" ",'',Sys.time()))
    BARCODE=sub(" ",'',BARCODE)
    BARCODE=sub(" ",'',BARCODE)
    BARCODE=str_replace_all(BARCODE, "[^[:alnum:]]", "")
    BARCODE=sub(" ",'',BARCODE)
    
    #Junction1 1:M setup
    query <- sprintf<-(paste("INSERT INTO `Junction1` (`User_ID`, `PRESENTATION_TABLE_NAME`, `TITLE`, `NSLIDES`) VALUES ('",
                             logged_in$user, " ','", BARCODE, "','", title, " ','", numSlides, "');", sep = ""))
    dbExecute(General, query)
    
    #New Table for Presentation
    pres.data <- data.frame(user_code$lines, user_code2$lines, greek_lines$lines, english_lines$lines)
    pres.data$ID <- seq.int(nrow(pres.data))
    colnames(pres.data)=c("CODE1", "CODE2", "GREEK", "ENGLISH",  "ID")
    
    dbWriteTable(General, BARCODE, value=pres.data, row.names = FALSE,overwrite=T)
    
    poolClose(General)
    
    presentations$wason=BARCODE
    presentations$refresh=T
  
    
    
    
    })
    
  
  observeEvent(input$SignUpBTN,{
    
    General <- dbPool(drv = RMySQL::MySQL(),
                      dbname = sqlstuff$dbname,
                      host = sqlstuff$host,
                      username = sqlstuff$username,
                      password = sqlstuff$password,
                      port = 3306)
    
    if(nrow(dbGetQuery(General, paste0("SELECT * FROM Users WHERE Email = '", input$email ,"'")))==0 && isValidEmail(input$email) && nchar(input$password)>5){
      query <- sprintf<-(paste("INSERT INTO `Users` (`Email`, `Password`) VALUES ('",
                               input$email, " ','", sha512(input$password), "');", sep = ""))
      dbExecute(General, query)
      shinyjs::show("Dashboard")
      shinyjs::hide("SignUp")
      shinyjs::hide("Login")
      logged_in$user=dbGetQuery(General, paste0("SELECT `ID` FROM Users WHERE Email = '", input$Email ,"'"))[[1]]
      logged_in$logged=T
    }
    else{
      errors=c()
      if(nrow(dbGetQuery(General, paste0("SELECT * FROM Users WHERE Email = '", input$email ,"'")))>0){
        errors=c(errors,"Email already taken ")
      }
      if(!isValidEmail(input$email)){
        errors=c(errors,"Email is not valid ")
      }
      if(nchar(input$password)<6){
        errors=c(errors,"Password requires 6 characters minimum ")
      }
      output$ErrorSU<-renderText(errors[1:length(errors)])
    }
    
    poolClose(General)
    
  })
  
  observeEvent(input$LogInBTN,{
    
    General <- dbPool(drv = RMySQL::MySQL(),
                      dbname = sqlstuff$dbname,
                      host = sqlstuff$host,
                      username = sqlstuff$username,
                      password = sqlstuff$password,
                      port = 3306)
    
    if(nrow(dbGetQuery(General, paste0("SELECT * FROM Users WHERE Email = '", input$Email ,"'")))==1 && dbGetQuery(General, paste0("SELECT `Password` FROM Users WHERE Email = '", input$Email ,"'"))[[1]]==sha512(input$Password)){
      shinyjs::show("Dashboard")
      shinyjs::hide("SignUp")
      shinyjs::hide("Login")
      logged_in$user=dbGetQuery(General, paste0("SELECT `ID` FROM Users WHERE Email = '", input$Email ,"'"))[[1]]
      logged_in$logged=T
    }
    else{
      if(nrow(dbGetQuery(General, paste0("SELECT * FROM Users WHERE Email = '", input$Email ,"'")))==0){
        errors="No account under this email exists"
      }
      else{
        errors="The password you entered is not correct"
      }
      output$ErrorLI<-renderText(errors)
    }
    
    poolClose(General)
    
  })
  
  observeEvent(input$LogOutBTN,{
    General <- dbPool(drv = RMySQL::MySQL(),
                      dbname = sqlstuff$dbname,
                      host = sqlstuff$host,
                      username = sqlstuff$username,
                      password = sqlstuff$password,
                      port = 3306)
    
    
    shinyjs::hide("Dashboard")
    shinyjs::show("SignUp")
    shinyjs::show("Login")
    logged_in$user=-1
    logged_in$logged=F
    
    empty=dbGetQuery(General, paste0("SELECT * FROM EMPTY"))
    user_code$lines=empty$CODE1
    user_code2$lines=empty$CODE2
    greek_lines$lines=empty$GREEK
    english_lines$lines=empty$ENGLISH
    numSlides=5
    
    updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides),selected = "Slide #1")
    
    output$proposition <- renderUI("Untitled")
    updateTextInput(session,"title","Make Title", "",placeholder = "Untitled")
    
    slidenumber=1
    session$sendCustomMessage("greek_lines_stuff", greek_lines$lines[slidenumber])
    session$sendCustomMessage("english_lines_stuff", english_lines$lines[slidenumber])
    session$sendCustomMessage("user_code_stuff", user_code$lines[slidenumber])
    session$sendCustomMessage("user_code_stuff2", user_code2$lines[slidenumber])
    
    
    poolClose(General)
  })
  
  #Loading, updating and deleting presentations
  presentations=reactiveValues()
  presentations$list=c()
  presentations$nslides=c(5)
  presentations$refresh=F
  presentations$wason="New"
  presentations$wason2=1
  
  observe({
    if(logged_in$user!=-1){
      presentations$refresh=T
    }
  })
  
  observe({
    if(presentations$refresh==T){
      
      General <- dbPool(drv = RMySQL::MySQL(),
                        dbname = sqlstuff$dbname,
                        host = sqlstuff$host,
                        username = sqlstuff$username,
                        password = sqlstuff$password,
                        port = 3306)
      
      
      my_pres=dbGetQuery(General, paste0("SELECT * FROM Junction1 WHERE USER_ID = '", logged_in$user ,"'"))
      if(nrow(my_pres)>=1){
        
        presentations$list=c("New", my_pres$TITLE)
        presentations$tables=c("New", my_pres$PRESENTATION_TABLE_NAME)
        list=as.list(presentations$tables)
        names(list)=presentations$list
        updateSelectInput(session, "LoadPres", label = "Load Presentation", choices = list )
        
        presentations$nslides=c("5",my_pres$NSLIDES)
        
      }
      else{
        presentations$list=c("New")
        presentations$tables=c("New")
        list=as.list(presentations$tables)
        presentations$wason="New"
        names(list)=presentations$list
      }

      updateSelectInput(session, "LoadPres", label = "Load Presentation", choices = list,selected=presentations$wason)
      
      
      

      
      
      
      poolClose(General)
      
      presentations$wason="New"
      presentations$refresh=F
      presentations$refresh2=T
    }
  })  
  
  observeEvent(input$LoadPres, {
    
    
    General <- dbPool(drv = RMySQL::MySQL(),
                      dbname = sqlstuff$dbname,
                      host = sqlstuff$host,
                      username = sqlstuff$username,
                      password = sqlstuff$password,
                      port = 3306)
    
    if(input$LoadPres=="New"){
      empty=dbGetQuery(General, paste0("SELECT * FROM EMPTY"))
      user_code$lines=empty$CODE1
      user_code2$lines=empty$CODE2
      greek_lines$lines=empty$GREEK
      english_lines$lines=empty$ENGLISH
      numSlides=5
      shinyjs::show("SavePres2")
      shinyjs::hide("dash")
      
      updateSliderInput(session, "numSlides", value = numSlides)
      updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides),selected = "Slide #1")
      
      output$proposition <- renderUI("Untitled")
      updateTextInput(session,"title","Make Title", "",placeholder = "Untitled")
      
      slidenumber=1
      session$sendCustomMessage("greek_lines_stuff", greek_lines$lines[slidenumber])
      session$sendCustomMessage("english_lines_stuff", english_lines$lines[slidenumber])
      session$sendCustomMessage("user_code_stuff", user_code$lines[slidenumber])
      session$sendCustomMessage("user_code_stuff2", user_code2$lines[slidenumber])
      
    }
    else{
      myslides=dbGetQuery(General, paste0("SELECT * FROM ", input$LoadPres))
      user_code$lines=myslides$CODE1
      user_code2$lines=myslides$CODE2
      greek_lines$lines=myslides$GREEK
      english_lines$lines=myslides$ENGLISH
      numSlides=as.numeric(presentations$nslides[match(input$LoadPres,presentations$tables)])
      print(numSlides)
      Title=presentations$list[match(input$LoadPres,presentations$tables)]
      
      shinyjs::hide("SavePres2")
      shinyjs::show("dash")
      
      if(presentations$refresh2==F){
        presentations$wason2=1
      }
      
      updateSliderInput(session, "numSlides", value = numSlides)
      updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides), selected= paste("Slide #", presentations$wason2,sep=""))
      
      output$proposition <- renderUI(Title)
      if(Title!="Untitled"){
        updateTextInput(session,"title","Make Title", Title,placeholder = "Untitled")
      }
      else{
        updateTextInput(session,"title","Make Title", "",placeholder = "Untitled")
        
      }
      
      slidenumber=presentations$wason2
      session$sendCustomMessage("greek_lines_stuff", greek_lines$lines[slidenumber])
      session$sendCustomMessage("english_lines_stuff", english_lines$lines[slidenumber])
      session$sendCustomMessage("user_code_stuff", user_code$lines[slidenumber])
      session$sendCustomMessage("user_code_stuff2", user_code2$lines[slidenumber])
      
    }
    presentations$wason2=1
    poolClose(General)
    presentations$refresh2=F
  })
  
  observeEvent(input$UpdateP, {
    
    slidenumber=strtoi(strsplit(input$slides,"#")[[1]][2])
    #Need to avoid a replacement length = 0 error
    if(length(input$mydata)>0){
      greek_lines$lines[slidenumber]=input$mydata 
    }
    if(length(input$englishtrans)>0){
      english_lines$lines[slidenumber]=input$englishtrans
    }
    if(length(input$user_code)>0){
      user_code$lines[slidenumber]=input$user_code
    }  
    if(length(input$user_code)>0){
      user_code2$lines[slidenumber]=input$user_code2
    }  
    
    
    
    
    General <- dbPool(drv = RMySQL::MySQL(),
                      dbname = sqlstuff$dbname,
                      host = sqlstuff$host,
                      username = sqlstuff$username,
                      password = sqlstuff$password,
                      port = 3306)
    
    if(input$title==""){
      title='Untitled'
    }
    else{
      title=input$title
    }
    
    
    #Junction1 1:M setup
    query <- sprintf<-(paste("UPDATE `Junction1` SET TITLE = ","'",title,"' WHERE 
                             PRESENTATION_TABLE_NAME = '",  input$LoadPres,"' ", 
                             sep = ""))
    dbExecute(General, query)
    
    query <- sprintf<-(paste("UPDATE `Junction1` SET NSLIDES = ",numSlides," WHERE 
                             PRESENTATION_TABLE_NAME = '",  input$LoadPres,"' ", 
                             sep = ""))
    dbExecute(General, query)
    
    #New Table for Presentation
    pres.data <- data.frame(user_code$lines, user_code2$lines, greek_lines$lines, english_lines$lines)
    pres.data$ID <- seq.int(nrow(pres.data))
    colnames(pres.data)=c("CODE1", "CODE2", "GREEK", "ENGLISH",  "ID")
    
    dbWriteTable(General, input$LoadPres, value=pres.data, row.names = FALSE,overwrite=T)
    
    poolClose(General)
    
    presentations$wason=input$LoadPres

    presentations$refresh=T
  })
  
  
  observeEvent(input$DeleteP,{
    
    General <- dbPool(drv = RMySQL::MySQL(),
                      dbname = sqlstuff$dbname,
                      host = sqlstuff$host,
                      username = sqlstuff$username,
                      password = sqlstuff$password,
                      port = 3306)
    
    
    
    query <- sprintf<-(paste("DELETE FROM Junction1 WHERE 
                             PRESENTATION_TABLE_NAME = '",  input$LoadPres,"' ", 
                             sep = ""))
    dbExecute(General, query)
    
    
    query <- sprintf<-(paste("DROP TABLE ",  input$LoadPres," ", 
                             sep = ""))
    dbExecute(General, query)
    
    
    
    
    
    
    poolClose(General)
    presentations$wason="New"
    presentations$wason2=1
    presentations$refresh=T
  })
  
  

  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

