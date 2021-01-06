#KNOWN ISSUES:

#TO DO (big things):
#-javascript cookies?
#improve download efficiency

#NOTES:
#stuff2.html and ace2.html are now deprecated.

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
library(tools)
library(rdrop2)
library(sodium)
library(openssl)

#SOURCES:
source("EuclidToolkit.R")
source("SphericalToolkit.R")


#GLOBAL VARIABLES:
numSlides <- 5
maxSlides <- 10

#FUNCTIONS:

#2 functions below from https://github.com/karthik/rdrop2/issues/165
remove_trailing_slashes <- function(x) gsub("/*$", "", x)

download_folder <- function (path,
                             local_path,
                             dtoken = rdrop2::drop_auth(),
                             unzip = TRUE,
                             overwrite = FALSE,
                             progress = interactive(),
                             verbose = interactive()) {
  if (unzip && dir.exists(local_path))
    stop("a directory already exists at ", local_path)
  if (!unzip && file.exists(local_path))
    stop("a file already exists at ", local_path)
  
  path <- remove_trailing_slashes(path)
  local_path <- remove_trailing_slashes(local_path)
  local_parent <- dirname(local_path)
  original_dir_name <- basename(path)
  download_path <- if (unzip) tempfile("dir") else local_path
  
  if (!dir.exists(local_parent)) stop("target parent directory ", local_parent, " not found")
  
  url <- "https://content.dropboxapi.com/2/files/download_zip"
  req <- httr::POST(
    url = url,
    httr::config(token = dtoken),
    httr::add_headers(
      `Dropbox-API-Arg` = jsonlite::toJSON(list(path = paste0("/", path)),
                                           auto_unbox = TRUE)),
    if (progress) httr::progress(),
    httr::write_disk(download_path, overwrite)
  )
  httr::stop_for_status(req)
  if (verbose) {
    size <- file.size(download_path)
    class(size) <- "object_size"
    message(sprintf("Downloaded %s to %s: %s on disk", path,
                    download_path, format(size, units = "auto")))
  }
  if (unzip) {
    if (verbose) message("Unzipping file...")
    new_dir_name <- basename(local_path)
    unzip_path <- tempfile("dir")
    unzip(download_path, exdir = unzip_path)
    file.rename(file.path(unzip_path, original_dir_name),
                file.path(unzip_path, new_dir_name))
    file.copy(file.path(unzip_path, new_dir_name),
              local_parent,
              recursive = TRUE)
  }
  
  TRUE
}


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
      plot.new()
    }))  
  }
}


#Dropbox stuff
token <- readRDS("token.rds")
drop_acc(dtoken = token)
system(paste("rm -rf", "images"))
download_folder("images","images", dtoken=token, overwrite = TRUE)


# Define UI for application that draws a histogram
header <- dashboardHeader(title = "Math Presentations")

sidebar <- dashboardSidebar(  
  tags$div(
    id = "SP_Edit",
    condition = "input.Present%2==0",
    textInput("title", "Make Title", placeholder = "Untitled"),
    #actionButton("titleButton", "Make Title"),
    sliderInput("numSlides", "Number of slides", min=1, max=10, value=5, ticks=FALSE),
    actionButton("addSlidesButton", "Add Slide +"),
    #checkboxInput("SecondPlot", label = "Second Plot?", value = F),
    checkboxInput("RPlot", label = "R Plot?", value = F),
    actionButton("SaveSlide", "Save Slide"),
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
    tags$script(src="http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.js"),
    tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.css", integrity="sha384-qCEsSYDSH0x5I45nNW4oXemORUZnYFtPy/FqB/OjqxabTMW5HVaaH9USK4fN3goV", crossorigin="anonymous")
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
    
    #HTML editor for text 
    h3("Insert Text here:"),
    
    includeHTML("www/stuff.html"),
    
    conditionalPanel(
      condition = "input.RPlot == 1",
    
    h3("Insert Code for this slide's plot here:"),
    includeHTML("www/ace.html"),
    
    tags$br(),
    
    tags$ul(
      class="horul",
      tags$li(class="horli", actionButton("Preview", "Preview Plot")),
      tags$li(class="horli", id="horliright", actionButton("GoBack", "Undo")),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      )
    ),  
    
    
    # Show plot and text
    htmlOutput("message"),
    tags$br(),
    
    plotOutput("pplot"),
    
    ),
    
    tags$br(),
    
    #this will be image upload system
    
    uiOutput('file1_ui'),
    
    tags$br(),
    
    imageOutput("Image"),
    
  ),
  
  
  shinyjs::hidden(
    tags$div(
      id="MP_Pres",
      
      tags$div(
        id="text",
        htmlOutput("Text")
      ),
      
      tags$br(),
      
      conditionalPanel(
        condition = "input.RPlot == 1",
      tags$div(
        id="plot1",
        plotOutput("ppplot"),
      ),
      ),
      
      tags$br(),
      
      tags$div(
        id="image",
        fluidRow(
        column(
          6,
          imageOutput("pimage"),
        )
        )
      ),
      
      
      
      tags$br(),
      
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

    #Console should be removed/hidden on final version
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
  
  reset=F
  
  output$file1_ui <- renderUI({
    fileInput("file1", "Choose a .jpg, .jpeg or .png Image",
              multiple = FALSE,
              accept = c('image/png', 'image/jpeg', 'image/jpg'))
  })  
  
  lines=reactiveValues()
  user_code=reactiveValues()
  images=reactiveValues()
  
  #DEFAULT SLIDE SETTINGS
  
  output$pplot <- renderPlot(plot.new())
  
  
  output$proposition <- renderUI("Untitled")
  
  lines$lines=StringVecInit(maxSlides)
  user_code$lines=StringVecInit(maxSlides)
  images$data=StringVecInit(maxSlides)
  
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
    Pres_track$bool=T
    
    shinyjs::show("pimage")
    output$pimage<-renderImage(
      if(nchar(images$data[Pres_track$slide])>1){
        return( 
          list(
            src = paste("images/",images$data[Pres_track$slide],sep=""), 
            contentType=file_ext(images$data[Pres_track$slide]),
            height = 400
          )
        )
      }
      else{
        shinyjs::hide("pimage")
      }
    )
  })
  
  #Initiate Editing mode
  observeEvent(input$Edit,{
    shinyjs::show("SP_Edit")
    shinyjs::show("MP_Edit")
    shinyjs::hide("SP_Pres")
    shinyjs::hide("MP_Pres")
    Pres_track$bool=F
  })
  
  #Display Text
  output$Text <- renderUI({
    HTML(lines$lines[Pres_track$slide])
  })
  
  #Do the plots at start
  output$ppplot<-Plotter(Pres_track$slide, user_code)
    output$pimage<-renderImage(
      if(nchar(images$data[Pres_track$slide])>1){
        shinyjs::show("pimage")
        return( 
        list(
          src = paste("images/",images$data[Pres_track$slide],sep=""), 
          contentType=file_ext(images$data[Pres_track$slide]),
          height = 400
        )
      )
      }
      else{
        shinyjs::hide("pimage")
      }
    )

  
  #Move a slide 
  observeEvent(input$NextP,{
    if(Pres_track$slide < numSlides) {
      Pres_track$slide=(Pres_track$slide+1)%%(length(lines$lines)+1) + 1*((Pres_track$slide+1)%%(length(lines$lines)+1)==0)
      output$ppplot<-Plotter(Pres_track$slide, user_code)
      output$presentationSlideCounter <- renderUI(paste("Slide", Pres_track$slide, "out of", numSlides))
      
      
      output$pimage<-renderImage(
        if(nchar(images$data[Pres_track$slide])>1){
          shinyjs::show("pimage")
          return( 
            list(
              src = paste("images/",images$data[Pres_track$slide],sep=""), 
              contentType=file_ext(images$data[Pres_track$slide]),
              height = 400
            )
          )
        }
        else{
          shinyjs::hide("pimage")
        }
      )

      
      
    }
  })
  
  observeEvent(input$PreviousP,{
    if(Pres_track$slide > 1) {
      Pres_track$slide=(Pres_track$slide-1)%%(length(lines$lines)+1) + length(lines$lines)*((Pres_track$slide-1)%%(length(lines$lines)+1)==0)
      output$ppplot<-Plotter(Pres_track$slide, user_code)
      output$presentationSlideCounter <- renderUI(paste("Slide", Pres_track$slide, "out of", numSlides))
      
      output$pimage<-renderImage(
        print(images$data[Pres_track$slide]),
        if(nchar(images$data[Pres_track$slide])>1){
          shinyjs::show("pimage")
          return( 
            list(
              src = paste("images/",images$data[Pres_track$slide],sep=""), 
              contentType=file_ext(images$data[Pres_track$slide]),
              height = 400
            )
          )
        }
        else{
          shinyjs::hide("pimage")
        }
      )

      
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
    output$pplot <- renderPlot(plot.new())
    updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides))
    updateSliderInput(session, "numSlides", value = numSlides)
  })
  
  #Slider
  observeEvent(input$numSlides, {
    numSlides <<- input$numSlides
    output$pplot <- renderPlot(plot.new())
    updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides))
  })
  
  #Changing which slide number user is on
  #sends message to js code which changes js editors
  observeEvent(input$slides,{
    
    #update plots
    if(!is.na(strtoi(strsplit(input$slides,"#")[[1]][2]))){
      output$pplot<-PreviewerBack(input$slides, user_code)
    }
    
    slidenumber=strtoi(strsplit(input$slides,"#")[[1]][2])
    session$sendCustomMessage("lines_stuff", lines$lines[slidenumber])
    session$sendCustomMessage("user_code_stuff", user_code$lines[slidenumber])
    presentations$wason2=slidenumber
    
    print(images$data[slidenumber])
    print(is.null(images$data[slidenumber])==FALSE && nchar(images$data[slidenumber])>1)
    
    output$file1_ui <- renderUI({
      fileInput("file1", "Choose a .jpg, .jpeg or .png Image",
                multiple = FALSE,
                accept = c('image/png', 'image/jpeg', 'image/jpg'))
    })  
    
    reset=T
    
    shinyjs::show("Image")
    
    output$Image<-renderImage(
      if(is.null(images$data[slidenumber])==FALSE && nchar(images$data[slidenumber])>1){
        return(
          list(
            src = paste("images/",images$data[slidenumber],sep=""),
            contentType=file_ext(images$data[slidenumber]),
            height = 400
          )
        )
      }
      else{
        shinyjs::hide("Image")
      }
    )
    
    
  })
  
  #Saving a slide
  #Saves the inputs
  observeEvent(input$SaveSlide, {
    slidenumber=strtoi(strsplit(input$slides,"#")[[1]][2])
    #Need to avoid a replacement length = 0 error
    if(length(input$mydata)>0){
      lines$lines[slidenumber]=input$mydata 
    }
    if(length(input$user_code)>0){
      user_code$lines[slidenumber]=input$user_code
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
  
  #image processing
  observeEvent(input$file1,{
    if (is.null(input$file1)) return()
    if(reset){
      reset=F
      return()
    }
    else{
    #FLAW: 2 users save something at same time
    #current (bad) fix is overwrite is true
    BARCODE= paste("image", 1 , sub(" ",'',Sys.time()),sep="")
    BARCODE=str_replace_all(BARCODE, "[^[:alnum:]]", "")
    if(logged_in$logged){
      BARCODE=paste(BARCODE,logged_in$user,sep="")
    }
    slidenumber=strtoi(strsplit(input$slides,"#")[[1]][2])
    file.copy(input$file1$datapath, paste("images/",BARCODE,".",file_ext(input$file1$datapath),sep=""))
    images$data[slidenumber]=paste(BARCODE,".",file_ext(input$file1$datapath),sep="")
    
    #upload to dropbox
    if(logged_in$logged){
      drop_upload(paste("images/",BARCODE,".",file_ext(input$file1$datapath),sep=""), path = logged_in$user)
    }

      drop_upload(paste("images/",BARCODE,".",file_ext(input$file1$datapath),sep=""), path = "images")
    
    
    shinyjs::show("Image")
    
    output$Image<-renderImage(
      if(is.null(images$data[slidenumber])==FALSE && nchar(images$data[slidenumber])>1){
        return( 
          list(
            src = paste("images/",images$data[slidenumber],sep=""), 
            contentType=file_ext(images$data[slidenumber]),
            height = 400
          )
        )
      }
      else{
        shinyjs::hide("Image")
      }
    )
    }
    
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
      lines$lines[slidenumber]=input$mydata 
    }
    if(length(input$user_code)>0){
      user_code$lines[slidenumber]=input$user_code
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
    query <- sprintf<-(paste("INSERT INTO `Junction1` (`User_ID`, `PRESENTATION_TABLE_NAME`, `TITLE`, `NSLIDES`, `RPlot`) VALUES ('",
                             logged_in$user, " ','", BARCODE, "','", title, " ','", numSlides, " ','", as.integer(as.logical(input$RPlot)), "');", sep = ""))
    dbExecute(General, query)
    
    #New Table for Presentation
    pres.data <- data.frame(user_code$lines, lines$lines, images$data)
    pres.data$ID <- seq.int(nrow(pres.data))
    colnames(pres.data)=c("CODE1", "LINES", "IMAGE", "ID")
    
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
      logged_in$user=dbGetQuery(General, paste0("SELECT `ID` FROM Users WHERE Email = '", input$email ,"'"))[[1]]
      logged_in$logged=T
      shinyjs::hide("SaveSlide")
      
      presentations$list=c("New")
      presentations$tables=c("New")
      list=as.list(presentations$tables)
      presentations$wason="New"
      names(list)=presentations$list
      
      updateSelectInput(session, "LoadPres", label = "Load Presentation", choices = list,selected=presentations$wason)
      
      drop_create(toString(logged_in$user))
      
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
      shinyjs::hide("SaveSlide")
      presentations$refresh=T
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
    lines$lines=empty$TEXT
    images$data=empty$IMAGE
    output$file1_ui <- renderUI({
      fileInput("file1", "Choose a .jpg, .jpeg or .png Image",
                multiple = FALSE,
                accept = c('image/png', 'image/jpeg', 'image/jpg'))
    })  
    numSlides=5
    
    updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides),selected = "Slide #1")
    
    output$proposition <- renderUI("Untitled")
    updateTextInput(session,"title","Make Title", "",placeholder = "Untitled")
    
    slidenumber=1
    session$sendCustomMessage("lines_stuff", lines$lines[slidenumber])
    session$sendCustomMessage("user_code_stuff", user_code$lines[slidenumber])
    
    
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
    
    output$file1_ui <- renderUI({
      fileInput("file1", "Choose a .jpg, .jpeg or .png Image",
                multiple = FALSE,
                accept = c('image/png', 'image/jpeg', 'image/jpg'))
    })  
    
    reset=T
    
    
    General <- dbPool(drv = RMySQL::MySQL(),
                      dbname = sqlstuff$dbname,
                      host = sqlstuff$host,
                      username = sqlstuff$username,
                      password = sqlstuff$password,
                      port = 3306)
    
    if(input$LoadPres=="New"){
      empty=dbGetQuery(General, paste0("SELECT * FROM EMPTY"))
      user_code$lines=empty$CODE1
      lines$lines=empty$TEXT
      images$data=empty$IMAGE
      numSlides=5
      shinyjs::show("SavePres2")
      shinyjs::hide("dash")
      
      updateSliderInput(session, "numSlides", value = numSlides)
      updateSelectInput(session, "slides", label = "Slides", choices = paste0("Slide #",1:numSlides),selected = "Slide #1")
      
      output$proposition <- renderUI("Untitled")
      updateTextInput(session,"title","Make Title", "",placeholder = "Untitled")
      
      slidenumber=1
      session$sendCustomMessage("lines_stuff", lines$lines[slidenumber])
      session$sendCustomMessage("user_code_stuff", user_code$lines[slidenumber])
      
      shinyjs::show("Image")
      
      output$Image<-renderImage(
        if(is.null(images$data[slidenumber])==FALSE && nchar(images$data[slidenumber])>1){
          return(
            list(
              src = paste("images/",images$data[slidenumber],sep=""),
              contentType=file_ext(images$data[slidenumber]),
              height = 400
            )
          )
        }
        else{
          shinyjs::hide("Image")
        }
      )
      
      updateCheckboxInput(session,"RPlot",value=F)
    
    }
    else{
      myslides=dbGetQuery(General, paste0("SELECT * FROM ", input$LoadPres))
      user_code$lines=myslides$CODE1
      lines$lines=myslides$LINES
      images$data=myslides$IMAGE
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
      session$sendCustomMessage("lines_stuff", lines$lines[slidenumber])
      session$sendCustomMessage("user_code_stuff", user_code$lines[slidenumber])
      
      shinyjs::show("Image")
      
      output$Image<-renderImage(
        if(is.null(images$data[slidenumber])==FALSE && nchar(images$data[slidenumber])>1){
          return(
            list(
              src = paste("images/",images$data[slidenumber],sep=""),
              contentType=file_ext(images$data[slidenumber]),
              height = 400
            )
          )
        }
        else{
          shinyjs::hide("Image")
        }
      )
      
      statement=sprintf<-(paste("SELECT RPlot FROM `Junction1` WHERE PRESENTATION_TABLE_NAME = '",  input$LoadPres,"' ", 
                      sep = ""))
      
      
      
      
      bool=dbGetQuery(General, statement)[1,1]
      
      updateCheckboxInput(session,"RPlot",value=bool)
      shinyjs::show("pimage")
      output$pimage<-renderImage(
        if(nchar(images$data[Pres_track$slide])>1 &&  Pres_track$bool){
          return( 
            list(
              src = paste("images/",images$data[Pres_track$slide],sep=""), 
              contentType=file_ext(images$data[Pres_track$slide]),
              height = 400
            )
          )
        }
        else{
          shinyjs::hide("pimage")
        }
      )
      
    }
    
    
    
    
    presentations$wason2=1
    poolClose(General)
    presentations$refresh2=F
  })
  
  observeEvent(input$UpdateP, {

    slidenumber=strtoi(strsplit(input$slides,"#")[[1]][2])
    #Need to avoid a replacement length = 0 error
    if(length(input$mydata)>0){
      lines$lines[slidenumber]=input$mydata 
    }
    if(length(input$user_code)>0){
      user_code$lines[slidenumber]=input$user_code
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
    
    query <- sprintf<-(paste("UPDATE `Junction1` SET RPlot = ",as.integer(as.logical(input$RPlot))," WHERE 
                             PRESENTATION_TABLE_NAME = '",  input$LoadPres,"' ", 
                             sep = ""))
    dbExecute(General, query)
    
    #New Table for Presentation
    pres.data <- data.frame(user_code$lines, lines$lines, images$data)
    pres.data$ID <- seq.int(nrow(pres.data))
    colnames(pres.data)=c("CODE1", "LINES", "IMAGE", "ID")
    
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

