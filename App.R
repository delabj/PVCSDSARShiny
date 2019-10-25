source("libraries.R")

## UI for the App that makes charts for the annual report
ui <- navbarPage("Annual Report Charts:",
                 tabPanel("Home",
                          h1("About this App"),
                          fluidRow(
                              column(width = 3, img(src='spartanLogo.png', align = "center")),
                              column(width = 9, 
                                     p("This app is deisgned to help create the charts used in the annual report. \n
                   Simply enter the requested information on each page and then download the provided chart"), 
                                     p(HTML("Images created with this can be saved by right clicking and saving as 
                                       <em>plot_name</em>.png")),
                                     p(tags$b("It is reccomended that this App be run in a full sized window"))
                              )
                          )
                          ),
                 tabPanel("Class Size", 
                          mainPanel(
                              
                              rHandsontableOutput("classSize"), 
                              plotOutput("classSizePlot")
                              
                          )
                          ),
                 tabPanel("AP Scores", 
                          rHandsontableOutput("apScores"), 
                          p("The total in the percentage should equal 100 exactly"),
                          plotOutput("apPlot")),
                 navbarMenu("MAP", 
                            tabPanel("MAP: Data Upload", 
                                     titlePanel("Uploading Files"),
                                     
                                     # Sidebar layout with input and output definitions ----
                                     sidebarLayout(
                                         
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(
                                             # Input: Select a file ----
                                             fileInput("file1", "Choose CSV File",
                                                       multiple = TRUE,
                                                       accept = c("text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")),
                                             
                                             h6("Use this", a(href="https://drive.google.com/file/d/1Y_EuwyRwoce4V292IqsID6ARNwoB4k3r/view?usp=sharing", "template"),"to upload the MAP data as a .CSV file"),
                                             h6("Variable discriptions", a(href="www.google.com", "here")),
                                             h6("Options should remain in the default position unless you know what you're doing"),
                                             # Horizontal line ----
                                             tags$hr(),
                                             
                                             # Input: Checkbox if file has header ----
                                             checkboxInput("header", "Header", TRUE),
                                             
                                             # Horizontal line ----
                                             tags$hr(),
                                             
                                             # Input: Select number of rows to display ----
                                             radioButtons("disp", "Display",
                                                          choices = c(Head = "head",
                                                                      All = "all"),
                                                          selected = "head")
                                             
                                         ),
                                         
                                         # Main panel for displaying outputs ----
                                         mainPanel(
                                             
                                             # Output: Data file ----
                                             tableOutput("contents")
                                             
                                         )
                                         
                                     )),
                            
                            # MAP Grade By Grade
                            tabPanel("MAP Grade By Grade",
                                     sidebarLayout(
                                         sidebarPanel(
                                             radioButtons("gradeSelection", "Grade Selection", 
                                                          choices = c('3rd Grade' = "3rd",
                                                                      '4th Grade' = "4th",
                                                                      '5th Grade' = "5th", 
                                                                      '6th Grade' = "6th", 
                                                                      '7th Grade' = "7th",
                                                                      '8th Grade' = "8th",
                                                                      '9th Grade' = "9th",
                                                                      '10th Grade' = "10th"
                                                                      ), 
                                                          selected = "3rd"), 
                                             radioButtons("subjectSelection", "Subject", 
                                                          choices = c(Reading= "Reading", 
                                                                      Math = "Math"),
                                                          selected ="Reading"), width = 3
                                         ),
                                         mainPanel(plotOutput("engMAPPlot"))
                                     )
                                     ),
                            
                            #MAP Year at a glance
                            tabPanel("MAP: Year At A Glance",
                                     sidebarLayout(
                                         sidebarPanel(
                                             radioButtons("subectSum", "Subject", 
                                                        choices = c(Reading = "Reading", Math = "Math"),
                                             selected = "Reading", width = 3
                                         ),uiOutput("yearSelect"),
                                         ), 
                                         mainPanel(
                                             plotOutput(("mapOverview"))
                                         )
                                     )))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #### Custom Theme ####
    
    theme_pleasval <-  function(){
        theme_minimal(base_size = 16, base_family = "") %+replace%
            theme(
                axis.text = element_text(size=rel(0.8),color = "#565656"),
                axis.ticks = element_line(color = "#D6D6D6"),
                legend.key = element_rect(colour = "White"),
                panel.grid.major = element_line(colour = "#C6C6C6", size = 0.2, lineend = "butt"),
                panel.grid.minor = element_line(colour = "#E6E6E6", size = 0.05, linetype = "longdash")
            )
    }

    #### Data ####
        
    ### Hands on Table ###
    
    ## Class Size
    output$classSize <- renderRHandsontable({
        if (is.null(input$classSize)) {
            Year <- c("08-09","09-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17", "17-18",
                      "18-19")
            Size <- c(22, 22, 23, 24, 24, 24,23, 22,23, 24,22)
            DFCS = data.frame(Year,Size,
                              stringsAsFactors = F)
        } else {
            DFCS = hot_to_r(input$classSize)
        }
        
        rhandsontable(DFCS) %>%
           hot_col(col = 1, readOnly = FALSE) 
    }) 
    
    ## AP Scores
        output$apScores <- renderRHandsontable({
            if (is.null(input$apScores)) {
                Score=c("5","4","3","2","1")
                Percent=c(19,35,30,14,1)
                DFAP = data.frame(Score,Percent,stringsAsFactors = F)
            } 
            else {
                DFAP = hot_to_r(input$apScores)
            }
            
            rhandsontable(DFAP) %>%
              hot_col(col = 1, readOnly = FALSE) 
        })         
        
    ## Extract the CSV
    filedata <- reactive({
        infile <- input$file1
        if (is.null(infile)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        read.csv(infile$datapath,
                 header = input$header,
                 sep = ",",
                 quote = '"')
    })
        
    ## Show Uploaded Data
        output$contents <- renderTable({
            df <- filedata()
            
            if(input$disp == "head") {
                return(head(df))
            }
            else {
                return(df)
            }
        })
    
    #### UI ####
    
    output$yearSelect <- renderUI({
        df <-filedata()
        
        if (is.null(df)) return(NULL)
        
        items=unique(df$schoolYear)
        selectInput("yearDropdown", "Year for Overview",items)
    })
    
    ##### plots ####
    
    ## Class Size ##
    output$classSizePlot<-renderPlot({
        hot_to_r(input$classSize) %>%
            ggplot(aes(x=Year,y=Size, group= Year))+
            geom_bar(stat='identity',, fill="#003569")+
            labs(x="", y="Class Size",title =" Elementary Class Size")+
            geom_text(aes(label=Size), col ="#D6D6D6", vjust=1.6, position = position_dodge(width=1))+
            theme_pleasval()+ 
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+ 
            ylim(0,30)+ 
            coord_fixed(ratio=.1)+ 
            theme(plot.title = element_text(hjust = 0.5))
    },height = 400,width = 800)
    
    ## AP SCORES ## 
    output$apPlot<-renderPlot({
        hot_to_r(input$apScores) %>%
            ggplot(aes(y= Percent,x= Score))+
            geom_col(fill=c("#000000","#565656","#D6D6D6","#26547C","#003569"), col="#000000")+ #manually adjusting colors
            labs(title = "2018-2019 AP Scores", y="")+
            geom_text(aes(label=paste0(Percent,"%")), col ="#000000",
                      vjust=0,hjust=-.25, position = position_dodge(width=1))+
            theme_pleasval()+ 
            coord_flip()+ 
            theme(aspect.ratio = .25)+ 
            theme(plot.title = element_text(hjust = 0.5))+ 
            theme(legend.position="none", axis.text.x=element_blank())
    },height = 400,width = 800)
    
    ## MAP BY GRADE ##
    output$engMAPPlot <-renderPlot({
        
        
        if(is.null(filedata())) return(NULL)
        
        filedata() %>%
            filter(schoolYear != '2013-2014')%>%
            filter(subject==input$subjectSelection)%>%
            filter(grade==input$gradeSelection)%>%
            ggplot(aes(x=reorder(schoolYear, desc(schoolYear))))+
            geom_bar(stat="Identity",aes(y=springMeanRit), fill = "#4B6C8C", width=.8)+
            geom_bar(stat="Identity",aes(y=fallMeanRIT), fill = "#003569", width=.8)+
            geom_hline(aes(yintercept=`meanEOY`), size=1.5)+
            coord_flip(ylim=c(0,300))+
            labs(x="", y="", title = paste0(input$gradeSelection, paste0(" Grade Mean RIT: ",input$subjectSelection)))+
            theme_pleasval()+
            theme(aspect.ratio = .2)+
            theme(plot.title = element_text(hjust = 0.5))
    },height = 400,width = 800)
    
    ## MAP YEARLY OVERVIEW ##
    output$mapOverview <-renderPlot({
        
        if(is.null(filedata())) return(NULL)
        
        filedata() %>%
            filter(schoolYear==input$yearDropdown)%>%
            filter(subject==input$subectSum)%>%
            ggplot(aes(x=reorder(grade,meanEOY)))+
            geom_bar(stat="Identity",aes(y=springMeanRit), fill = "#4B6C8C", width=.8)+
            geom_bar(stat="Identity",aes(y=fallMeanRIT), fill = "#003569", width=.8)+
            geom_bar( stat="Identity",aes(y=meanEOY),fill = "#000000", width=.8)+
            geom_bar( stat="Identity",aes(y=meanEOY-1),fill = "#003569", width=.8)+
            coord_flip(ylim=c(0,300))+
            labs(x="", y="MAP RIT Score", title = paste0(input$yearDropdown, paste0(" MAP ", paste0(input$subectSum," Scores By Grade"))))+
            theme_pleasval()+
            theme(aspect.ratio = .2)+
            theme(legend.position="Bottom")
    },height = 400,width = 800)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
