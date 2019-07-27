

#Anusha Reddy S -11915017; Nakul Reddy Nalla - 11915025 ; Harshitha C - 11915028

# Code referenced from classroom sessions of 4 & 5 and https://shiny.rstudio.com/articles/download.html

shinyUI(
  fluidPage(
    
    titlePanel("Shiny App - UDPipe NLP Workflow"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("textfile", "Upload Text File"),

        checkboxGroupInput("upos_checkGroup", label = h3("Select Speech Tags"), 
                           choices = list("adjective (ADJ)" = "AE", "noun(NOUN)" = "NN", "proper noun (PROPN)" = "PRN", "adverb (ADV)" = "AB", "verb (VERB)" = "VE"),
                           selected = c("AE","NN","PRN")),

        
        selectInput("Language", label = h3("Select Language"), 
                    choices = list("English-EWT" = "EnglishEWT","English-GUM" = "EnglishGUM","English-LinES" = "EnglishLinES","English-ParTUT" = "EnglishParTUT"), 
                    selected = "EnglishEWT"),
        hr(),
        
        fluidRow(column(3, verbatimTextOutput("value"))),
        
        submitButton(text = "Apply Changes", icon("refresh"))),
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",h4(p("How to use this App")),
                             
                             p("Upload a document in txt file format and consider UDPIPE English Language Model file.\n\n 
                       To upload a file, go to browse in left-sidebar panel and upload from your PC. \n\n
                       Once the upload is complete, shinyapp will compute annotated documents, Cooccurances, word clouds in the backdrop and will display the results in 3 different tabs.", align = "justify")),
                    
                    tabPanel("Annotated Documents", 
                             h4(p("Annotated Documents Table xwith 100 rows of data")),
                             dataTableOutput('annotated_documents'),
                             h4(p("Download all rows of Annotated Documents")),
                             downloadButton('downloadData', 'Download CSV')),
                    
                    tabPanel("Word Clouds",
                             h4(p("Word Clouds of NOUN & VERB")),
                             plotOutput('nounsplot'),
                             plotOutput('verbsplot')),
                    
                    tabPanel("Co-occurrences Plot",
                             h4(p("Top-30 co-occurrences at document level")),
                             plotOutput('cooccurplot'))
                    
                             ) 
                    )
  ) 
)  
) 