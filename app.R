
setwd("/Users/harshithachintam/Downloads/TAQ3")
ui <- shinyUI(
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
# server.R code
server <- shinyServer(function(input, output) {
  
  TextFileFull <- reactive({
    
    if (is.null(input$textfile)) {   
      return(NULL) } else{
        dataset <- readLines(input$textfile$datapath) #,encoding = "UTF-8")
        return(dataset)
      }
  })
  
  AnnotatedText100 <- reactive({
    
    
    model_file <- udpipe_load_model("./english-ewt-ud-2.4-190531.udpipe")  #input$udpipefile$datapath)
    anno_text <- udpipe_annotate(model_file, x = TextFileFull())
    anno_text <- select(as.data.frame(anno_text),-sentence)
    head(anno_text,100)
  }
  )
  
  
  # Tab 2 - Annotated Documents
  output$annotated_documents = renderDataTable({ #TODO - Output not displaying in tab 1
    AnnotatedText100()
    
  })
  
  AnnotatedTextFull <- reactive({
    
    
    model_file <- udpipe_load_model("./english-ewt-ud-2.4-190531.udpipe")
    anno_text <- udpipe_annotate(model_file, x = TextFileFull())
    anno_text <- as.data.frame(anno_text)
    return(anno_text)
    
  })
  
  # Download CSV button
  output$downloadData <- downloadHandler(
    filename = function() {'anno.csv'},
    
    content = function(file) {
      write.csv (AnnotatedTextFull(), file)
    }
  )
  # Tab 3- NOUNS wordcloud
  output$nounsplot = renderPlot({
    input_text_value <-  as.character(TextFileFull())
    model = udpipe_load_model("./english-ewt-ud-2.4-190531.udpipe")
    x <- udpipe_annotate(model, x = input_text_value, doc_id = seq_along(input_text_value))
    x <- as.data.frame(x)
    
    all_nouns = x %>% subset(., upos %in% "NOUN") 
    top_nouns = txt_freq(all_nouns$lemma)
    
    wordcloud(words = top_nouns$key, 
              freq = top_nouns$freq, 
              min.freq = 2, 
              max.words = 100,
              random.order = FALSE, 
              colors = brewer.pal(6, "Dark2"))
    
  })
  # Tab 3- VERBS wordcloud
  output$verbsplot = renderPlot({
    input_text_value <-  as.character(TextFileFull())
    model = udpipe_load_model("./english-ewt-ud-2.4-190531.udpipe")
    x <- udpipe_annotate(model, x = input_text_value, doc_id = seq_along(input_text_value))
    x <- as.data.frame(x)
    
    all_verbs = x %>% subset(., upos %in% "VERB") 
    top_verbs = txt_freq(all_verbs$lemma)
    
    wordcloud(words = top_verbs$key, 
              freq = top_verbs$freq, 
              min.freq = 2, 
              max.words = 100,
              random.order = FALSE, 
              colors = brewer.pal(6, "Dark2"))
  })
  
  # Tab 4 - Top -30 Co-Occurances at document level
  output$cooccurplot = renderPlot({
    input_text_value <-  as.character(TextFileFull())
    model = udpipe_load_model("./english-ewt-ud-2.4-190531.udpipe")
    x <- udpipe_annotate(model, x = input_text_value, doc_id = seq_along(input_text_value))
    x <- as.data.frame(x)
    
    if (input$Language == "English"){
      co_occur <- cooccurrence(   	# try `?cooccurrence` for parm options
        x = subset(x, x$xpos %in% input$upos_checkGroup), term = "lemma", 
        group = c("doc_id", "paragraph_id", "sentence_id"))  
    }
    else{
      check_options_values <- input$upos_checkGroup
      for(i in seq_len(length(input$upos_checkGroup))){
        if (input$upos_checkGroup[i] == "AE"){
          check_options_values[i] <- "ADJ"
        }
        else if (input$upos_checkGroup[i] == "NN"){
          check_options_values[i] <- "NOUN"
        }
        else if (input$upos_checkGroup[i] == "PRN"){
          check_options_values[i] <- "PROPN"
        }
        else if (input$upos_checkGroup[i] == "AB"){
          check_options_values[i] <- "ADV"
        }
        else{
          check_options_values[i] <- "VE"
        }
      }
      co_occur <- cooccurrence(   	# try `?cooccurrence` for parm options
        x = subset(x, x$upos %in% check_options_values), term = "lemma", 
        group = c("doc_id", "paragraph_id", "sentence_id"))  # 0.02 secs
    }
    words_network <- head(co_occur, 30)
    words_network <- igraph::graph_from_data_frame(words_network) # needs edgelist in first 2 colms.
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    suppressWarnings(ggraph(words_network, layout = "fr") +  
                       
                       geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
                       geom_node_text(aes(label = name), col = "darkgreen", size = 6) +
                       
                       theme_graph(base_family = "Arial Unicode MS") +  
                       theme(legend.position = "none") +
                       
                       labs(title = "Top 30 Co-occurrence at Document Level Plot", subtitle = "Universal Speech of TAGS as chosen"))
  })
})
shinyApp(ui = ui, server = server)