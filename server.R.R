
#Anusha Reddy S -11915017; Nakul Reddy Nalla - 11915025 ; Harshitha C - 11915028

# Code referenced from classroom sessions of 4 & 5 and https://shiny.rstudio.com/articles/download.html

setwd("C:/Users/chintamh/Documents/TAQ3")
getwd()

shinyServer(function(input, output) {
  
  TextFileFull <- reactive({
    
    if (is.null(input$textfile)) {   
                  return(NULL) } else{
      dataset <- readLines(input$textfile$datapath) 
      return(dataset)
    }
  })
  
  AnnotatedText100 <- reactive({
    
    
        model_file <- udpipe_load_model("./english-ewt-ud-2.4-190531.udpipe") 
        anno_text <- udpipe_annotate(model_file, x = TextFileFull())
        anno_text <- select(as.data.frame(anno_text),-sentence)
        head(anno_text,100)
      }
  )
  
  
 # Tab 2 - Annotated Documents
  output$annotated_documents = renderDataTable({ 
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
      co_occur <- cooccurrence(   	
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
      co_occur <- cooccurrence(   	
        x = subset(x, x$upos %in% check_options_values), term = "lemma", 
        group = c("doc_id", "paragraph_id", "sentence_id"))  
    }
    words_network <- head(co_occur, 30)
    words_network <- igraph::graph_from_data_frame(words_network) 
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    suppressWarnings(ggraph(words_network, layout = "fr") +  
                       
                       geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
                       geom_node_text(aes(label = name), col = "darkgreen", size = 6) +
                       
                       theme_graph(base_family = "Arial Unicode MS") +  
                       theme(legend.position = "none") +
                       
                       labs(title = "Top 30 Co-occurrence at Document Level Plot", subtitle = "Universal Speech of TAGS as chosen"))
  })
})