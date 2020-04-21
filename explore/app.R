library(shiny)
library(httr)
library(wordcloud2)
library(colourpicker)
library(stringr)
library(tm)

source("./cle.R")

ui <- fluidPage(
  h1("Exploration avec keywords.gg"),
  tabsetPanel(
    tabPanel(
      title = "Exploration",
      sidebarLayout(
        sidebarPanel(
            radioButtons("langue", "Choix de la langue :", c("Français" = "fr","Anglais" = "en")),
          hr(),
            textAreaInput("text", "Tapez votre requête", placeholder = "", rows = 1),
          hr(),
            textAreaInput("sup1", "Je ne veux pas de ce mot", rows = 1),
          hr(),
            textAreaInput("sup2", "Ni de celui là", rows = 1),
          hr(),
            textAreaInput("sup3", "Allez, encore un de moins", rows = 1),
          hr(),
            numericInput("maxo", "Largeur de l'exploration (de 1 à 20)\n(attention une demi-seconde en plus par unité)", value = 1, min = 1, max = 20),
          hr(),
            actionButton("actionbutton", "Exploration !"),
                    ),
        mainPanel(
          wordcloud2Output("cloud")
        )
      )
    ),
    tabPanel(
      title = "A propos",
      br(),
      "Pour utiliser cet outil d'exploration il vous faut une clé d'utilisation.",
      br(),
      "Pour cela, il faut aller sur le site suivant :",
      br(),
      HTML('<p><a href="https://www.keywords.gg/">keywords.gg</a></p>'),
      br()
    )
  )
)

getcontext <- function(input, pays) {
  #print(input)
  query <- as.character(input)
  #print(query)
  value <- list(content = query, lang = pays, limit = 10,  key = cleapi)
  #print(value)
  reponse <- POST('https://api.keywords.gg/query', content_type_json(), body  = value, encode='json')
  l <- httr::content(reponse,as="parsed")$prediction
  #print(l)
  return(paste(unlist(l), collapse=' '))
}

morecontext <- function(input, compteur, pays){
  l <- getcontext(input,pays)
  o <- l
  com <- as.numeric(compteur)
  for (i in 1:compteur){
    Sys.sleep(0.3)
    o <- paste(o,getcontext(strsplit(l," ")[[1]][i],pays),sep =" ")
  }
  return(o)
}


server <- function(input, output) {
  observeEvent(input$actionbutton, {
    print(input$langue)
    realdata <- morecontext(input$text,as.numeric(input$maxo)+1,as.character(input$langue))
    Sys.sleep(0.5)    
    create_wordcloud <- function(realdata, background = "white") {
        corpus <- Corpus(VectorSource(realdata))
        corpus <- tm_map(corpus, removeWords, c(input$sup1))
        corpus <- tm_map(corpus, removeWords, c(input$sup2))
        corpus <- tm_map(corpus, removeWords, c(input$sup3))
        tdm <- as.matrix(TermDocumentMatrix(corpus))
        data <- sort(rowSums(tdm), decreasing = TRUE)
        data <- data.frame(word = names(data), freq = as.numeric(data))
     
              if (nrow(data) == 0) {
                  return(NULL)
                                  }
    
    wordcloud2(data, backgroundColor = background)
                                                                            }
    
    output$cloud <- renderWordcloud2({
    create_wordcloud(realdata)
  })
  })
}

shinyApp(ui = ui, server = server)
