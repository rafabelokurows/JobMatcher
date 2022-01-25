library(dashboardthemes)
library(shiny)
library(stringr)
library(shinydashboard)
library(pdftools)
library(tidyverse)
library(tidytext)
library(widyr)
library(wordcloud2)
library(RColorBrewer)
library(DT)

ui = dashboardPage(
  dashboardHeader(title = "Job Matcher"),
  dashboardSidebar(
    h5("Here you can find if your CV is a match to your desired job position. Just copy the text from the job ad, paste it here and upload your CV to compare them!"),
    textAreaInput("caption", "Paste the the job ad here", "", width = "500px",height = "300px"),
    fileInput("file1", "Upload your CV", accept = ".pdf")
    ,sidebarMenu(
      menuItem("CV", tabName = "dashboard")
      #      menuItem("Job ad", tabName = "rawdata")
    )
  ),
  dashboardBody(shinyDashboardThemes(
    theme = "grey_dark"
  ),
    tabItems(
      
      tabItem("dashboard",
              fluidRow(
                box(width=6,
                    title="Wordcloud from the CV",
                    wordcloud2Output("cloud")
                    ),
                box(width=6,
                    title="Wordcloud from the job ad",
                    wordcloud2Output("cloud2")
                )
                
              ),
              fluidRow(
                box(
                  width=4,title="10 Most frequent words in CV",
                  dataTableOutput("contents")
                ),
                box(
                  width=4,title="10 Most frequent words in job ad",
                  dataTableOutput("contents2")
                ),
                                   # status = "info", solidHeader = TRUE,
                   # title = "Popularity by package (last 5 min)",
                  valueBoxOutput("ValueBox")
                
                 
              
                
              )
              )
    )
  )  
)


server <- function(input, output) {
  
  output$ValueBox <- renderValueBox({
    similarityWords = 0
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "pdf", "Please upload a PDF resume"))
    
    cv = tibble(text=pdf_text(file$datapath)) %>% 
      unnest_tokens(word, text, token = "ngrams", n = 1) %>% 
      anti_join(stop_words)%>% 
      filter(!str_detect(word, "^\\d")) %>% 
      count(word, sort = TRUE) %>% add_column(type = "CV")
    
    jobpost = tibble(text = input$caption) %>% 
      unnest_tokens(word, text, token = "ngrams", n = 1) %>% 
      anti_join(stop_words) %>% 
      filter(!str_detect(word, "^\\d"))%>% 
      count(word, sort = TRUE) %>% add_column(type = "Job")
    
    all = bind_rows(cv,jobpost) 
    similarityWords <- all %>%
      pairwise_similarity(type, word, n, upper = FALSE, sort = TRUE) %>% pull(similarity)
    
    
    
    shinydashboard::valueBox(
      value = round(similarityWords,2),
      subtitle = "Similarity between CV and job ad",
      icon = icon("area-chart"),
      color = "aqua"
    )
  })
  
  output$contents <- renderDataTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "pdf", "Please upload a CV in PDF format"))
    
    cv = tibble(text=pdf_text(file$datapath)) %>% 
      unnest_tokens(word, text, token = "ngrams", n = 1) %>% 
      anti_join(stop_words)%>% 
      filter(!str_detect(word, "^\\d")) %>% 
      count(word, sort = TRUE) %>% slice(1:10)
    
  })
  
  output$contents2 <- renderDataTable({
    jobpost = tibble(text = input$caption) %>% 
      unnest_tokens(word, text, token = "ngrams", n = 1) %>% 
      anti_join(stop_words) %>% 
      filter(!str_detect(word, "^\\d"))%>% 
      count(word, sort = TRUE) %>% slice(1:10)
  })
  
  
  output$cloud <- renderWordcloud2({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "pdf", "Please upload a CV in PDF format"))
    
    cv = tibble(text=pdf_text(file$datapath)) %>% 
      unnest_tokens(word, text, token = "ngrams", n = 1) %>% 
      anti_join(stop_words)%>% 
      filter(!str_detect(word, "^\\d")) %>% 
      count(word, sort = TRUE)
    cores = rev(RColorBrewer::brewer.pal(5,'Blues'))
    azuis = c()
    for(i in 1:5){
      novas = rep(cores[i],10)
      azuis = c(azuis,novas)
    }
    azuis
    wordcloud2(cv[1:50,],color=azuis,backgroundColor ="#ECECEC")
    
  }
    
  )
  output$cloud2 <- renderWordcloud2({

    jobpost = tibble(text = input$caption) %>% 
      unnest_tokens(word, text, token = "ngrams", n = 1) %>% 
      anti_join(stop_words) %>% 
      filter(!str_detect(word, "^\\d"))%>% 
      count(word, sort = TRUE)
    cores = rev(RColorBrewer::brewer.pal(5,'Reds'))
    vermelhos = c()
    for(i in 1:5){
      novas = rep(cores[i],10)
      vermelhos = c(vermelhos,novas)
    }
    vermelhos
    wordcloud2(jobpost[1:50,],color=vermelhos,backgroundColor = "#DEDEDE")
  }
  
  )
}

shinyApp(ui = ui, server = server)

