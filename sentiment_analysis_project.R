library(tidyverse)
library(dplyr)
library(lubridate)
library(wordcloud)
library(sentimentr)
library(tidytext)
library(textdata)
library(splitstackshape)
library(shiny)
library(DT)
library(reshape2)
rm(list=ls())

setwd('~/DATA-332/Sentiment Analysis')

df <- readRDS('Consumer_Complaints.rds')

#Data cleaning
df <- tail(df, 80000)
names(df) <- tolower(names(df))

df <- df %>%
  rename(date.sent = date.sent.to.company,
         complaint.response = company.response.to.consumer,
         timely.response = timely.response., 
         consumer.disputed = consumer.disputed.)

df$consumer.complaint.narrative <- gsub("X", "", df$consumer.complaint.narrative)
df$consumer.complaint.narrative <- gsub("/", "", df$consumer.complaint.narrative)
df$consumer.complaint.narrative <- gsub(" n't", "n't", df$consumer.complaint.narrative)

df$date.received <- as.Date(df$date.received, format = "%m/%d/%Y")
df$date.sent <- as.Date(df$date.sent, format = "%m/%d/%Y")

df$sub.product[df$sub.product == ""] <- NA


# Creating tidy dataframe to perform sentiment analysis
company_complaints_tidy <- df %>%
  select(company, consumer.complaint.narrative, state) %>%
  unnest_tokens(word, consumer.complaint.narrative)


# Finding total negative sentiment in complaints for each company
afinn <- company_complaints_tidy %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(company) %>%
  summarise(sentiment = sum(value)) %>% 
  arrange(sentiment)


# Finding average sentiment value per word to compare companies with uneven customers
sentiment_per_word <- company_complaints_tidy %>%
  group_by(company) %>%
  count(word, name = 'word_count') %>%
  group_by(company) %>%
  summarise(word_count = sum(word_count)) %>%
  left_join(afinn, by = 'company') %>%
  mutate(weighted_sentiment = sentiment/word_count) %>%
  select(company, weighted_sentiment)

sentiment_per_word$weighted_sentiment[is.na(sentiment_per_word$weighted_sentiment)] <- 0


# Finding the products with the most complaints in each company
most_common_issue <- df %>%
  select(company, sub.product, product) %>%
  mutate(sub.product = coalesce(sub.product, product))%>%
  group_by(company, sub.product) %>%
  summarise(count = n()) %>%
  summarise(product = sub.product[which.max(count)], count = max(count))


# Joining the most common product complained about into the dataframe
# with the total sentiment values attached to the companies
afinn <- most_common_issue %>%
  left_join(afinn, by = 'company') %>%
  select(company, sentiment, product) %>%
  rename(common_faulty_product = product)

# Eliminating N/A values by making them 0
afinn$sentiment[is.na(afinn$sentiment)] <- 0



ui<-fluidPage( 
  
  titlePanel(title = "Sentiment Analysis of Financial Companies"),
  
  sidebarLayout(
    
    sidebarPanel(
      fluidRow(
        column(4, selectInput('companies', 'Select # of Companies Shown:', choices = 1:30)),
        column(8,DT::dataTableOutput("table_01", width = "100%"))
      ),
      fluidRow(
        column(12, plotOutput('plot_02'))
      )
    ),
  
    mainPanel(
      fluidRow(
        column(12, plotOutput('plot_01'))
      ),
      fluidRow(
        column(12, plotOutput('plot_03'))
      ),
      fluidRow(
        column(12, plotOutput('plot_04'))
      ), 
      fluidRow(
        column(12, plotOutput('plot_05'))
      )
    )
  )
)

server<-function(input,output){
  
  # Modify the negative 'afinn' data frame based on the user input
  afinn_filtered <- reactive({
    afinn %>%
      slice_max(order_by = -(sentiment), n = as.numeric(input$companies))
  })
  
  # Plot the negative 'afinn' data based on user input
  output$plot_01 <- renderPlot({
    ggplot(afinn_filtered(), aes(reorder(company, sentiment), sentiment, fill = sentiment)) +
      geom_bar(stat = "identity") +
      theme(axis.text = element_text(size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab("Company Names") +
      ylab("Total Sentiment Score") +
      ggtitle("Total Negative Sentiment Score")
  })
  
  # Plot wordcloud based on user input
  output$plot_02 <- renderPlot({
    company_complaints_tidy %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = as.numeric(input$companies)+30))
  })
  
  # Modify negative sentiment per word data based on user input
  sentiment_per_word_filtered <- reactive({
    sentiment_per_word %>%
      slice_max(order_by = -(weighted_sentiment), n = as.numeric(input$companies))
  })
  
  # Graph negative sentiment per word data based on user input
  output$plot_03 <- renderPlot({
    ggplot(sentiment_per_word_filtered(), aes(reorder(company, weighted_sentiment), weighted_sentiment, fill = weighted_sentiment)) +
      geom_bar(stat = "identity") +
      theme(axis.text = element_text(size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab("Company Names") +
      ylab("Sentiment Score per Word") +
      ggtitle("Negative Sentiment Score per Word in Complaint")
  })
  
  # Modify the positive 'afinn' data based on user input
  afinn_filtered1 <- reactive({
    afinn %>%
      slice_max(order_by = sentiment, n = as.numeric(input$companies))
  })
  
  # Plot positive 'afinn' data based on user input
  output$plot_04 <- renderPlot({
    ggplot(afinn_filtered1(), aes(reorder(company, sentiment), sentiment, fill = sentiment)) +
      geom_bar(stat = "identity") +
      theme(axis.text = element_text(size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab("Company Names") +
      ylab("Total Sentiment Score") +
      ggtitle("Total Positive Sentiment Score")
  }) 
    
  # Modify positive sentiment per word data based on user input
  sentiment_per_word_filtered1 <- reactive({
    sentiment_per_word %>%
      slice_max(order_by = weighted_sentiment, n = as.numeric(input$companies))
  })
  
  # Plot positive sentiment per word based on user input
  output$plot_05 <- renderPlot({
    ggplot(sentiment_per_word_filtered1(), aes(reorder(company, weighted_sentiment), weighted_sentiment, fill = weighted_sentiment)) +
      geom_bar(stat = "identity") +
      theme(axis.text = element_text(size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab("Company Names") +
      ylab("Sentiment Score per Word") +
      ggtitle("Positive Sentiment Score per Word in Complaint")
  })
    
  # Modify the 'afinn' data to include the product with the most complaints for 
  # each company based on user input
  afinn_most_common_filtered <- reactive({
    afinn %>%
      slice_max(order_by = -(sentiment), n = as.numeric(input$companies))
  })

  # Plot the data table to show the products with the most complaints for each 
  # company based on user input
  output$table_01<-DT::renderDataTable({
    afinn_filtered() %>% select(company, common_faulty_product)}, 
    options = list(pageLength = 4))
}

shinyApp(ui=ui, server=server)