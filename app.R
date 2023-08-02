library(shiny)
library(tidyverse)
library(here)
library(ggrepel)

plo <- read_csv(here("data", "dictionary_app_stats.csv"))

# set plot theme
chartheme <-  theme_classic(base_size = 14)  +
  theme(strip.background = element_blank())

theme_set(chartheme)

ui = fluidPage(
  tabsetPanel(
    tabPanel("Dictionary", fluid = TRUE,
             sidebarLayout(
                sidebarPanel(
                  #uiOutput("word"),
                  selectizeInput("word", "Choose a word:", choices = NULL, options= list(maxOptions = 5000)),
                  selectizeInput("language", "Choose a language:", choices = NULL, options= list(maxOptions = 5000)),
                ),
               mainPanel(
                div(style = "height:350px;",
                  plotOutput("lang_strength_plot", height = "100%"),
                  plotOutput("lang_strength_plot2", height = "100%")
                  )
                )
             )
    )
  )
)

server <- function(input, output, session) {

  # Filter the characters based on the selected type
  data_filtered <- reactive({
     plo
  })


  observe({updateSelectizeInput(session, 'word', choices = unique(data_filtered()$word), server = TRUE) })
  observe({updateSelectizeInput(session, 'language', choices = unique(data_filtered()$langname_data), server = TRUE) })

  output$lang_strength_plot<- renderPlot({
    if (length(input$word) > 0 && input$word != '') { # don't plot anything until a word has been selected
      selected_char <- data_filtered() %>%
        filter(word== input$word) %>%
        arrange(desc(log_odds_weighted)) %>%
        head(n=20) %>%
        mutate(langname_data= fct_reorder(langname_data, desc(log_odds_weighted)))

      ggplot(selected_char , aes(x = langname_data, y = log_odds_weighted, label = gcode_data)) +
        geom_bar(stat="identity", fill = "#F8766D" , color ="#F8766D") +
        #geom_text_repel(y = -1, hjust = "right") +
        scale_x_discrete(labels = selected_char$langname_data) +
        labs(x = "Language", y = "Score",  title = paste("Top scores for", input$word)) +
        coord_flip()
   }
  })

    output$lang_strength_plot2<- renderPlot({
    if (length(input$language) > 0 && input$language!= '') { # don't plot anything until a word has been selected
      selected_char <- data_filtered() %>%
        filter(langname_data== input$language) %>%
        arrange(desc(log_odds_weighted)) %>%
        head(n=20) %>%
        mutate(word= fct_reorder(word, desc(log_odds_weighted)))

      ggplot(selected_char , aes(x = word, y = log_odds_weighted, label = word)) +
        geom_bar(stat="identity", fill = "#add8e6" , color ="#add8e6") +
        #geom_text_repel(y = -1, hjust = "right") +
        scale_x_discrete(labels = selected_char$word) +
        labs(x = "Word", y = "Score",  title = paste("Top scores for", input$language)) +
        coord_flip()
   }
  })

}

shinyApp(ui = ui, server = server)

