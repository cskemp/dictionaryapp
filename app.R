library(shiny)
library(tidyverse)
library(here)
library(ggrepel)
library(maps)
library(sf)

# Top 5%: above 8.7
# Top 1%: above 16.2
# Top 0.1%: above 37.6

plo <- read_csv(here("data", "dictionary_app_stats.csv")) %>%
  arrange(langname_data) %>%
  arrange(word)

posns <- plo %>%
  select(gcode_data, longitude_data, latitude_data) %>%
  mutate(longitude_data = if_else(longitude_data > 180, longitude_data - 360, longitude_data)) %>%
  unique()

world <- map_data("world")

# set plot theme
chartheme <-  theme_classic(base_size = 14)  +
  theme(strip.background = element_blank())

theme_set(chartheme)

ui = fluidPage(
  tabsetPanel(
    tabPanel("Dictionary", fluid = TRUE,
             fluidRow(
                column(4,
                    selectizeInput("word", "Choose a word:", choices = NULL, options= list(maxOptions = 5000)),
                    selectizeInput("language", "Choose a language:", choices = NULL, options= list(maxOptions = 5000)),
                ),
                column(8,
                  div(style = "height:350px;",
                    plotOutput("lang_strength_plot", height = "100%"),
                  )
                )
             ),
             fluidRow(
               column(4,
                  div(style = "height:350px;",
                    plotOutput("lang_strength_plot2", height = "100%"),
                  )
               ),
               column(8,
                  div(style = "height:350px;",
                    plotOutput("world_map", height = "100%"),
                  )
               )
             )
    )
  )
)


# ui = fluidPage(
#   tabsetPanel(
#     tabPanel("Dictionary", fluid = TRUE,
#              sidebarLayout(
#                 sidebarPanel(
#                   #uiOutput("word"),
#                   selectizeInput("word", "Choose a word:", choices = NULL, options= list(maxOptions = 5000)),
#                   selectizeInput("language", "Choose a language:", choices = NULL, options= list(maxOptions = 5000)),
#                 ),
#                mainPanel(
#                 div(style = "height:350px;",
#                   plotOutput("lang_strength_plot", height = "100%"),
#                   plotOutput("lang_strength_plot2", height = "100%")
#                   )
#                 )
#              )
#     )
#   )
# )

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
        #head(n=20) %>%
        head(n=10) %>%
        mutate(langname_data= fct_reorder(langname_data, desc(log_odds_weighted))) %>%
        mutate(barcolor= if_else(log_odds_weighted > 8.7, "#F8766D", "grey30"))

      ggplot(selected_char , aes(x = langname_data, y = log_odds_weighted, label = gcode_data)) +
        #geom_bar(stat="identity", fill = "#F8766D" , color ="#F8766D") +
        scale_fill_identity() +
        scale_color_identity() +
        geom_bar(stat="identity", aes(fill = barcolor, color = barcolor)) +
        #geom_text_repel(y = -1, hjust = "right") +
        scale_x_discrete(labels = selected_char$langname_data) +
        labs(x = "Language", y = "Score",  title = paste("Top scores for", input$word)) +
        coord_flip()
   }
  })

   output$world_map<- renderPlot({
    if (length(input$word) > 0 && input$word!= '') { # don't plot anything until a word has been selected

      focal_posns <- data_filtered() %>%
        filter(word== input$word) %>%
        arrange(desc(log_odds_weighted)) %>%
        #head(n=20) %>%
        filter(log_odds_weighted >8.7) %>% # top 5
        select(-latitude_data, -longitude_data) %>%
        left_join(posns, by = c("gcode_data"))

        ggplot() +
          geom_map(data = world, map = world,
                 aes(long, lat, map_id = region),
                 color = "white", fill = "lightgray", size = 0.1) +
          coord_sf(xlim = c(-180, 180), ylim=c(-55, 80)) +
          geom_point(data = posns, aes(longitude_data, latitude_data), color = "grey30", size = 1) +
          #geom_point(data = focal_posns, aes(longitude_data, latitude_data), color = "#F8766D", size = 2) +
          geom_point(data = focal_posns, aes(longitude_data, latitude_data), color = "red", size = 2) +
          theme_void()+
          theme(legend.position = "none")
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

