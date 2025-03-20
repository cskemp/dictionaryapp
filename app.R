library(shiny)
library(tidyverse)
library(here)
library(ggrepel)
library(maps)
library(sf)
library(stringr)

nterms <- 2000

# on local machine can use bila_app_stats_6000.csv.gz
# if so change nterms above

# use uncompressed file when deploying to shinyapps.io
plo <- read_csv(here("data", "bila_app_stats_2000.csv")) %>%
  arrange(langname) %>%
  arrange(word)

nns <- read_csv(here("data", "bila_app_nn_2000.csv")) %>%
  mutate(i = -i)

nls <- read_csv(here("data", "bila_app_nl.csv"))

top_thresh <- quantile(plo$zeta, probs = c(0.95))

posns <- plo %>%
  select(glottocode, longitude, latitude) %>%
  mutate(longitude = if_else(longitude > 180, longitude - 360, longitude)) %>%
  unique()

world <- map_data("world")

chartheme <-  theme_classic(base_size = 12)  +
  theme(strip.background = element_blank())

theme_set(chartheme)

ui = fluidPage(

  fluidRow(
    column(6, selectizeInput("word", "Choose a word:", choices = NULL, options= list(maxOptions = nterms))),
    column(6, selectizeInput("language", "Choose a language:", choices = NULL, options= list(maxOptions = nterms)))
  ),

  fluidRow(
    column(6,
           div(style = "height:350px;",
               plotOutput("lang_strength_plot", height = "100%")
           )
    ),
    column(6,
           div(style = "height:350px;",
               plotOutput("lang_strength_plot2", height = "100%")
           )
    )
  ),

  # Third row: Maps
  fluidRow(
    column(6,
           div(style = "height:300px;",
               plotOutput("world_map", height = "100%")
           )
    ),
    column(6,
           div(style = "height:300px;",
               plotOutput("language_map", height = "100%")
           )
    )
  ),

  fluidRow(
    column(6,
           div(style = "height:100px;",
               plotOutput("nn_plot", height = "100%")
           )
    ),
    column(6,
           div(style = "height:100px;",
               plotOutput("nl_plot", height = "100%")
           )
    )
  )
)

server <- function(input, output, session) {
  observe({updateSelectizeInput(session, 'word', choices = unique(plo$word), server = TRUE) })
  observe({updateSelectizeInput(session, 'language', choices = unique(plo$langname), server = TRUE) })

  # output$nn_plot<- renderPlot({
  #   if (length(input$word) > 0 && input$word != '') { # don't plot anything until a word has been selected
  #     this_nns <- nns %>%
  #       filter(word == input$word)
  #
  #     ggplot(this_nns, aes(x = 1, y = i, label = neighbour)) +
  #       geom_text(hjust = 0, x = -0.00) +
  #       xlim(-0.75,1.25) +
  #       theme_void(base_size = 14) +
  #       theme(plot.title = element_text(hjust = 0.1, size = 15))  +
  #       labs(title = paste("Words related to", input$word))
  #
  #   }
  # })

  output$nn_plot <- renderPlot({
    if (length(input$word) > 0 && input$word != '') {  # Don't plot until a word is selected
      this_nns <- nns %>%
        filter(word == input$word)

      words_text <- paste(this_nns$neighbour, collapse = ", ")
      wrapped_words <- str_wrap(words_text, width = 60)

      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = wrapped_words, size = 5, hjust = 0.5) +  # Centered text
        theme_void(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        labs(title = paste("Words related to", input$word))
    }
  })

  output$lang_strength_plot<- renderPlot({
    if (length(input$word) > 0 && input$word != '') {
      selected_char <- plo %>%
        filter(word == input$word) %>%
        arrange(desc(zeta)) %>%
        mutate(cutoff = quantile(zeta, 0.95)) %>%
        head(n=20) %>%
        mutate(langname = fct_reorder(langname, desc(zeta))) %>%
        mutate(barcolor = if_else(zeta > cutoff, "#CC79A7", "grey30"))

      ggplot(selected_char, aes(x = langname, y = zeta, label = glottocode)) +
        geom_bar(stat="identity", aes(fill = barcolor, color = barcolor)) +
        scale_fill_identity() +
        scale_color_identity() +
        scale_x_discrete(labels = selected_char$langname) +
        labs(x = "Language", y = "Score", title = paste("Top scores for", input$word)) +
        theme(plot.title = element_text(hjust = 0.4, size = 16))  +
        coord_flip()
    }
  })

  output$world_map<- renderPlot({
    if (length(input$word) > 0 && input$word!= '') {
      focal_posns <- plo %>%
        filter(word == input$word) %>%
        arrange(desc(zeta)) %>%
        mutate(cutoff = quantile(zeta, 0.95)) %>%
        filter(zeta > cutoff) %>%
        select(-latitude, -longitude) %>%
        left_join(posns, by = c("glottocode"))

      ggplot() +
        geom_map(data = world, map = world, aes(long, lat, map_id = region),
                 color = "white", fill = "gray95", size = 0.1) +
        coord_sf(xlim = c(-180, 180), ylim=c(-55, 80)) +
        geom_point(data = posns, aes(longitude, latitude), color = "grey60", size = 0.8) +
        geom_point(data = focal_posns, aes(longitude, latitude), color = "#A52A6C", size = 1.8) +
        theme_void() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 16))  +
        labs(title = paste("Locations of top-scoring languages for", input$word))
    }
  })

  output$lang_strength_plot2<- renderPlot({
    if (length(input$language) > 0 && input$language!= '') {
      selected_char <- plo %>%
        filter(langname == input$language) %>%
        arrange(desc(zeta)) %>%
        head(n=20) %>%
        mutate(word = fct_reorder(word, desc(zeta)))

      ggplot(selected_char, aes(x = word, y = zeta, label = word)) +
        geom_bar(stat="identity", fill = "#add8e6", color ="#add8e6") +
        scale_x_discrete(labels = selected_char$word) +
        labs(x = "Word", y = "Score", title = paste("Top scores for", input$language)) +
        theme(plot.title = element_text(hjust = 0.4, size = 15))  +
        coord_flip()
    }
  })

  output$language_map <- renderPlot({
    if (length(input$language) > 0 && input$language != '') {
      lang_posns <- plo %>%
        filter(langname == input$language) %>%
        select(glottocode) %>%
        left_join(posns, by = c("glottocode"))

      ggplot() +
        geom_map(data = world, map = world, aes(long, lat, map_id = region),
                 color = "white", fill = "gray95", size = 0.1) +
        coord_sf(xlim = c(-180, 180), ylim = c(-55, 80)) +
        geom_point(data = posns, aes(longitude, latitude), color = "grey60", size = 0.8) +
        geom_point(data = lang_posns, aes(longitude, latitude), color = "#2E6DB6", size = 2) +
        theme_void() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 16)) +
        labs(title = paste("Location of", input$language))
    }
  })

  output$nl_plot <- renderPlot({
    if (length(input$language) > 0 && input$language != '') {  # Don't plot until a language is selected
      this_nls <- nls %>%
        filter(lang_a == input$language)

      words_text <- paste(this_nls$lang_b, collapse = ", ")
      wrapped_words <- str_wrap(words_text, width = 60)

      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = wrapped_words, size = 5, hjust = 0.5) +  # Centered text
        theme_void(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        labs(title = paste("Languages related to", input$language))
    }
  })
}

shinyApp(ui = ui, server = server)
