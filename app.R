library(shiny)
library(dplyr)
library(forcats)
library(ggplot2)
library(here)
library(maps)
library(sf)

global_data <- readRDS(here("data", "preprocessed_app_data.rds"))

chartheme <-  theme_classic(base_size = 12)  +
  theme(strip.background = element_blank())

theme_set(chartheme)

ui = fluidPage(
  tags$head(
    tags$style(HTML("
      .col-sm-6 {
        float: left !important;
        width: 50% !important;
      }
    "))
  ),
  fluidRow(
    column(6, selectizeInput("word", "Choose a concept:",
                             choices = global_data$words,
                             options = list(
                               placeholder = 'Select a concept',
                               maxItems = 1,
                               maxOptions = 5999
                             ))),
    column(6, selectInput("language", "Choose a language:", choices = global_data$languages))
  ),
  fluidRow(
    column(6,
           div(style = "height:300px;",
               plotOutput("lang_strength_plot", height = "100%")
           )
    ),
    column(6,
           div(style = "height:300px;",
               plotOutput("lang_strength_plot2", height = "100%")
           )
    )
  ),

  # Third row: Maps
  fluidRow(
    column(6,
           div(style = "height:250px;",
               plotOutput("world_map", height = "100%")
           )
    ),
    column(6,
           div(style = "height:250px;",
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

  # Reactive data preparation functions
  get_word_data <- reactive({
    req(input$word)
    global_data$plo %>%
      filter(word == input$word) %>%
      arrange(desc(zeta)) %>%
      head(n = 20) %>%
      mutate(langname = fct_reorder(langname, desc(zeta))) %>%
      mutate(barcolor = if_else(top_flag, "#CC79A7", "grey30"))
  })

  get_word_map_data <- reactive({
    req(input$word)
    global_data$plo %>%
      filter(word == input$word) %>%
      filter(top_flag) %>%
      select(-latitude, -longitude) %>%
      left_join(global_data$posns, by = c("glottocode"))
  })

  get_language_data <- reactive({
    req(input$language)
    global_data$plo %>%
      filter(langname == input$language) %>%
      arrange(desc(zeta)) %>%
      head(n = 20) %>%
      mutate(word = fct_reorder(word, desc(zeta)))
  })

  get_language_map_data <- reactive({
    req(input$language)
    global_data$plo %>%
      filter(langname == input$language) %>%
      select(glottocode) %>%
      left_join(global_data$posns, by = c("glottocode"))
  })

  get_nn_data <- reactive({
    req(input$word)
    global_data$nns %>%
        filter(word == input$word)
  })

  get_nl_data <- reactive({
    req(input$language)
    global_data$nls %>%
        filter(langs== input$language)
  })


  output$lang_strength_plot <- renderPlot({
    selected_char <- get_word_data()

    ggplot(selected_char, aes(x = langname, y = zeta, label = glottocode)) +
      geom_bar(stat = "identity", aes(fill = barcolor, color = barcolor)) +
      scale_fill_identity() +
      scale_color_identity() +
      scale_x_discrete(labels = selected_char$langname) +
      theme(plot.title = element_text(hjust = 0.4, size = 14)) +
      labs(x = "Language", y = "Score", title = paste("Top scores for", input$word)) +
      coord_flip()
  })

  output$world_map <- renderPlot({
    focal_posns <- get_word_map_data()

    ggplot() +
      geom_map(data = global_data$world, map = global_data$world,
               aes(long, lat, map_id = region),
               color = "white", fill = "gray95", size = 0.1) +
      coord_sf(xlim = c(-180, 180), ylim = c(-55, 80)) +
      geom_point(data = global_data$posns, aes(longitude, latitude), color = "grey60", size = 0.8) +
      geom_point(data = focal_posns, aes(longitude, latitude), color = "#A52A6C", size = 1.8) +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      labs(title = paste("Top languages for", input$word))
  })

  output$lang_strength_plot2 <- renderPlot({
    selected_char <- get_language_data()

    ggplot(selected_char, aes(x = word, y = zeta, label = word)) +
      geom_bar(stat = "identity", fill = "#add8e6", color = "#add8e6") +
      scale_x_discrete(labels = selected_char$word) +
      theme(plot.title = element_text(hjust = 0.4, size = 14)) +
      labs(x = "Concept", y = "Score", title = paste("Top scores for", input$language)) +
      coord_flip()
  })

  output$language_map <- renderPlot({
    lang_posns <- get_language_map_data()

    ggplot() +
      geom_map(data = global_data$world, map = global_data$world,
               aes(long, lat, map_id = region),
               color = "white", fill = "gray95", size = 0.1) +
      coord_sf(xlim = c(-180, 180), ylim = c(-55, 80)) +
      geom_point(data = global_data$posns, aes(longitude, latitude), color = "grey60", size = 0.8) +
      geom_point(data = lang_posns, aes(longitude, latitude), color = "#2E6DB6", size = 2) +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      labs(title = paste("Location of", input$language))
  })

  output$nn_plot <- renderPlot({
      this_nns <- get_nn_data()

      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = this_nns$top_cases, size = 4, hjust = 0.5) +  # Centered text
        theme_void(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) +
        labs(title = paste("Concepts related to", input$word))
  })

  output$nl_plot <- renderPlot({
      this_nls <- get_nl_data()

      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = this_nls$top_cases, size = 4, hjust = 0.5) +  # Centered text
        theme_void(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, size = 14)) +
        labs(title = paste("Languages related to", input$language))
  })
}

shinyApp(ui = ui, server = server)
