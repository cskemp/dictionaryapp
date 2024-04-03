library(shiny)
library(tidyverse)
library(here)
library(ggrepel)
library(maps)
library(sf)

nterms <- 2000

# on local machine can use bila_app_stats_6000.csv.gz
# if so change nterms above

# use uncompressed file when deploying to shinyapps.io
plo <- read_csv(here("data", "bila_app_stats_2000.csv.gz")) %>%
  arrange(langname) %>%
  arrange(word)

top_thresh <- quantile(plo$zeta, probs = c(0.95))

posns <- plo %>%
  select(glottocode, longitude, latitude) %>%
  mutate(longitude = if_else(longitude > 180, longitude - 360, longitude)) %>%
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
                    selectizeInput("word", "Choose a word:", choices = NULL, options= list(maxOptions = nterms)),
                    selectizeInput("language", "Choose a language:", choices = NULL, options= list(maxOptions = nterms)),
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

server <- function(input, output, session) {
  observe({updateSelectizeInput(session, 'word', choices = unique(plo$word), server = TRUE) })
  observe({updateSelectizeInput(session, 'language', choices = unique(plo$langname), server = TRUE) })

  output$lang_strength_plot<- renderPlot({
    if (length(input$word) > 0 && input$word != '') { # don't plot anything until a word has been selected
      selected_char <- plo %>%
        filter(word== input$word) %>%
        arrange(desc(zeta)) %>%
        #head(n=20) %>%
        head(n=10) %>%
        mutate(langname= fct_reorder(langname, desc(zeta))) %>%
        mutate(barcolor= if_else(zeta > top_thresh, "#F8766D", "grey30"))

      ggplot(selected_char , aes(x = langname, y = zeta, label = glottocode)) +
        #geom_bar(stat="identity", fill = "#F8766D" , color ="#F8766D") +
        scale_fill_identity() +
        scale_color_identity() +
        geom_bar(stat="identity", aes(fill = barcolor, color = barcolor)) +
        #geom_text_repel(y = -1, hjust = "right") +
        scale_x_discrete(labels = selected_char$langname) +
        labs(x = "Language", y = "Score",  title = paste("Top scores for", input$word)) +
        coord_flip()
   }
  })

   output$world_map<- renderPlot({
    if (length(input$word) > 0 && input$word!= '') { # don't plot anything until a word has been selected

      focal_posns <- plo %>%
        filter(word== input$word) %>%
        arrange(desc(zeta)) %>%
        #head(n=20) %>%
        filter(zeta > top_thresh) %>% # top 5
        select(-latitude, -longitude) %>%
        left_join(posns, by = c("glottocode"))

        ggplot() +
          geom_map(data = world, map = world,
                 aes(long, lat, map_id = region),
                 color = "white", fill = "lightgray", size = 0.1) +
          coord_sf(xlim = c(-180, 180), ylim=c(-55, 80)) +
          geom_point(data = posns, aes(longitude, latitude), color = "grey30", size = 1) +
          ##geom_point(data = focal_posns, aes(longitude, latitude), color = "#F8766D", size = 2) +
          geom_point(data = focal_posns, aes(longitude, latitude), color = "red", size = 2) +
          theme_void()+
          theme(legend.position = "none")
   }
  })

    output$lang_strength_plot2<- renderPlot({
    if (length(input$language) > 0 && input$language!= '') { # don't plot anything until a word has been selected
      selected_char <- plo %>%
        filter(langname== input$language) %>%
        arrange(desc(zeta)) %>%
        head(n=20) %>%
        mutate(word= fct_reorder(word, desc(zeta)))

      ggplot(selected_char , aes(x = word, y = zeta, label = word)) +
        geom_bar(stat="identity", fill = "#add8e6" , color ="#add8e6") +
        #geom_text_repel(y = -1, hjust = "right") +
        scale_x_discrete(labels = selected_char$word) +
        labs(x = "Word", y = "Score",  title = paste("Top scores for", input$language)) +
        coord_flip()
   }
  })
}

shinyApp(ui = ui, server = server)

