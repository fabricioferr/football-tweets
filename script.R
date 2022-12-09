#############################################################################
#############################################################################
################# Text mining de Tweets que mencionaram #Neymar #############
#############################################################################
#############################################################################


# Importing packages ------------------------------------------------------

library(tidyverse)
library(tidytext)
library(rtweet)
library(wordcloud)
library(wordcloud2)
library(lexiconPT)
library(showtext)

font_add_google(name = "Montserrat", family = "mont")
showtext_auto()

# Accessing Twitter API ---------------------------------------------------

## In this section were utilized steps to authenticate Twitter API
## Omitted code because I use my personal token to that

# Extract Twitter API data ------------------------------------------------

twitter_raw <- search_tweets(
  q = "#Neymar",
  type = "recent",
  lang = "pt",
  n = 32000,
  include_rts = FALSE,
  retryonratelimit = TRUE
  )

## Applying filter into data to gave only tweets from 05/12/2022 

twitter_raw <- twitter_raw %>% 
  dplyr::select(created_at, id, full_text) %>% 
  dplyr::mutate(date_formatted = format(created_at, "%Y-%m-%d")) %>% 
  dplyr::filter(date_formatted == '2022-12-05') %>% 
  dplyr::select(created_at, date_formatted, id, full_text)


# Manipulating data -------------------------------------------------------

## Unnesting data, removing spaces, symbols, emotes, numbers etc.

twitter_unnested <- twitter_raw %>% 
  mutate(
    full_text = str_to_lower(full_text) %>% str_trim(),
    comment_id = row_number()
    ) %>% 
  unnest_tweets(
    word, 
    "full_text",
    strip_punct = TRUE,
    strip_url = TRUE
    ) %>%
  filter(
    !str_detect(word, "[^\x01-\x7F]"),
    !str_detect(word, "@\\w+"),
    !str_detect(word, "https?://.+"),
    !str_detect(word, "\\d+\\w*\\d*"),
    !str_detect(word, "#\\w+"),
    !str_detect(word, "[^\x01-\x7F]"),
    !str_detect(word, "[[:punct:]]"),
    !str_detect(word, "^kk"),
    !str_detect(word, "[a-z]*2"),
    str_detect(word, "^([:alpha:]{1,})[:alpha:]{3}$")
    )

## Get stopwords 

### Custom stopwords

custom_stopwords <- tibble(
  "word" = c("pra", "x", "vai", "vem", "|", "neymar", "jr", "vamos", "vc",
             "ta","hoje", "volta", "gente", "fazer", "faz", "assim", "fala",
             "ainda", "falar", "todos", "agora", "bora", "partir", "aqui"),
  "lexicon" = "custom")

### Stopwords from lexiconPT

stopwords_pt <- get_stopwords(language = "pt") %>% 
  bind_rows(custom_stopwords)

## Anti join with stopwords

twitter_unnested <- twitter_unnested %>% 
  anti_join(stopwords_pt, by = "word")


# Diving into analysis ------------------------------------------------------

## Word clouds

twitter_counts <- twitter_unnested %>% 
  group_by(word) %>% 
  count(sort = TRUE)

### Static word cloud

wordcloud(
  words = twitter_counts$word,
  freq = twitter_counts$n,
  max.words = 50, 
  colors = brewer.pal(8, "Dark2"), 
  random.color = TRUE,
  random.order = FALSE
  )

### Dynamic word cloud

wordcloud_plot <- wordcloud2::wordcloud2(twitter_counts)

wordcloud_plot

htmlwidgets::saveWidget(
  widget = wordcloud_plot, 
  selfcontained = FALSE, file = "wordcloud_plot.html")

webshot::webshot(
  url = "wordcloud_plot.html",
  file = "wordcloud_plot.png", 
  delay = 5, 
  vwidth = 480, 
  vheight = 480
  )

## Sentiment analysis

## Getting sentiments and joining with Twitter tibble

oplexicon <- oplexicon_v3.0 %>% select(term, op_polarity = polarity)

sentilex <- sentiLex_lem_PT02 %>% select(term, sentilex_polarity = polarity)

twitter_sentiments <- twitter_unnested %>% 
  left_join(y = oplexicon, by = c("word" = "term")) %>% 
  left_join(y = sentilex, by = c("word" = "term"))
  
## Analysis

polarity_plot <- twitter_sentiments %>% 
  group_by(comment_id) %>% 
  summarise(general_sentiment = sum(op_polarity, na.rm = TRUE)) %>% 
  mutate(
    general_sentiment = case_when(
      general_sentiment == 1 ~ "Positivo",
      general_sentiment == -1 ~ "Negativo",
      TRUE ~ "Neutro")) %>% 
  group_by(general_sentiment) %>% 
  count() %>% 
  ggplot(aes(x = fct_reorder(general_sentiment, n), y = n))+
  geom_col(fill = "#836FFF")+
  theme_classic()+
  theme(
    text = element_text(family = "mont", size = 14),
    axis.title = element_text(family = "mont", colour = "black", size = 10)
    )+
  geom_text(aes(
    label = scales::number(
      x = n, suffix = " Tweets", accuracy = NULL,
      big.mark = ".", decimal.mark = ","
      )
    ),
    hjust = 0.5, vjust = -0.5, colour = "black", size = 4)+
  labs(
    x = "Sentimento",
    y = "Quantidade de Tweets",
    title = "Copa do Mundo 2022: Quais foram os sentimentos expressos no Twitter no dia 05/12?",
    subtitle = "Tweets que mencionaram a #Neymar na postagem",
    caption = "Fonte: Twitter API. Elaboração: @fabr_ferreirac"
    )

plot(polarity_plot)

ggsave(
  plot = polarity_plot,
  filename = "polarity_plot.png",
  width = 10,
  height = 5,
  dpi = 150,
  units = "in"
  )
