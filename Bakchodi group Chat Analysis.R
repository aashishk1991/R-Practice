history <- "C:/Users/kharb/Desktop/R/WhatsApp Chat with Bakchodi.txt"
history
library("rwhatsapp")
library("dplyr")
chat <- rwa_read(history) %>%
filter(!is.na(author)) # remove messages without author

# number of message per day#
  
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")
chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")

# number of message by group member
chat %>%
  mutate(day = year(time)) %>%
  filter(year(time)>= "2019") %>%
  count(author) %>%
  ggplot(aes(x = reorder(author,n), y = n)) +
  geom_bar(stat = "identity",fill="steelblue") +
  ylab("") + xlab("") +
  coord_flip() +
  geom_text(aes(label=n),vjust = 0.5,hjust =1,size = 3.5 )+
  theme_minimal()+
  ggtitle("Number of messages")

library("tidyr")
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis")


#################################

library("tidytext")
library("tidyr")
library("stopwords")
to_remove <- c(
               "media",
               "omitted",
               "ref",
               "dass",
               "schon",
               "hai",
               "nahi",
               "bhi",
               "mal",
               "android.s.wt")
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 10) %>%
  group_by(author) %>%
  top_n(n = 6, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Important words using tf-idf by author")


chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("unique words") +
  xlab("") +
  ggtitle("Lexical Diversity") +
  coord_flip()

