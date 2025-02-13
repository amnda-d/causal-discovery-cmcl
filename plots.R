set.seed(999)
library(tidyverse)
library(paletteer)
library(ggridges)
library(patchwork)
library(ggthemes)

all_edges <- read.csv("edges.csv", header = TRUE, row.names = NULL)

include_langs <- c(
  "Albanian",
  "Amharic",
  "Azerbaijani",
  "Catalan",
  "Czech",
  "Dutch",
  "English",
  "French",
  "German",
  "Hungarian",
  "Italian",
  "Mongolian",
  "Polish",
  "Portuguese",
  "Romanian",
  "Russian",
  "Serbo-Croatian",
  "Spanish",
  "Swedish",
  "Turkish",
  "Ukrainian",
  "Zulu",
  "Kazakh",
  "Chewa",
  "Uzbek"
)

no_edge <- all_edges %>%
  mutate(
    var = factor(paste0(var1, "~", var2)),
    no_edge = arrow == ".-."
  ) %>%
  group_by(lang, no_edge, var, .drop = FALSE) %>%
  count() %>%
  filter(no_edge == FALSE) %>%
  mutate(type = "A.\nAny edge", n = n / 1000)

dir_edge <- all_edges %>%
  mutate(
    var = factor(paste0(var1, "~", var2)),
    dir_edge = arrow == "-->" | arrow == "<--"
  ) %>%
  group_by(lang, dir_edge, var, .drop = FALSE) %>%
  count() %>%
  filter(dir_edge == TRUE) %>%
  mutate(type = "B.\nDirected edge", n = n / 1000)

confound <- all_edges %>%
  mutate(
    var = factor(paste0(var1, "~", var2)),
    confound = arrow == "o->" | arrow == "<-o" | arrow == "<->" | arrow == "o-o"
  ) %>%
  group_by(lang, confound, var, .drop = FALSE) %>%
  count() %>%
  filter(confound == TRUE) %>%
  mutate(type = "C.\nPossible confounder", n = n / 1000)

plotdata <- bind_rows(no_edge, dir_edge, confound)

plotdata %>% ggplot(aes(x = n, fill = type)) +
  geom_histogram(
    aes(y = after_stat(count)),
    bins = 9
  ) +
  geom_hline(yintercept = 0) +
  theme_hc() +
  facet_grid(type ~ var, switch = "y") +
  scale_x_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.00),
    labels = c("0", ".2", ".4", ".6", ".8", "1")
  ) +
  scale_y_continuous(position = "right", limits = c(0, 25)) +
  theme(
    strip.text.x = element_text(margin = unit(rep(10, 4), "pt")),
    strip.text.y = element_text(margin = unit(rep(10, 4), "pt")),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c("#56B4E9", "#E69F00", "#009E73")
  ) +
  ylab("Languages") +
  xlab("Proportion of bootstrap samples")

ggsave("r/hist.png", width = 7, height = 5, units = "in")


# Most likely directed language

most_likely <- list()
for (i in seq_along(include_langs)) {
  e <- all_edges %>% filter(lang == include_langs[[i]])
  e <- e %>%
    group_by(var1, var2, arrow1, arrow2, lang) %>%
    count()

  most_likely[[i]] <- e %>%
    group_by(var1, var2, lang) %>%
    arrange(desc(n)) %>%
    slice(1)
}

# Most likely directed

lang_graphs <- bind_rows(most_likely)

dir <- bind_rows(most_likely) %>%
  group_by(var1, var2, arrow1, arrow2) %>%
  count() %>%
  group_by(var1, var2) %>%
  arrange(desc(n)) %>%
  slice(1)

dir

# most likely undirected

most_likely_undir <- list()
for (i in seq_along(include_langs)) {
  e <- all_edges %>% filter(lang == include_langs[[i]])
  e <- e %>%
    group_by(var1, var2, undir) %>%
    count()
  e$lang <- l
  most_likely_undir[[i]] <- e %>%
    group_by(var1, var2, lang) %>%
    arrange(desc(n)) %>%
    slice(1)
}

undir <- bind_rows(most_likely_undir) %>%
  group_by(var1, var2, undir) %>%
  count() %>%
  group_by(var1, var2) %>%
  arrange(desc(n)) %>%
  slice(1)

undir
