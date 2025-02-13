set.seed(999)
library(pcalg)
library(tidyverse)
library(reshape2)

setwd("~/McGill/projects/thesis/causal-inference-pilot")

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

code_to_name <- function(data) {
  case_match(
    data,
    "amh" ~ "Amharic",
    "aze" ~ "Azerbaijani",
    "ben" ~ "Bengali",
    "cat" ~ "Catalan",
    "ceb" ~ "Cebuano",
    "ces" ~ "Czech",
    "csb" ~ "Kashubian",
    "deu" ~ "German",
    "eng" ~ "English",
    "fra" ~ "French",
    "got" ~ "Gothic",
    "hbs" ~ "Serbo-Croatian",
    "hin" ~ "Hindi",
    "hun" ~ "Hungarian",
    "ind" ~ "Indonesian",
    "ita" ~ "Italian",
    "kaz" ~ "Kazakh",
    "kbd" ~ "Kabardian",
    "khk" ~ "Mongolian",
    "kir" ~ "Kyrgyz",
    "mao" ~ "Maori",
    "mlt" ~ "Maltese",
    "nld" ~ "Dutch",
    "nya" ~ "Chewa",
    "ood" ~ "O\"odham",
    "orm" ~ "Oromo",
    "pol" ~ "Polish",
    "por" ~ "Portuguese",
    "ron" ~ "Romanian",
    "rus" ~ "Russian",
    "sna" ~ "Shona",
    "spa" ~ "Spanish",
    "swc" ~ "Swahili",
    "swe" ~ "Swedish",
    "sqi" ~ "Albanian",
    "tel" ~ "Telugu",
    "tgk" ~ "Tajik",
    "tgl" ~ "Tagalog",
    "tuk" ~ "Turkmen",
    "tur" ~ "Turkish",
    "uig" ~ "Uyghur",
    "ukr" ~ "Ukrainian",
    "urd" ~ "Urdu",
    "uzb" ~ "Uzbek",
    "zul" ~ "Zulu",
    .default = data
  )
}

transform_data <- function(d) {
  d <- d %>%
    mutate(
      WL = (phon_len - mean(phon_len)) / sd(phon_len),
      logfreq = log(freq),
      FR = (log(freq) - mean(log(freq))) / sd(log(freq)),
      MI = (morph_complexity - mean(morph_complexity)) / sd(morph_complexity),
      PC = (phon_loss - mean(phon_loss)) / sd(phon_loss)
    )
  return(d)
}

data <- read.csv("processed.tsv", sep = "\t")

data$lang <- code_to_name(data$lang)
data$lang <- as.factor(data$lang)
data <- data %>% filter(count > 0)
data <- data %>% filter(!is.na(phon_loss))
data <- data %>% transform_data()

to_edges <- function(fci_out) {
  g <- melt(fci_out@amat)

  v <- as.vector(unique(c(g$Var1, g$Var2)))

  edges <- as.data.frame(t(combn(v, 2))) %>%
    setNames(c("var1", "var2"))

  edges$e1 <- mapply(
    function(v1, v2) {
      (g %>%
        filter(Var1 == v2, Var2 == v1) %>% first())$value
    },
    edges$var1,
    edges$var2
  )
  edges$e2 <- mapply(
    function(v1, v2) {
      (g %>%
        filter(Var1 == v1, Var2 == v2) %>% first())$value
    },
    edges$var1,
    edges$var2
  )
  edges %>%
    mutate(
      arrow1 = case_match(
        e1,
        0 ~ ".",
        1 ~ "o",
        2 ~ "<",
        3 ~ "-"
      ),
      arrow2 = case_match(
        e2,
        0 ~ ".",
        1 ~ "o",
        2 ~ ">",
        3 ~ "-"
      ),
      undir = if_else((e1 > 0) & (e2 > 0), 1, 0)
    ) %>%
    mutate(
      arrow = paste(arrow1, arrow2, sep = "-")
    ) %>%
    mutate(
      arrow = factor(arrow)
    )
}

doFCI <- function(d) {
  fci(
    suffStat = list(C = cor(d), n = nrow(d)),
    indepTest = gaussCItest,
    labels = colnames(d),
    alpha = 0.01
  )
}

bootFCI <- function(d, n, undir = FALSE) {
  edges <- list()
  for (i in 1:n) {
    boot_sample <- d %>% sample_n(nrow(d), replace = TRUE)
    edges[[i]] <- doFCI(boot_sample) %>% to_edges()
  }
  bind_rows(edges)
}

all_edges <- list()

for (i in seq_along(include_langs)) {
  l <- include_langs[[i]]
  d <- data %>%
    filter(lang == l) %>%
    dplyr::select(
      WL,
      PC,
      FR,
      MI
    )
  e <- bootFCI(d, 1000, FALSE)
  e$lang <- l
  all_edges[[i]] <- e
}

edges <- bind_rows(all_edges)

write.csv(edges, "edges.csv", quote = TRUE, row.names = FALSE)
