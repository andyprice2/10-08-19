library(tidyverse)

dice <- function() {
  sample(1:6, size = 1, replace = TRUE)
}

twicedice <- function(n_pairs = 2) {
  results <- vector(mode = "integer", length = n_pairs)

  for (i in 1:n_pairs) {
    results[i] <- (dice() + dice())
  }

  print(results)
}

mapdice <- function(n_pairs = 2) {
  results <- vector(mode = "integer", length = n_pairs)
  map_int(results, dice)
  print(results)
}



x <- tibble(rolls = twicedice(100000))
ggplot(x, aes(x = rolls)) +
  geom_histogram()


roll_dice <- function(n = 1) {
  map_int(1:n, ~ dice() + dice())
}

x <- tibble(rolls = roll_dice(100000))

x <- x %>%
  mutate(include_7_or_11 = ifelse(rolls %in% c(7, 11), TRUE, FALSE)) %>%
  summarize(prop)

list_col <- tibble(
  replication = 1:100,
  throws=map(1:100, ~ roll_dice(7))
)

with_7_11 <- list_col %>%
  mutate(both_7_and_11 = ifelse(throws %>% c(7), TRUE, FALSE))

for
unlist(list_col[i, "throws"])




ggplot(x, aes(x = rolls)) +
  geom_histogram()






props <- x %>%
  count(rolls) %>%
  mutate(prop = n / sum(n)) %>%
  props() %>%
  filter(rolls == "7") %>%
  pull(prop) +
  props %>%
  filter(rolls == "11") %>%
  pull(prop)
