
rm(list = ls())

required_pkgs <- c(
  "tidyverse", "readxl", "janitor", "scales", "caret"
)
installed <- rownames(installed.packages())
for (p in required_pkgs) {
  if (!p %in% installed) install.packages(p, dependencies = TRUE)
}

library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(caret)

set.seed(42)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

df_raw <- read_excel("data/Spotify_data.xlsx") %>%
  clean_names()

cat("Rows:", nrow(df_raw), "\n")
cat("Cols:", ncol(df_raw), "\n")
cat("Columns:\n")
print(names(df_raw))

col_song  <- intersect(names(df_raw), c("song", "song_name", "title", "track_name", "name"))
col_artist <- intersect(names(df_raw), c("artist", "artists", "singer", "artist_name"))
col_year  <- intersect(names(df_raw), c("year", "release_year"))
col_rank  <- intersect(names(df_raw), c("rank", "position", "billboard_rank"))

if (length(col_song) == 0) stop("Couldn't find song/title column. Check dataset column names.")
if (length(col_artist) == 0) stop("Couldn't find artist column. Check dataset column names.")
if (length(col_year) == 0) stop("Couldn't find year column. Check dataset column names.")
if (length(col_rank) == 0) stop("Couldn't find rank/position column. Check dataset column names.")

df <- df_raw %>%
  mutate(
    song = .data[[col_song[1]]],
    artist = .data[[col_artist[1]]],
    year = as.integer(.data[[col_year[1]]]),
    rank = as.integer(.data[[col_rank[1]]])
  )

df <- df %>%
  mutate(
    artist_lower = str_to_lower(artist),
    is_collab = str_detect(artist_lower, "feat\.|featuring|&|,| x | and ")
  )

df_unique <- df %>%
  arrange(year, rank) %>%
  group_by(year, song) %>%
  slice(1) %>%
  ungroup()

cat("Rows after dedupe:", nrow(df_unique), "\n")

collab_trend <- df_unique %>%
  filter(!is.na(year), year >= 2000, year <= 2023) %>%
  group_by(year) %>%
  summarise(
    n_songs = n(),
    pct_collab = mean(is_collab, na.rm = TRUE)
  ) %>%
  ungroup()

p_collab <- ggplot(collab_trend, aes(x = year, y = pct_collab)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Collaboration trend in Billboard Hot 100 songs (2000–2023)",
    x = "Year",
    y = "% of songs with multiple credited artists"
  ) +
  theme_minimal()

ggsave("figures/plot_collaboration_trend.png", p_collab, width = 10, height = 5, dpi = 300)

feature_candidates <- c(
  "danceability", "energy", "key", "loudness", "mode",
  "speechiness", "acousticness", "instrumentalness", "liveness",
  "valence", "tempo", "duration_ms", "time_signature"
)

available_features <- intersect(names(df_unique), feature_candidates)
cat("Available features:\n")
print(available_features)

numeric_features <- intersect(
  available_features,
  c("danceability", "energy", "loudness", "speechiness", "acousticness",
    "instrumentalness", "liveness", "valence", "tempo", "duration_ms")
)

if (length(numeric_features) < 5) {
  warning("Few Spotify features found. Modeling/clustering may be limited.")
}

df_feat <- df_unique %>%
  filter(year >= 2000, year <= 2023)

df_feat <- df_feat %>%
  filter(if_any(all_of(numeric_features), ~ !is.na(.)))

cat("Rows with at least some audio features:", nrow(df_feat), "\n")

df_feat_2000_2004 <- df_feat %>%
  filter(year >= 2000, year <= 2004)

cat("Rows with audio features (2000–2004):", nrow(df_feat_2000_2004), "\n")

if (length(numeric_features) > 0 && nrow(df_feat_2000_2004) > 50) {

  df_long <- df_feat_2000_2004 %>%
    select(all_of(numeric_features)) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

  p_dist <- ggplot(df_long, aes(x = value)) +
    geom_histogram(bins = 30) +
    facet_wrap(~ feature, scales = "free", ncol = 3) +
    labs(
      title = "Distribution of Spotify audio features (2000–2004 subset)",
      x = "Value",
      y = "Count"
    ) +
    theme_minimal()

  ggsave("figures/plot_feature_distributions_2000_2004.png", p_dist, width = 12, height = 8, dpi = 300)
}

if (length(numeric_features) >= 5 && nrow(df_feat_2000_2004) > 50) {

  corr_data <- df_feat_2000_2004 %>%
    select(all_of(numeric_features)) %>%
    drop_na()

  corr_mat <- cor(corr_data)

  corr_df <- as.data.frame(as.table(corr_mat)) %>%
    rename(var1 = Var1, var2 = Var2, corr = Freq)

  p_corr <- ggplot(corr_df, aes(x = var1, y = var2, fill = corr)) +
    geom_tile() +
    geom_text(aes(label = round(corr, 2)), size = 3) +
    labs(
      title = "Correlation matrix of Spotify audio features (2000–2004 subset)",
      x = "", y = ""
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("figures/plot_feature_correlations_2000_2004.png", p_corr, width = 10, height = 8, dpi = 300)
}

if (nrow(df_feat_2000_2004) > 100 && "rank" %in% names(df_feat_2000_2004)) {

  model_df <- df_feat_2000_2004 %>%
    mutate(top10 = factor(if_else(rank <= 10, "Top10", "NotTop10"))) %>%
    select(top10, all_of(numeric_features)) %>%
    drop_na()

  nzv <- nearZeroVar(model_df %>% select(-top10), saveMetrics = TRUE)
  keep_cols <- rownames(nzv)[!nzv$nzv]
  model_df <- model_df %>% select(top10, all_of(keep_cols))

  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )

  model_df$top10 <- relevel(model_df$top10, ref = "Top10")

  glm_fit <- train(
    top10 ~ .,
    data = model_df,
    method = "glm",
    family = "binomial",
    trControl = ctrl,
    metric = "ROC"
  )

  sink("outputs/model_results.txt")
  cat("===== Logistic Regression (Top10 vs NotTop10) =====\n\n")
  print(glm_fit)
  cat("\n\nCoefficients:\n")
  print(summary(glm_fit$finalModel))
  sink()

  vi <- varImp(glm_fit)
  png("figures/plot_model_variable_importance.png", width = 1200, height = 700, res = 150)
  plot(vi, main = "Variable importance (Logistic Regression)")
  dev.off()
}

if (length(numeric_features) >= 5 && nrow(df_feat_2000_2004) > 100) {

  clust_df <- df_feat_2000_2004 %>%
    select(all_of(numeric_features)) %>%
    drop_na()

  clust_scaled <- scale(clust_df)

  k <- 4
  km <- kmeans(clust_scaled, centers = k, nstart = 25)

  pca <- prcomp(clust_scaled)
  pca_df <- as_tibble(pca$x[, 1:2]) %>%
    mutate(cluster = factor(km$cluster))

  p_km <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(alpha = 0.8) +
    labs(
      title = "K-means clusters (k=4) visualised using PCA (2000–2004)",
      x = "PC1",
      y = "PC2"
    ) +
    theme_minimal()

  ggsave("figures/plot_kmeans_pca_clusters.png", p_km, width = 9, height = 6, dpi = 300)

  cluster_profiles <- as_tibble(clust_df) %>%
    mutate(cluster = factor(km$cluster)) %>%
    group_by(cluster) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

  write_csv(cluster_profiles, "outputs/cluster_profiles.csv")
}

cat("\nDONE ✅\nCheck /figures for plots and /outputs for results.\n")
