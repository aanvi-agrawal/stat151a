library(tidyverse)
library(corrplot)
library(car)        
library(broom)      
library(gridExtra)

spotify <- read.csv("spotify_songs.csv")

spotify_clean <- spotify |>
  select(danceability, energy, key, loudness, mode,
         speechiness, acousticness, instrumentalness,
         liveness, valence, tempo, duration_ms)

---

cor_matrix <- cor(spotify_clean, use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.8, number.cex = 0.7)

---

ggplot(spotify_clean, aes(danceability)) +
  geom_histogram(bins = 30) +
  ggtitle("Distribution of Danceability")

ggplot(spotify_clean, aes(tempo)) +
  geom_histogram(bins = 30) +
  ggtitle("Distribution of Tempo")

ggplot(spotify_clean, aes(energy)) +
  geom_histogram(bins = 30) +
  ggtitle("Distribution of Energy")

---

plot_list <- lapply(names(spotify_clean)[-1], function(var) {
  ggplot(spotify_clean, aes_string(x = var, y = "danceability")) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", color = "blue") +
    ggtitle(paste("Danceability vs", var))
})

do.call(grid.arrange, c(plot_list, ncol = 3))

---

model <- lm(danceability ~ energy + key + loudness + mode +
              speechiness + acousticness + instrumentalness +
              liveness + valence + tempo + duration_ms,
            data = spotify_clean)

tidy(model)
glance(model)

---

plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)
plot(model, which = 5)

---

vif(model)
