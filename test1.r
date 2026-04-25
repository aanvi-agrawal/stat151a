# Load libraries
library(ggplot2)

# Create data
x <- 1:10
y <- x^2

# Plot
plot(x, y, type = "b", col = "blue")

# Linear model
model <- lm(y ~ x)
summary(model)

# Code for the Data Cleaning Portion

```{r}
library(tidyverse)
library(readr)
```

```{r}
# Checking null values
raw_spotify = read.csv('spotify_songs.csv', na.strings = c("", "NA"))
raw_spotify |>
  filter(if_any(everything(), is.na))
```

```{r}
# column selection and type conversion
spotify = raw_spotify |>
  select(c('track_id', 'danceability', 'energy','key', 'loudness',	'mode',	'speechiness',	'acousticness',	'instrumentalness', 'liveness',	'valence',	'tempo',	'duration_ms')) |>
  mutate(key = as.double(key)) |>
  mutate(mode = as.double(mode)) |>
  mutate(duration_ms = as.double(duration_ms))
spotify
```

```{r}
spotify[,3:13] |>
  pivot_longer(cols = c(energy, key, loudness,	speechiness,	acousticness,	instrumentalness, liveness,	valence, tempo, duration_ms)) |>
  ggplot() +
  geom_boxplot(aes(x = value, y = name))
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = energy)) +
  theme_minimal()
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = key)) +
  theme_minimal()
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = loudness)) +
  theme_minimal()
min(spotify['loudness'])
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = speechiness)) +
  theme_minimal()
max(spotify['speechiness'])
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = acousticness)) +
  theme_minimal()
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = instrumentalness)) +
  theme_minimal()
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = liveness)) +
  theme_minimal()
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = valence)) +
  theme_minimal()
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = tempo)) +
  theme_minimal()
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = duration_ms)) +
  theme_minimal()
min(spotify['duration_ms'])
```

```{r}
spotify |>
  ggplot() +
  geom_boxplot(aes(x = danceability)) +
  theme_minimal()
```
