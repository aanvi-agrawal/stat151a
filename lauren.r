## Some exploration

```{r}
library(splines2)

ames1 <- read.csv("~/Desktop/STAT 151A/Final Project/ames_de_cock.csv")

ames1 |>
  ggplot(aes(x=Overall.Qual,y=SalePrice)) + 
  geom_point()
cor.test(ames1$Overall.Qual,ames1$SalePrice)

lmQual_Sale <- lm(SalePrice ~ Overall.Qual, ames1)
summary(lmQual_Sale)

num_knots <- 5
quants <- 1:num_knots / (num_knots + 1)
knots <- quantile(ames1$Overall.Qual, probs = quants, na.rm = TRUE)
regs <- bSpline(
  ames1$Overall.Qual,
  knots = knots,
  degree = 0
)

x <- seq(
  min(ames1$Overall.Qual),
  max(ames1$Overall.Qual),
  length.out = 100
)

newdata <- data.frame(Overall.Qual = x)

lmQual_Sale_fit <- lm(SalePrice ~ bSpline(Overall.Qual, df=4, degree=2), ames1)

yhat <- predict(lmQual_Sale_fit, newdata = newdata)

ggplot(ames, aes(x = Overall.Qual, y = SalePrice)) +
  geom_point(alpha = 0.3) +
  geom_line(
    data = data.frame(Overall.Qual = x, yhat = yhat),
    aes(x = Overall.Qual, y = yhat),
    color = "red",
    linewidth = 1.6
  )

```


