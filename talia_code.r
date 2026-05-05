```{r}
model_all <- lm(SalePrice ~ Bedroom.AbvGr + Total.Bath +
              Gr.Liv.Area + Overall.Qual +
              Year.Built + Neighborhood +
              Bedroom.AbvGr * Gr.Liv.Area +
              Total.Bath * Gr.Liv.Area + 
              Bedroom.AbvGr * Overall.Qual +
              Total.Bath * Overall.Qual +
              Bedroom.AbvGr * Year.Built +
              Total.Bath * Year.Built +
              Bedroom.AbvGr * Neighborhood + 
              Total.Bath * Neighborhood +
              Gr.Liv.Area * Overall.Qual +
              Overall.Qual * Year.Built +
              Neighborhood * Overall.Qual +
              Bedroom.AbvGr * Total.Bath
              ,
            data = ames)
summary(model_all)
```
# the above is not great - wide confidence interval


```{r}
model_quad <- lm(SalePrice ~ 
                   Bedroom.AbvGr + I(Bedroom.AbvGr^2) +
                   Total.Bath + I(Total.Bath^2) +
                   Gr.Liv.Area + Overall.Qual + Neighborhood,
                 data = ames)

summary(model_quad)
```
# maybe build off of this one?
-
# this one is interesting because it shows that as you add more bedrooms, 
# the price reduction increases but as you add more bathrooms, your price 
# actually increases



```{r}

model_ratio <- lm(SalePrice ~ 
                    I(Bedroom.AbvGr / Gr.Liv.Area) +
                    I(Total.Bath / Gr.Liv.Area) +
                    Gr.Liv.Area +
                    Overall.Qual + Neighborhood,
                  data = ames)

summary(model_ratio)

```
