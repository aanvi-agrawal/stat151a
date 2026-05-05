model_quad <- lm(SalePrice ~ 
                   Bedroom.AbvGr + I(Bedroom.AbvGr^2) +
                   Total.Bath + I(Total.Bath^2) +
                   Gr.Liv.Area + Overall.Qual + Neighborhood,
                 data = ames)

summary(model_quad)
