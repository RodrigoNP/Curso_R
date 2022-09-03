#### 
# Running Regressions
##

## Multinomial Logit Model

# ln(s_j) - ln(s_0) = X_j \beta - \alpha p_j + \varepsilon_j

ols_model <- felm( y ~
                    price +
                    x_j |0|0|0,
                  data = chips)


iv_model <- ivreg( y ~  x_j | price | whole_cost,
                   data = chips)


