# modelc

modelc is an R model object to SQL compiler. It generates SQL select statements from linear and generalized linear models. 

Its interface currently consists of a single function, `construct_select`, which takes a single input, namely an `lm` or `glm` model object.

It currently supports gamma and quasipoisson family distributions with log link functions. 

# Usage

Supposing the following data

```R
a <- 1:10
b <- 2*1:10 + runif(1) * 1.5
c <- as.factor(1:10)
df <- data.frame(a,b,c)
formula = b ~ a + c
```

A vanilla linear model

```R
linear_model <- lm(formula, data=df)
construct_select(linear_model)

```

generates the following SQL


``` sql
SELECT
  0.231808555545287 + 2 * `a` + (
    CASE
      WHEN c = 2 THEN -0.00000000000000193216758587821 * c
      WHEN c = 3 THEN -0.000000000000000776180314897008 * c
      WHEN c = 4 THEN -0.000000000000000665297412768863 * c
      WHEN c = 5 THEN -0.00000000000000055441451064072 * c
      WHEN c = 6 THEN -0.000000000000000887620818362638 * c
      WHEN c = 7 THEN -0.000000000000000332648706384432 * c
      WHEN c = 8 THEN -0.00000000000000110994422395641 * c
      WHEN c = 9 THEN -0.00000000000000188723974152839 * c
      WHEN c = 10 THEN 0 * c
    END
  )
```

GLMs are also supported with log or identity link functions


```R
glm_model <- glm(formula, data=df, family=Gamma(link="log"))
construct_select(glm_model)
```

``` sql
SELECT
  EXP(
    0.557874070609732 + 0.244938197625494 * `a` + (
      CASE
        WHEN c = 2 THEN 0.394878990324516 * c
        WHEN c = 3 THEN 0.536977925025217 * c
        WHEN c = 4 THEN 0.570378881020516 * c
        WHEN c = 5 THEN 0.542936294999294 * c
        WHEN c = 6 THEN 0.476536561025273 * c
        WHEN c = 7 THEN 0.383038044594683 * c
        WHEN c = 8 THEN 0.269593156578649 * c
        WHEN c = 9 THEN 0.140849942185343 * c
        WHEN c = 10 THEN 0 * c
      END
    )
  )
```


```R
glm_model_idlink <- glm(formula, data=df, family=Gamma(link="identity"))
construct_select(glm_model_idlink)
```

``` sql
SELECT
  0.231808555545287 + 2 * `a` + (
    CASE
      WHEN c = 2 THEN 0.00000000000000139594865689472 * c
      WHEN c = 3 THEN -0.000000000000000581567338978993 * c
      WHEN c = 4 THEN -0.00000000000000111588502938831 * c
      WHEN c = 5 THEN 0.000000000000000967650035758108 * c
      WHEN c = 6 THEN -0.00000000000000149265067586469 * c
      WHEN c = 7 THEN -0.000000000000000100985345060517 * c
      WHEN c = 8 THEN -0.0000000000000000673235633736781 * c
      WHEN c = 9 THEN 0.00000000000000199047558220559 * c
      WHEN c = 10 THEN 0 * c
    END
  )
```

Note that your R session should be configured with `options(scipen=999)` to disable rendering numbers with scientific notation, otherwise `construct_select` may output invalid SQL.
