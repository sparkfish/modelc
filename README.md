# modelc

[![R build
status](https://github.com/sparkfish/modelc/workflows/R-CMD-check/badge.svg)](https://github.com/sparkfish/modelc/actions)

modelc is an R model object to SQL compiler. It generates SQL select statements from linear and generalized linear models.

Its interface currently consists of a single function, `modelc`, which takes a single input, namely an `lm` or `glm` model object.

It currently supports Gaussian and gamma family distributions using log or identity link functions.

To import linear models directly to your SQL Server database, consider using [Castpack](https://github.com/sparkfish/castpack), which depends on `modelc`.

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
modelc(linear_model)

```

generates the following SQL


``` sql
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
modelc(glm_model)
```

``` sql
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
modelc(glm_model_idlink)
```

``` sql
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

In order to avoid generating invalid SQL, `modelc` temporarily sets your `scipen` option to 999.

# Installing

Using `devtools`:

```R
install.packages("devtools")
install.packages("remotes")
remotes::install_github("sparkfish/modelc")
```

# Precision

Note that you may encounter minor differences between the output of your R and generated SQL models depending on the precision with which your numeric types are represented in the database. To ensure parity between the two models, numeric types should have a precision of at least 17.

# Tests

Tests are written using `testthat`. To run them, simply do

``` R
devtools::test()
```
