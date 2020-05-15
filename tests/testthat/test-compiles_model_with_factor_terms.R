test_that("Queries are constructed from linear models containing factors", {
  scipen_before <- getOption("scipen")
  options(scipen=999)

  a <- 1:10
  b <- 2*1:10
  c <- as.factor(1:10)
  df <- data.frame(a,b,c)
  formula = b ~ a + c
  model <- lm(formula, data = df)

  intercept <- model$coefficients[["(Intercept)"]]
  a_coefficient <- model$coefficients[["a"]]
  c2_coefficient <- model$coefficients[["c2"]]
  c3_coefficient <- model$coefficients[["c3"]]
  c4_coefficient <- model$coefficients[["c4"]]
  c5_coefficient <- model$coefficients[["c5"]]
  c6_coefficient <- model$coefficients[["c6"]]
  c7_coefficient <- model$coefficients[["c7"]]
  c8_coefficient <- model$coefficients[["c8"]]
  c9_coefficient <- model$coefficients[["c9"]]
  c10_coefficient <- model$coefficients[["c10"]]

  expected <- paste(
    intercept, " + ", a_coefficient, "*a", " + ",
    "(CASE WHEN c = ", "'2'", " THEN ", c2_coefficient,
    " WHEN c = ", "'3'", " THEN ", c3_coefficient,
    " WHEN c = ", "'4'", " THEN ", c4_coefficient,
    " WHEN c = ", "'5'", " THEN ", c5_coefficient,
    " WHEN c = ", "'6'", " THEN ", c6_coefficient,
    " WHEN c = ", "'7'", " THEN ", c7_coefficient,
    " WHEN c = ", "'8'", " THEN ", c8_coefficient,
    " WHEN c = ", "'9'", " THEN ", c9_coefficient,
    " WHEN c = ", "'10'", " THEN ", 0,
    " ELSE 0 END)",
    sep=""
  )

  sql <- modelc::modelc(model)

  expect_equal(sql, expected)

  options(scipen=scipen_before)
})
