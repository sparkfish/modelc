test_that("Queries are constructed from simple linear models", {
  scipen_before <- getOption("scipen")
  options(scipen=999)

  a <- c(1, 2, 3, 4, 5)
  b <- c(1, 2, 3, 4, 5)
  df <- data.frame(a, b)
  model <- lm(b ~ a, data = df)

  intercept <- model$coefficients[["(Intercept)"]]
  a_coefficient <- model$coefficients[["a"]]

  expected <- paste(
    intercept, " + ", a_coefficient, "*a",
    sep=""
  )

  sql <- modelc::modelc(model)
  expect_equal(sql, expected)

  options(scipen=scipen_before)
})
