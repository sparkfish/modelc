#' @importFrom stats coef
#' @title Extract parameters from a linear model
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @return A character vector of terms from a linear model
extract_parameters <- function(model) {
  return(names(coef(model)))
}

#' @importFrom stats coef
#' @title Extract the coefficient of a model parameter
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @param parameter A character string corresponding to a model predictor
#' @return A double corresponding to the coefficient, or 0 if the coefficient is missing
extract_parameter_coefficient <- function(model, parameter) {
  coefficient_value <- coef(model)[[parameter]]
  if (!is.na(coefficient_value)) {
    return(coefficient_value)
  } else {
    return(0)
  }
}

#' @title Build a SQL product
#' @param lhs A character string representing the left hand side of the multiplication
#' @param rhs A character string representing the right hand side of the multiplication
#' @return A character string representing a valid SQL product term
build_product <- function(lhs, rhs) {
  return(
    paste(lhs, "*", rhs, sep="")
  )
}

#' @title Check if the given parameter is the intercept
#' @param parameter A parameter name.
#' @return A logical representing whether the given parameter is the intercept
is_intercept <- function(parameter) {
  return(parameter == '(Intercept)')
}

#' @title Get SQL representing the intercept term given the R model and parameter name
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @param parameter A parameter name.
#' @param first A logical flag signaling whether the term is the first term in the formula
#' @return A SQL character string representing the intercept term in the model
build_intercept <- function(model, parameter, first=FALSE) {
  coefficient <- extract_parameter_coefficient(model, parameter)
  if (!first) {
    return(paste("+", coefficient))
  } else {
    return(coefficient)
  }
}

#' Get SQL representing a continuous term in the model with no interactions
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @param additive_term A parameter name.
#' @param first A logical flag signaling whether the term is the first term in the formula
#' @return A SQL character string representing an additive term
build_additive_term <- function(model, additive_term, first=FALSE) {
  coefficient <- extract_parameter_coefficient(model, additive_term)
  column <- additive_term
  if (!first) {
    return(paste("+", build_product(coefficient, column)))
  } else {
    return(build_product(coefficient, column))
  }
}

#' @title Detect if the given model term is an interaction
#' @param parameter A parameter name.
#' @return A logical representing whether or not the term is an interaction
is_interaction <- function(parameter) {
  return(grepl(":", parameter, fixed=T));
}

#' @title Detect if the given model term is a factor
#' @param parameter A parameter name.
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @return A logical representing whether or not the term is a factor
is_factor <- function(parameter, model) {
  factorlist <- names(model$xlevels)
  for (factor in factorlist) {
    if (grepl(factor, parameter, fixed=T)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' @title Extract the factor name from an R model
#' @param parameter A parameter name.
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @return A character string representing the factor name
get_factor_name <- function(parameter, model) {
  factorlist <- names(model$xlevels)
  for (factor in factorlist) {
    if (grepl(factor, parameter, fixed=T)) {
      return(trimws(factor))
    }
  }
  return("")
}

#' @title Extract the level from the factor name
#' @param parameter A parameter name
#' @param factor A factor term
#' @return A SQL string literal representing the factor level
extract_level <- function(parameter, factor) {
  level_start <- nchar(factor) + 1
  level_end <- nchar(parameter)
  raw_level <- substring(parameter, level_start, level_end)
  return(paste("'", trimws(raw_level), "'", sep=""))
}

#' @importFrom stats coef
#' @title Check if an R model contains a coefficient
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @param parameter A parameter name
#' @return A logical representing whether a coefficient is present in the model
has_parameter <- function(model, parameter) {
  parameter %in% names(coef(model));
}

#' @title Build a SQL interaction term
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @param interaction_term The raw interaction term (a character string) from the R model
#' @param first A logical flag signaling whether the term is the first term in the formula
#' @return A character string representing a SQL interaction term
build_interaction_term <- function(model, interaction_term, first=FALSE) {

    split_interaction <- strsplit(interaction_term, ":")[[1]]
    coefficient <- extract_parameter_coefficient(model, interaction_term)

    sql <- paste(coefficient, "*", sep="")
    if (!first) {
      sql <- paste("+", sql)
    }

    i = 0

    for (interaction_variable in split_interaction) {
      if (is_factor(interaction_variable, model)) {
        factor <- get_factor_name(interaction_variable, model)
        level <- extract_level(interaction_variable, factor)
        sql = paste(sql, "(CASE WHEN", trimws(factor), "=", trimws(level), "THEN", 1, "ELSE", 0, "END)")
      }
      else {
        sql = paste(sql, interaction_variable, sep="")
      }

      if (i == 0) {
        sql = paste(sql, "*", sep="")
      }

      i = i + 1

    }
    return (sql)
}

#' @title Build SQL CASE statements representing the factors in the model
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @param first A logical flag signaling whether the term is the first term in the formula
#' @return A character string representing a SQL CASE statement
build_factor_case_statements <- function(model, first=F) {
  SQL_START_FIRST <- "(CASE"
  SQL_START <- "+ (CASE"
  factors <- model$xlevels
  factor_variables <- names(factors)
  cases <- ""
  for (factor in factor_variables) {
    if (!first) {
      sql = SQL_START
    } else {
      sql <- SQL_START_FIRST
    }
    for (level in factors[[factor]]) {
      formula_term <- paste(factor, level, sep="")
      if (has_parameter(model, formula_term)) {
        coefficient <- extract_parameter_coefficient(model, formula_term)
        level <- paste("'", trimws(level), "'", sep="")
        sql = paste(sql, "WHEN", trimws(factor), "=", level, "THEN", coefficient)
      }
    }

    if (!(sql %in% c(SQL_START_FIRST, SQL_START))) {
      cases <- paste(cases, sql, "ELSE 0 END)")
    }

  }
  return(cases)
}

#' @title Wrap the model SQL in the appropriate link function inverse to return scaled predictions
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @param sql A character string representing the SQL to be wrapped in the link inverse
#' @return A character string representing a SQL model formula
apply_linkinverse <- function(model, sql) {
  if (is.null(model$family)) {
    return(sql)
  }

  if (model$family$link == "identity") {
    return(sql)
  }

  else if(model$family$link == "log") {
    sql <- paste("EXP(", sql, ")", sep="")
    return(sql)
  }
  else {
    stop("Unsupported link function passed:\n", model$family$link, "\n", "Supported link functions are: log, identity")
  }
}

#' @title Compile an R model to a valid TSQL formula
#' @param model A list with the same signature as the output of \code{lm} or \code{glm}
#' @param modify_scipen A boolean indicating whether to modify the "scipen" option to avoid generating invalid SQL
#' @return A character string representing a SQL model formula
#' @export
modelc <- function(model, modify_scipen = FALSE) {

  # Disable scientific notation to avoid generation of invalid SQL
  if (modify_scipen) {
    scipen_previous <- getOption("scipen")
    options(scipen=999)
   }

  parameters <- extract_parameters(model)
  select <- ""
  count <- 0
  for (parameter in parameters) {

    if (is_intercept(parameter)) {
      build_term <- build_intercept
    }
    else if (is_interaction(parameter)) {
      build_term <- build_interaction_term
    }
    else if (is_factor(parameter, model)) {
      next;
    }
    else {
      build_term <- build_additive_term
    }
    select = paste(select, build_term(model, parameter, first=count==0))
    count = count + 1
  }

  select <- paste(
    select,
    " ",
    build_factor_case_statements(model, first=count==0),
    sep=""
  )

  select_with_linkinverse <- apply_linkinverse(model, select)

  select <- gsub("  ", " ", trimws(select_with_linkinverse))

 # Restore the original scipen setting
 if (modify_scipen) {
    options(scipen=scipen_previous)
 }
 return(select)
}
