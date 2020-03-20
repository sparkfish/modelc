extract_parameters <- function(model) {
  return(names(coef(model)))  
}

extract_parameter_coefficient <- function(model, parameter) { 
  coefficient_value <- coef(model)[[parameter]]
  if (!is.na(coefficient_value)) { 
    return(coefficient_value)  
  } else {
    return(0) 
  }
}

build_column_name <- function(parameter) {
  return(
    paste("`", parameter, "`", sep="")
  )
}

build_product <- function(lhs, rhs) {
  return(
    paste(lhs, "*", rhs, sep="")
  )
}

is_intercept <- function(parameter) {
  return(parameter == '(Intercept)')
}

build_intercept <- function(model, parameter, first=FALSE) { 
  coefficient <- extract_parameter_coefficient(model, parameter)
  if (!first) {
    return(paste("+", coefficient))
  } else { 
    return(coefficient)
  }
}

build_additive_term <- function(model, additive_term, first=FALSE) {
  coefficient <- extract_parameter_coefficient(model, additive_term)
  column <- build_column_name(additive_term)
  if (!first) {
    return(paste("+", build_product(coefficient, column)))
  } else {
    return(build_product(coefficient, column))
  }
}

is_interaction <- function(parameter) {
  return(grepl(":", parameter, fixed=T));
}

is_factor <- function(parameter, model) {
  factorlist <- names(model$xlevels)
  for (factor in factorlist) { 
    if (grepl(factor, parameter, fixed=T)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

get_factor_name <- function(parameter, model) {
  factorlist <- names(model$xlevels)
  for (factor in factorlist) { 
    if (grepl(factor, parameter, fixed=T)) {
      return(factor)
    }
  }
  return("")
}

extract_level <- function(parameter, factor) {
  level_start <- nchar(factor) + 1
  level_end <- nchar(parameter)
  return(substring(parameter, level_start, level_end))
}

has_parameter <- function(model, parameter) { 
  parameter %in% names(coef(model));
}

build_interaction_term <- function(model, interaction_term, first=FALSE) { 
  
    split_interaction <- strsplit(interaction_term, ":")[[1]]
    coefficient <- extract_parameter_coefficient(model, interaction_term)
    
    sql <- paste(coefficient, "*", sep="")
    if (first) { 
      sql <- paste("+", sql) 
    }
    
    i = 0
    for (interaction_variable in split_interaction) {
      if (is_factor(interaction_variable, model)) {
        factor <- get_factor_name(interaction_variable, model)
        level <- extract_level(interaction_variable, factor)
        sql = paste(sql, "(CASE WHEN", factor, "=", level, "THEN", level, "ELSE", 0, "END)")
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
        sql = paste(sql, "WHEN", factor, "=", level, "THEN", coefficient, "*", factor)
      }
    }
    
    if (!(sql %in% c(SQL_START_FIRST, SQL_START))) {
      cases <- paste(cases, sql, "END)") 
    }

  }
  return(cases)
}

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


construct_select <- function(model) {
  parameters <- extract_parameters(model)
  select <- ""
  count <- 0 
  for (parameter in parameters) {
    if (is_intercept(parameter)) {
      build_term <- build_intercept
    }
    else if (is_factor(parameter, model)) {
      next;
    }
    else if (is_interaction(parameter)) {
      build_term <- build_interaction_term
    }
    else {
      build_term <- build_additive_term
    }
    select = paste(select, build_term(model, parameter, first=count==0))
    count = count + 1
  }
  
  select <- paste(
    select,
    build_factor_case_statements(model, first=count==0)
  )
  
  select_with_linkinverse <- apply_linkinverse(model, select)
  
  select <- paste("SELECT", select_with_linkinverse)
  
  return(select)
}
