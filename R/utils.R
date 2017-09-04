# map true to character 'y', return "" for everything else
true_to_y <- function(expr) {
  value<-""
  try(value<-eval(parse(text = expr), envir = xda_env))
  if (is.na(value) || value == "" || value == FALSE){
    return("")
  } else {
    return("y")
  }
}

# map TRUE to character 'y', FALSE to '',  or return '.' if the expression cannot be evaluated because of an NA
true_to_y_dot <- function(expr) {
  try(value<-eval(parse(text = expr), envir = xda_env))
  if (is.na(value)){
    return(".")
  }
  if (value == TRUE) {
    return("y")
  }
  if (value == FALSE) {
    return("")
  }
  return(".")
}

# return 'y' if any of the strings evaluates to TRUE, return "" for everything else
any_to_y <- function(expressions) {
  if (any(expressions, na.rm = TRUE)){
    return("y")
  }
  else{
    return("")
  }
}

# return '' if any of the strings evaluates to TRUE, return "y" for everything else
any_to_empty <- function(expressions) {
  if (any(expressions, na.rm = TRUE)){
    return("")
  }
  else{
    return("y")
  }
}

#get if exists, return default otherwise
g_or_d <- function(var_name, default = ""){
  #print(var_name)
  value <- get(var_name, envir = xda_env)
  if (is.na(value)){
    return(default)
  }
  return(value)
}

# evaluate expression, return default on empty
exp_def <- function(expr, default) {
  value<-""
  try(value<-eval(parse(text = expr), envir = xda_env))
  if (is.null(value) ||  is.na(value) || nchar(value) < 1) {
    return(default)
  } else {
    return(value)
  }
}

multi_select_contains <- function(what, who_id) {
  if (is.na(who_id)) {
    return(FALSE)
  }
  split_expression <- as.list(strsplit(who_id, " ")[[1]])
  found <- FALSE
  for (selection in split_expression) {
    if (grepl(what, selection)) {
      found <- TRUE
    }
  }
  return(found)
}

yes_to_code <- function(qlist, clist, default) {
  code <- ""
  for (i in 1:length(qlist)) {
    who_value <- get(qlist[i], envir = xda_env)
    if (!is.na(who_value)){
      if (who_value == "yes") {
        code <- paste(code, clist[i])
      }
    }
  }
  # use default if code is empty
  if (nchar(code) == 0) {
    code <- default
  }
  return(trimws(code))
}

# from_list: upper limits of range to_list: codes to map to
range_to_code <- function(from_list, to_list, default, who_id) {
  code <- default
  try({
    value <- as.numeric(get(who_id, envir = xda_env))
    for (i in 1:length(to_list)) {
      if (value > from_list[i] && value <= from_list[i + 1]) {
        code <- to_list[i]
      }
    }
  }, silent = TRUE)
  return(code)
}

#map between sets of codes
map_code <- function(from_list, to_list, who_id) {
  code <- ""
  value <- get(who_id, envir = xda_env)
  if (is.na(value)){
    return(code)
  }
  for (i in 1:length(from_list)) {
    if (from_list[i] == value) {
      code <- to_list[i]
    }
  }
  # leave empty if no match
  return(code)
}

#Create ODK-style muli-select answer (chosen options listed separated by space)
map_multi_code <- function(from_list, to_list, who_id) {
  code <- ""
  values <- strsplit(as.character(get(who_id), envir = xda_env), " ")[[1]]
  for (i in 1:length(from_list)) {
    if (any(from_list[i] == values)) {
      code <- paste(code, to_list[i])
    }
  }
  # leave empty if no match
  return(trimws(code))
}


#Create ODK-style muli-select answer (chosen options listed separated by space)
map_to_multi <- function(from_who_list, to_list) {
  code <- ""
  for (i in 1:length(from_who_list)) {
    if (get(from_who_list[i], envir = xda_env) != "") {
      code <- paste(code, to_list[i])
    }
  }
  # leave empty if no match
  return(trimws(code))
}


# get year from a date string, slightly more safe than just substringing
get_year <- function(date_string) {
  year <- lubridate::year(lubridate::parse_date_time(c(date_string), c("dmY"), quiet = TRUE)[1])
  return (ifelse(is.na(year), "", year))
}

# get (numeric) month from a date string, slightly more safe than just substringing
get_month <- function(date_string) {
  month <- lubridate::month(lubridate::parse_date_time(c(date_string), c("dmY"), quiet = TRUE)[1])
  return (ifelse(is.na(month), "", month))
}

# get day from a date string, slightly more safe than just substringing
get_day <- function(date_string) {
  day <- lubridate::day(lubridate::parse_date_time(c(date_string), c("dmY"), quiet = TRUE)[1])
  return (ifelse(is.na(day), "", day))
}
