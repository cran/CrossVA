xda_env <- new.env()
#' Map VA records to coding algorithm.
#'
#' \code{map_records} transform data collected with the WHO VA instrument
#'   to serve different alogrithms for coding cause of death.
#'
#' @param records A dataframe, obtained from reading an ODKBriefcase
#'   export of records collected with the WHO questionnaire.
#' @param mapping Name of an algorithm to map to (one of "interva4", "insilicova", or "tariff2""),
#'   or name of a mapping file.
#' @param csv_outfile Path to a file to write transformed data to.
#'   Defaults to empty string, in which case no file is written.
#' @return A dataframe, with the VA records mapped to the variables required
#'   by a coding algorithm, as specified in the mapping file.
#'
#' @examples
#'
#' record_f_name <- system.file('sample', 'who_va_output.csv', package = 'CrossVA')
#' records <- read.csv(record_f_name)
#' output_data <- map_records(records, 'insilicova')
#' output_f_name <- "output_for_insilicova.csv"
#' write.table(
#' output_data,
#' output_f_name,
#' row.names = FALSE,
#' na = "",
#' qmethod = "escape",
#' sep = ","
#' )
#' mapping_file <- system.file('mapping', 'interva4_mapping.txt', package = 'CrossVA')
#' output_data <- map_records(records, mapping_file)
#' output_f_name <- "output_for_interva4.csv"
#' write.table(
#' output_data,
#' output_f_name,
#' row.names = FALSE,
#' na = "",
#' qmethod = "escape",
#' sep = ","
#' )
#'
#' @export
#'
map_records <- function(records, mapping, csv_outfile = "") {
  if (mapping %in% c('interva4', 'insilicova', 'tariff2')){
    mapping_f_name <- system.file('mapping', paste(mapping, '_mapping.txt', sep = ''), package = 'CrossVA')
  }else{
    mapping_f_name <- mapping
  }
  map_def <- utils::read.delim(mapping_f_name)
  headers <- names(records)

  # number of variables required by coding algorithm
  target_n <- nrow(map_def)
  output_data <- data.frame(matrix(ncol = target_n))
  colnames(output_data) <- map_def[, 1]
  for (rec_count in 1:nrow(records)) {
    assign("rec_id", rec_count, envir = xda_env)
    record <- records[rec_count,]
    for (j in 1:length(headers)) {
      value <- as.character(record[1, j])
      val_as_num<-suppressWarnings(as.numeric(value))
      if (!is.na(val_as_num)){
        value<-val_as_num
      }
      header <- headers[j]
      header_cleaned <-
        regmatches(header, regexpr("[^\\.]*$", header))
      assign(header_cleaned, value, envir = xda_env)
    }
    current_data <- data.frame(matrix(ncol = target_n))
    for (i in 1:target_n) {
      target_var <- as.character(map_def[i, 1])
      expr <- as.character(map_def[i, 2])
      try(current_data[i] <- eval(parse(text = expr), envir = xda_env))
      if(is.na(current_data[i])){
        print(rec_count)
        print(target_var)
        print(expr)
        quit(1)
      }
      # make the value available for reference later in the destination var set
      name <- regmatches(target_var, regexpr("[^\\-]*$", target_var))
      name <- paste("t_", name, sep = "")
      assign(name, current_data[i][[1]], envir = xda_env)
    }
    output_data[rec_count,] <- current_data
  }
  if (csv_outfile != "") {
    utils::write.table(
      output_data,
      csv_outfile,
      row.names = FALSE,
      na = "",
      qmethod = "escape",
      sep = ","
    )
  }
  return(output_data)
}

#' Map VA records to InterVA4.
#'
#' \code{map_records} transform data collected with the WHO VA instrument
#'   for coding with the InterVA4 algorithm.
#'
#' @param records A dataframe, obtained from reading an ODKBriefcase
#'   export of records collected with the WHO questionnaire.
#' @param csv_outfile Path to a file to write transformed data to.
#'   Defaults to empty string, in which case no file is written.
#' @return A dataframe, with the VA records mapped to the variables required
#'   by InterVA4.
#'
#' @examples
#' \dontrun{
#' record_f_name <- system.file('sample', 'who_va_output.csv', package = 'CrossVA')
#' records <- read.csv(record_f_name)
#' output_data <- map_records_interva4(records)
#' }
#' @references http://www.interva.net/
#'
#' @export
#'
map_records_interva4 <- function(records, csv_outfile = "" ){
  return (map_records(records,"interva4", csv_outfile))
}

#' Map VA records to Tariff 2.
#'
#' \code{map_records} transform data collected with the WHO VA instrument
#'   for coding with the Tariff 2 algorithm.
#'
#' @param records A dataframe, obtained from reading an ODKBriefcase
#'   export of records collected with the WHO questionnaire.
#' @param csv_outfile Path to a file to write transformed data to.
#'   Defaults to empty string, in which case no file is written.
#' @return A dataframe, with the VA records mapped to the variables required
#'   by Tariff 2.
#'
#' @examples
#' \dontrun{
#' records <- read.csv('who_va_output.csv')
#' output_data <- map_records_tariff2(records)
#' }
#'
#' @references James, S. L., Flaxman, A. D., Murray, C. J., & Population Health Metrics Research Consortium. (2011). \emph{Performance of the Tariff Method: validation of a simple additive algorithm for analysis of verbal autopsies.} \emph{Population Health Metrics, 9(1), 1-16.}
#'
#' @export
#'
map_records_tariff2 <- function(records, csv_outfile = "" ){
  return (map_records(records,"tariff2", csv_outfile))
}

#' Map VA records to InsilicoVA.
#'
#' \code{map_records} transform data collected with the WHO VA instrument
#'   for coding with the InsilicoVA algorithm.
#'
#' @param records A dataframe, obtained from reading an ODKBriefcase
#'   export of records collected with the WHO questionnaire.
#' @param csv_outfile Path to a file to write transformed data to.
#'   Defaults to empty string, in which case no file is written.
#' @return A dataframe, with the VA records mapped to the variables required
#'   by InsilicoVA.
#'
#' @import stringi
#'
#' @examples
#' \dontrun{
#' records <- read.csv('who_va_output.csv')
#' output_data <- map_records_tariff2(records)
#' }
#'
#' @references Tyler H. McCormick, Zehang R. Li, Clara Calvert, Amelia C. Crampin, Kathleen Kahn and Samuel J. Clark (2014). Probabilistic cause-of-death assignment using verbal autopsies, Journal of the American Statistical Association, to appear
#'
#' @export
#'
map_records_insilicova <- function(records, csv_outfile = "" ){
  return (map_records(records,"insilicova", csv_outfile))
}

