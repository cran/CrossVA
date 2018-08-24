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

#' Map VA records to coding algorithms in openVA package: InterVA5 and InSilico (with option data.type = "WHO2016").
#'
#' \code{odk2openVA} transform data collected with the 2016 WHO VA  instrument (version 1.4.1)
#'   to serve different InterVA5 and InSilicoVA alogrithms for coding cause of death.
#'
#' @param odk A dataframe, obtained from reading an ODKBriefcase
#'   export of records collected with the WHO questionnaire.
#'
#' @examples
#' \dontrun{
#' records <- read.csv('who_va_output.csv')
#' output_data_IV5 <- odk2InterVA5(records)
#' }
#'
#' @export
#'
odk2openVA <- function(odk){

	## Input Data
	# odk <- read.csv("ODKExportNew.csv")
	odkNames <- tolower(names(odk))

	## Output Variables
	whoNames <- c("Id10004", "Id10004", "Id10019", "Id10019", "Id10022", "Id10022", "Id10022",
		"Id10022", "Id10022", "Id10022", "Id10022", "Id10022", "Id10022", "Id10022",
		"Id10022", "Id10022", "Id10022", "Id10022", "Id10059", "Id10077", "Id10079",
		"Id10082", "Id10083", "Id10084", "Id10085", "Id10086", "Id10087", "Id10089",
		"Id10090", "Id10091", "Id10092", "Id10093", "Id10094", "Id10095", "Id10096",
		"Id10098", "Id10099", "Id10100", "Id10104", "Id10105", "Id10106", "Id10107",
		"Id10108", "Id10109", "Id10110", "Id10111", "Id10112", "Id10113", "Id10114",
		"Id10115", "Id10116", "Id10120", "Id10120", "Id10123", "Id10125", "Id10127",
		"Id10128", "Id10129", "Id10130", "Id10131", "Id10132", "Id10133", "Id10134",
		"Id10135", "Id10136", "Id10137", "Id10138", "Id10139", "Id10140", "Id10141",
		"Id10142", "Id10143", "Id10144", "Id10147", "Id10148", "Id10148", "Id10148",
		"Id10149", "Id10150", "Id10151", "Id10152", "Id10153", "Id10154", "Id10154",
		"Id10155", "Id10156", "Id10157", "Id10158", "Id10159", "Id10161", "Id10165",
		"Id10166", "Id10167", "Id10167", "Id10168", "Id10169", "Id10169", "Id10170",
		"Id10171", "Id10172", "Id10173", "Id10174", "Id10175", "Id10176", "Id10178",
		"Id10181", "Id10182", "Id10182", "Id10182", "Id10183", "Id10184", "Id10185",
		"Id10186", "Id10187", "Id10188", "Id10189", "Id10190", "Id10191", "Id10192",
		"Id10193", "Id10194", "Id10195", "Id10197", "Id10197", "Id10199", "Id10199",
		"Id10200", "Id10201", "Id10201", "Id10203", "Id10204", "Id10205", "Id10205",
		"Id10207", "Id10208", "Id10209", "Id10209", "Id10210", "Id10211", "Id10212",
		"Id10213", "Id10214", "Id10215", "Id10216", "Id10217", "Id10218", "Id10219",
		"Id10220", "Id10221", "Id10221", "Id10222", "Id10223", "Id10224", "Id10225",
		"Id10226", "Id10227", "Id10228", "Id10229", "Id10230", "Id10231", "Id10232",
		"Id10233", "Id10234", "Id10234", "Id10235", "Id10235", "Id10235", "Id10235",
		"Id10236", "Id10237", "Id10238", "Id10239", "Id10240", "Id10241", "Id10242",
		"Id10243", "Id10244", "Id10245", "Id10246", "Id10247", "Id10248", "Id10249",
		"Id10250", "Id10251", "Id10252", "Id10253", "Id10254", "Id10255", "Id10256",
		"Id10257", "Id10258", "Id10259", "Id10260", "Id10260", "Id10260", "Id10260",
		"Id10260", "Id10260", "Id10260", "Id10261", "Id10262", "Id10263", "Id10263",
		"Id10264", "Id10265", "Id10266", "Id10267", "Id10268", "Id10269", "Id10270",
		"Id10271", "Id10272", "Id10273", "Id10274", "Id10275", "Id10276", "Id10277",
		"Id10278", "Id10279", "Id10281", "Id10282", "Id10283", "Id10284", "Id10285",
		"Id10286", "Id10287", "Id10288", "Id10289", "Id10290", "Id10294", "Id10295",
		"Id10296", "Id10297", "Id10298", "Id10299", "Id10300", "Id10301", "Id10302",
		"Id10303", "Id10304", "Id10305", "Id10306", "Id10309", "Id10310", "Id10312",
		"Id10313", "Id10314", "Id10315", "Id10316", "Id10317", "Id10318", "Id10319",
		"Id10319", "Id10320", "Id10321", "Id10322", "Id10323", "Id10324", "Id10325",
		"Id10326", "Id10327", "Id10328", "Id10329", "Id10330", "Id10331", "Id10332",
		"Id10333", "Id10334", "Id10335", "Id10336", "Id10337", "Id10337", "Id10337",
		"Id10338", "Id10340", "Id10342", "Id10343", "Id10344", "Id10347", "Id10354",
		"Id10355", "Id10356", "Id10357", "Id10358", "Id10360", "Id10360", "Id10360",
		"Id10361", "Id10362", "Id10363", "Id10364", "Id10365", "Id10367", "Id10367",
		"Id10367", "Id10368", "Id10369", "Id10370", "Id10371", "Id10372", "Id10373",
		"Id10376", "Id10377", "Id10382", "Id10383", "Id10384", "Id10385", "Id10387",
		"Id10388", "Id10389", "Id10391", "Id10393", "Id10394", "Id10394", "Id10395",
		"Id10396", "Id10397", "Id10398", "Id10399", "Id10400", "Id10401", "Id10402",
		"Id10403", "Id10404", "Id10405", "Id10406", "Id10408", "Id10411", "Id10412",
		"Id10413", "Id10414", "Id10415", "Id10418", "Id10419", "Id10420", "Id10421",
		"Id10422", "Id10423", "Id10424", "Id10425", "Id10426", "Id10427", "Id10428",
		"Id10450", "Id10451", "Id10452", "Id10453", "Id10454", "Id10455", "Id10456",
		"Id10457", "Id10458", "Id10459")
        whoNames <- tolower(whoNames)

	iv5Names <- c("i004a", "i004b", "i019a", "i019b", "i022a", "i022b", "i022c", "i022d",
		"i022e", "i022f", "i022g", "i022h", "i022i", "i022j", "i022k", "i022l",
		"i022m", "i022n", "i059o", "i077o", "i079o", "i082o", "i083o", "i084o",
		"i085o", "i086o", "i087o", "i089o", "i090o", "i091o", "i092o", "i093o",
		"i094o", "i095o", "i096o", "i098o", "i099o", "i100o", "i104o", "i105o",
		"i106a", "i107o", "i108a", "i109o", "i110o", "i111o", "i112o", "i113o",
		"i114o", "i115o", "i116o", "i120a", "i120b", "i123o", "i125o", "i127o",
		"i128o", "i129o", "i130o", "i131o", "i132o", "i133o", "i134o", "i135o",
		"i136o", "i137o", "i138o", "i139o", "i140o", "i141o", "i142o", "i143o",
		"i144o", "i147o", "i148a", "i148b", "i148c", "i149o", "i150a", "i151a",
		"i152o", "i153o", "i154a", "i154b", "i155o", "i156o", "i157o", "i158o",
		"i159o", "i161a", "i165a", "i166o", "i167a", "i167b", "i168o", "i169a",
		"i169b", "i170o", "i171o", "i172o", "i173a", "i174o", "i175o", "i176a",
		"i178a", "i181o", "i182a", "i182b", "i182c", "i183a", "i184a", "i185o",
		"i186o", "i187o", "i188o", "i189o", "i190o", "i191o", "i192o", "i193o",
		"i194o", "i195o", "i197a", "i197b", "i199a", "i199b", "i200o", "i201a",
		"i201b", "i203a", "i204o", "i205a", "i205b", "i207o", "i208o", "i209a",
		"i209b", "i210o", "i211a", "i212o", "i213o", "i214o", "i215o", "i216a",
		"i217o", "i218o", "i219o", "i220o", "i221a", "i221b", "i222o", "i223o",
		"i224o", "i225o", "i226o", "i227o", "i228o", "i229o", "i230o", "i231o",
		"i232a", "i233o", "i234a", "i234b", "i235a", "i235b", "i235c", "i235d",
		"i236o", "i237o", "i238o", "i239o", "i240o", "i241o", "i242o", "i243o",
		"i244o", "i245o", "i246o", "i247o", "i248a", "i249o", "i250a", "i251o",
		"i252o", "i253o", "i254o", "i255o", "i256o", "i257o", "i258o", "i259o",
		"i260a", "i260b", "i260c", "i260d", "i260e", "i260f", "i260g", "i261o",
		"i262a", "i263a", "i263b", "i264o", "i265o", "i266a", "i267o", "i268o",
		"i269o", "i270o", "i271o", "i272o", "i273o", "i274a", "i275o", "i276o",
		"i277o", "i278o", "i279o", "i281o", "i282o", "i283o", "i284o", "i285a",
		"i286o", "i287o", "i288o", "i289o", "i290o", "i294o", "i295o", "i296o",
		"i297o", "i298o", "i299o", "i300o", "i301o", "i302o", "i303a", "i304o",
		"i305o", "i306o", "i309o", "i310o", "i312o", "i313o", "i314o", "i315o",
		"i316o", "i317o", "i318o", "i319a", "i319b", "i320o", "i321o", "i322o",
		"i323o", "i324o", "i325o", "i326o", "i327o", "i328o", "i329o", "i330o",
		"i331o", "i332a", "i333o", "i334o", "i335o", "i336o", "i337a", "i337b",
		"i337c", "i338o", "i340o", "i342o", "i343o", "i344o", "i347o", "i354o",
		"i355a", "i356o", "i357o", "i358a", "i360a", "i360b", "i360c", "i361o",
		"i362o", "i363o", "i364o", "i365o", "i367a", "i367b", "i367c", "i368o",
		"i369o", "i370o", "i371o", "i372o", "i373o", "i376o", "i377o", "i382a",
		"i383o", "i384o", "i385a", "i387o", "i388o", "i389o", "i391o", "i393o",
		"i394a", "i394b", "i395o", "i396o", "i397o", "i398o", "i399o", "i400o",
		"i401o", "i402o", "i403o", "i404o", "i405o", "i406o", "i408o", "i411o",
		"i412o", "i413o", "i414a", "i415a", "i418o", "i419o", "i420o", "i421o",
		"i422o", "i423o", "i424o", "i425o", "i426o", "i427o", "i428o", "i450o",
		"i451o", "i452o", "i453o", "i454o", "i455o", "i456o", "i457o", "i458o",
		"i459o")

	iv5Out <- matrix(".", nrow=nrow(odk), ncol=353)

        ## function for creating simple Y/N indicators
	qYesNo <- c(20:40, 42, 44:51, 54:74, 78, 81:82, 85:89, 92, 95, 98:100,
		102:103, 106, 110:122, 127, 131, 134:135, 138, 140:143, 145:148,
		151:160, 162, 169:180, 182, 184:192, 200, 204:205, 207:213, 215:223,
		225:238, 240:251, 254:265, 267:270, 274:280, 282, 288:292, 296:303,
		305:306, 308:312, 315:330, 333:353)

	tmpMat <- matrix(sapply(whoNames[qYesNo], stri_detect_fixed, str=odkNames), nrow=length(odkNames))
	indexData <- apply(tmpMat, 2, which)
        iv5Out[ , qYesNo] <- as.matrix(odk[ , indexData])
        iv5Out[iv5Out=="yes"] <- "Y"
        iv5Out[iv5Out=="no"] <- "N"
        iv5Out[iv5Out=="dk"] <- "."
        iv5Out[iv5Out=="ref"] <- "."
        iv5Out[iv5Out==""] <- "."
        iv5Out[is.na(iv5Out)] <- "."

	# Step through iv5 indicators to create new values
	#1) Did s(he) die during the wet season? d wet & 2) Did s(he) die during the dry season? d dry
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[1]))
	## table(odk[ , indexData])
	iv5Out[tolower(odk[ , indexData])=="wet" | tolower(odk[ , indexData])=="wet season", 1] <- "Y" ## wet season
	iv5Out[tolower(odk[ , indexData])=="dry" | tolower(odk[ , indexData])=="dry season", 1] <- "N"

	iv5Out[tolower(odk[ , indexData])=="wet" | tolower(odk[ , indexData])=="wet season", 2] <- "N" ## dry season
	iv5Out[tolower(odk[ , indexData])=="dry" | tolower(odk[ , indexData])=="dry season", 2] <- "Y"

	#3) Was he male? male & ##4) Was he female? female
	indexData_sex <- which(stri_endswith_fixed(odkNames, whoNames[3]))
	## table(odk[ , indexData_sex])
	iv5Out[tolower(odk[ , indexData_sex])=="male",   3] <- "Y" ## male
	iv5Out[tolower(odk[ , indexData_sex])=="female", 3] <- "N"

	iv5Out[tolower(odk[ , indexData_sex])=="male",   4] <- "N" ## female
	iv5Out[tolower(odk[ , indexData_sex])=="female", 4] <- "Y"

	# age
	indexData1y <- which(stri_detect_regex(odkNames, "ageinyears$"))   ## exclude ageInXXXXRemain
	indexData1m <- which(stri_detect_regex(odkNames, "ageinmonths$"))
	indexData1d <- which(stri_detect_regex(odkNames, "ageindays$"))

	indexData2 <- which(stri_detect_regex(odkNames, "age_group"))

	indexData3 <- which(stri_detect_regex(odkNames, "age_adult"))

	indexData4  <- which(stri_detect_regex(odkNames, "age_child_unit"))
	indexData4d <- which(stri_detect_regex(odkNames, "age_child_days"))
	indexData4m <- which(stri_detect_regex(odkNames, "age_child_months"))
	indexData4y <- which(stri_detect_regex(odkNames, "age_child_years"))

	indexData5d <- which(stri_detect_regex(odkNames, "age_neonate_days"))
	indexData5h <- which(stri_detect_regex(odkNames, "age_neonate_hours"))
	indexData5m <- which(stri_detect_regex(odkNames, "age_neonate_minutes"))

	#5) Was s(he) aged 65 years or more at death? 65+
	iv5Out[odk[ , indexData1y]>=65, 5] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="adult" & odk[ , indexData3]>=65, 5] <- "Y"

	#6) Was s(he) aged 50 to 64 years at death? 50 to 64
	iv5Out[odk[ , indexData1y]< 65 & odk[ , indexData1y]>=50, 6] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ ,indexData2]=="adult" & odk[ ,indexData3]< 65 & odk[ ,indexData3]>=50, 6] <- "Y"

	#7) Was s(he) aged 15 to 49 years at death? 15-49
	iv5Out[odk[ , indexData1y]< 50 & odk[ , indexData1y]>=15, 7] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="adult" & odk[ , indexData3]< 50 & odk[ , indexData3]>=15, 7] <- "Y"

	#8) Was s(he) aged 5-14 years at death? 5-14 (adult or child)
	iv5Out[odk[ , indexData1y]< 15 & odk[ , indexData1y]>= 5, 8] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="adult" & odk[ , indexData3]< 15 & odk[ , indexData3]>=5, 8] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" & odk[ , indexData4]=="days" & odk[ , indexData4d]< 15*365.25 & odk[ , indexData4d]>=5*365.25, 8] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" & odk[ , indexData4]=="months" & odk[ , indexData4m]< 15*12 & odk[ , indexData4m]>=5*12, 8] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" & odk[ , indexData4]=="years" & odk[ , indexData4y]< 15 & odk[ , indexData4y]>=5, 8] <- "Y"

	#9) Was s(he) aged 1 to 4 years at death? 1 to 4 (child)
	iv5Out[odk[ , indexData1y]<  5 & odk[ , indexData1y]>=1, 9] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" & odk[ , indexData4]=="days" & odk[ , indexData4d]< 5*365.25 & odk[ , indexData4d]>=1*365.25, 9] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" & odk[ , indexData4]=="months" & odk[ , indexData4m]< 5*12 & odk[ , indexData4m]>=1*12, 9] <- "Y"
	iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" & odk[ , indexData4]=="years" & odk[ , indexData4y]< 5 & odk[ , indexData4y]>=1, 9] <- "Y"

	#10) Was s(he) aged 1 to 11 months at death? 1-11 months (child or neonate?)
	iv5Out[odk[ , indexData1d]< 365.25 & odk[ , indexData1d]>=30.4, 10] <- "Y"
	iv5Out[is.na(odk[ , indexData1d]) & odk[ , indexData2]=="child" & odk[ , indexData4]=="days" & odk[ , indexData4d]< 365.25 & odk[ , indexData4d]>=29, 10] <- "Y"
	iv5Out[is.na(odk[ , indexData1m]) & odk[ , indexData2]=="child" & odk[ , indexData4]=="months" & odk[ , indexData4m]< 12 & odk[ , indexData4m]>=1, 10] <- "Y"

	#11) Was s(he) aged < 1 month (28 days) at death? 0 - 27 days (neonate)
	iv5Out[odk[ , indexData1d]< 28, 11] <- "Y"
	iv5Out[is.na(odk[ , indexData1d]) & odk[ , indexData2]=="neonate", 11] <- "Y"

	#12) Was s(he) a live baby who died within 24 hours of birth? day0 iv5Names[12]
	iv5Out[odk[ , indexData1d]< 1, 12] <- "Y"
	ageDays    <- odk[ , indexData5d]
	ageHours   <- odk[ , indexData5h]/24;      ageHours[is.na(ageHours)]     <- 0
	ageMinutes <- odk[ , indexData5m]/(24*12); ageMinutes[is.na(ageMinutes)] <- 0
	ageNeonate <- ageDays + ageHours + ageMinutes
	iv5Out[odk[ , indexData2]=="neonate" & !is.na(odk[ , indexData5d]) & ageNeonate< 1    , 12] <- "Y"

	#13) Was s(he) a baby who died between 24 and 48 hours of birth? day1 iv5Names[13]
	iv5Out[odk[ , indexData2]=="neonate" & !is.na(odk[ , indexData5d]) & ageNeonate< 2 & ageNeonate>=1, 13] <- "Y"

	#14)  Was s(he) a baby who died more than 48 hours from birth, but within the first week? day2-6 iv5Names[14]
	iv5Out[odk[ , indexData2]=="neonate" & !is.na(odk[ , indexData5d]) & ageNeonate< 7 & ageNeonate>=2, 14] <- "Y"

	#15) Was s(he) a baby who died after the first week, but within the first month? wk2-4 iv5Names[15]
	iv5Out[odk[ , indexData2]=="neonate" & !is.na(odk[ , indexData5d]) & ageNeonate< 28 & ageNeonate>=7, 15] <- "Y"

        # Finish coding age (5-15) -- if only only one age has "Y", recode all others to "N"
        ## e.g., if age 65 == "Y", then age 50-64 == "N" and age 15-49 == "N" etc.
	indexData6 <-iv5Out[ , 5:15] != "Y"            ## identify elements in age columns that do not equal "Y"
        indexData7 <- rowSums(iv5Out[ , 5:15] == "Y")  ## identify with rows/records only have 1 "Y" for all age columns
        ## Now recode all "N"
        iv5Out[indexData7 == 1, 5:15][ indexData6[indexData7 == 1, ] ] <- "N"

	#16) Was she a woman aged 12-19 years at death? f-19
	iv5Out[ , 16] <- ifelse(odk[ , indexData_sex]=="female" & odk[ , indexData1y]< 19 & odk[ , indexData1y]>= 12, "Y", ".")
	iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) & odk[ , indexData2]=="adult" & odk[ , indexData3]< 19 & odk[ , indexData3]>=12, 16] <- "Y"

	#17) Was she a woman aged 20-34 years at death? f20-34
	iv5Out[ , 17] <- ifelse(odk[ , indexData_sex]=="female" & odk[ , indexData1y]< 35 & odk[ , indexData1y]>= 20, "Y", ".")
	iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) & odk[ , indexData2]=="adult" & odk[ , indexData3]< 35 & odk[ , indexData3]>=20, 17] <- "Y"

	#18) Was she a woman aged 35 to 49 years at death? f35-49
	iv5Out[ , 18] <- ifelse(odk[ , indexData_sex]=="female" & odk[ , indexData1y]< 50 & odk[ , indexData1y]>= 35, "Y", ".")
	iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) & odk[ , indexData2]=="adult" & odk[ , indexData3]< 50 & odk[ , indexData3]>=35, 18] <- "Y"

	#19) Was she married at the time of death? married
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[19]))
	## table(odk[ , indexData])
	iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="married", 19] <- "Y"
	iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="single", 19] <- "N"
	iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="partner", 19] <- "N"
	iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="divorced", 19] <- "N"
	iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="widowed", 19] <- "N"
	iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="too_young_to_be_married", 19] <- "N"
	iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="child", 19] <- "N"
	iv5Out[odk[ , indexData_sex]=="male", 19] <- "N"

	#41) Was it more than 5 minutes after birth before the baby first cried? cry 5+m
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[41]))
	iv5Out[odk[ , indexData]> 5, 41] <- "Y"
	iv5Out[odk[ , indexData]<=5, 41] <- "N"

	#43) Did the baby stop crying more than a day before (s)he died? cry st 1+d
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[43]))
	iv5Out[odk[ , indexData]> 24, 43] <- "Y"
	iv5Out[odk[ , indexData]<=24, 43] <- "N"

	#52) Did the final illness last less than 3 weeks? ill <3w. 
	indexDatad <- which(stri_detect_regex(odkNames, "id10120$"))
	indexDataw <- which(stri_endswith_fixed(odkNames, "id10122")) ## should this be "years"?
	indexDatam <- which(stri_endswith_fixed(odkNames, "id10121"))
	iv5Out[odk[ , indexDatad]< 21, 52] <- "Y"
	iv5Out[odk[ , indexDatad]>=21, 52] <- "N"

	iv5Out[odk[ , indexDataw]< 3, 52] <- "Y"
	iv5Out[odk[ , indexDataw]>=3, 52] <- "N"

	#iv5Out[odk[ , indexDatam]< 1, 52] <- "Y"
	iv5Out[odk[ , indexDatam]>=1, 52] <- "N"

	#53) Did the final illness last at least 3 weeks? ill 3+w
	iv5Out[odk[ , indexDatad]>=21, 53] <- "Y"
	iv5Out[odk[ , indexDatad]< 21, 53] <- "N"

	iv5Out[odk[ , indexDataw]>= 3, 53] <- "Y"
	iv5Out[odk[ , indexDataw]<  3, 53] <- "N"

	iv5Out[odk[ , indexDatam]>= 1, 53] <- "Y"
	#iv5Out[odk[ , indexDatam]<  1, 53] <- "Y"

	#75) Did the fever last less than a week before death? fev <1w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[75]))
	iv5Out[odk[ , indexData]< 7, 75] <- "Y"
	iv5Out[odk[ , indexData]>=7, 75] <- "N"

	#76) Did the fever last at least one week, but less than 2 weeks before death? fev 1-2w
	iv5Out[odk[ , indexData]>= 7 & odk[ , indexData]< 14, 76] <- "Y"
	iv5Out[odk[ , indexData]<  7,                         76] <- "N"

	#77) Did the fever last at least 2 weeks before death? fev 2+w
	iv5Out[odk[ , indexData]>= 14, 77] <- "Y"
	iv5Out[odk[ , indexData]<  14, 77] <- "N"

	#79) Was the fever severe? fev sev
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[79]))
	iv5Out[tolower(odk[ , indexData])=="severe",   79] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="mild",     79] <- "N"
	iv5Out[tolower(odk[ , indexData])=="moderate", 79] <- "N"

	#80) Was the fever continuous? fev cont
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[80]))
	iv5Out[tolower(odk[ , indexData])=="continuous", 80] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="nightly",    80] <- "N"
	iv5Out[tolower(odk[ , indexData])=="on_and_off", 80] <- "N"

	#83) Did the cough last less than 3 weeks before death? cou <3w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[83]))
	iv5Out[odk[ , indexData[1]]< 21, 83] <- "Y"
	iv5Out[odk[ , indexData[1]]>=21, 83] <- "N"

	#84) Did the cough last at least 3 weeks before death? cou 3+w
	iv5Out[odk[ , indexData[1]]>=21, 84] <- "Y"
	iv5Out[odk[ , indexData[1]]< 21, 84] <- "N"

	#90) Did the difficult breathing last for at least 3 days before death? dif br 3d
	indexDatad <- which(stri_endswith_fixed(odkNames, whoNames[90]))
	indexDatam <- which(stri_endswith_fixed(odkNames, "id10162"))
	indexDatay <- which(stri_endswith_fixed(odkNames, "id10163"))

	## indexDatad; odkNames[indexDatad]. # 
	iv5Out[odk[ , indexDatad]>=3, 90] <- "Y"
	iv5Out[odk[ , indexDatad]< 3, 90] <- "N"

	iv5Out[odk[ , indexDatam]>=1, 90] <- "Y"
	iv5Out[odk[ , indexDatay]>=1, 90] <- "Y"

	#91) Was the difficult breathing continuous during this period? dif br con
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[91]))
	iv5Out[tolower(odk[ , indexData])=="continuous", 91] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="on_and_off", 91] <- "N"

	#93) Did the fast breathing last for less than two weeks before death? br fs <2w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[93]))
	iv5Out[odk[ , indexData[1]]< 14, 93] <- "Y"
	iv5Out[odk[ , indexData[1]]>=14, 93] <- "N"

	#94) Did the fast breathing last for at least 2 weeks before death? br fs 2+w
	iv5Out[odk[ , indexData[1]]>=14, 94] <- "Y"
	iv5Out[odk[ , indexData[1]]< 14, 94] <- "N"

	#96) Did the breathlessness last for less than 2 weeks before death? brl <2w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[96]))
	iv5Out[odk[ , indexData[1]]< 14, 96] <- "Y"
	iv5Out[odk[ , indexData[1]]>=14, 96] <- "N"

	#97) Did the breathlessness last for at least 2 weeks before death? brl 2+w
	iv5Out[odk[ , indexData[1]]>=14, 97] <- "Y"
	iv5Out[odk[ , indexData[1]]< 14, 97] <- "N"

	#101) Did his/her breathing sound like wheezing or grunting? whz grun
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[101]))

	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "grunting"), 101] <- "Y"
	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "stridor"), 101] <- "Y"
	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "wheezing"), 101] <- "Y"

	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "no"), 101] <- "N"

	#104) Did (s)he experience chest pain at least 3 days before death? chp 3d
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[104]))
	iv5Out[odk[ , indexData]>=3, 104] <- "Y"
	iv5Out[odk[ , indexData]< 3, 104] <- "N"

	#105) Did the chest pain last for at least 30 minutes? chp 30m
	indexDatam <- which(stri_endswith_fixed(odkNames, whoNames[105]))
	indexDatah <- which(stri_endswith_fixed(odkNames, "id10179"))
	iv5Out[odk[ , indexDatam]>=30, 105] <- "Y"
	iv5Out[odk[ , indexDatah]>=.5, 105] <- "Y"

	iv5Out[odk[ , indexDatam]< 30, 105] <- "N"
	iv5Out[odk[ , indexDatah]< .5, 105] <- "N"

	#107) Did (s)he have diarrhoea for less than 2 weeks before death? drr <2w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[107]))
	iv5Out[odk[ , indexData]< 14, 107] <- "Y"
	iv5Out[odk[ , indexData]>=14, 107] <- "N"

	#108) Did (s)he have diarrhoea for at least 2 weeks but less than 4 weeks before death? drr 2-4w
	iv5Out[odk[ , indexData]< 28 & odk[ , indexData]>=14, 108] <- "Y"
	iv5Out[odk[ , indexData]< 14,                         108] <- "N"
	iv5Out[odk[ , indexData]>=28,                         108] <- "N"

	#109) Did (s)he have diarrhoea for at least 4 weeks before death? drr 4+w
	iv5Out[odk[ , indexData]>=28, 109] <- "Y"
	iv5Out[odk[ , indexData]< 28, 109] <- "N"

	#123) Did (s)he have severe abdominal pain for less than 2 weeks before death? abd p <2w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[121]))
	indexDatad <- which(stri_detect_regex(odkNames, "id10197$"))
	indexDatah <- which(stri_endswith_fixed(odkNames, "id10196"))
	indexDataw <- which(stri_endswith_fixed(odkNames, "id10197b"))
	indexDatam <- which(stri_endswith_fixed(odkNames, "id10198"))

	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14, 123] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14, 123] <- "N"

	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatah]< (24*14), 123] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatah]>=(24*14), 123] <- "N"

	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDataw]< 2, 123] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDataw]>=2, 123] <- "N"

	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatam]< 1, 123] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="no", 123] <- "N"

	#124) Did (s)he have severe abdominal pain for at least 2 weeks before death? abd p 2+w
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14, 124] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14, 124] <- "N"

	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatah]>=(24*14), 124] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatah]< (24*14), 124] <- "N"

	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDataw]>=2, 124] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDataw]< 2, 124] <- "N"

	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatam]>=1, 124] <- "Y"

	iv5Out[tolower(odk[ , indexData])=="no", 124] <- "N"

	#125) Was the pain in the upper abdomen? abd p up
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[125]))
	iv5Out[tolower(odk[ , indexData])=="upper_abdomen",       125] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="upper_lower_abdomen", 125] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="lower_abdomen",       125] <- "N"


	#126) Was the pain in the lower abdomen? abd p lo
	iv5Out[tolower(odk[ , indexData])=="upper_abdomen",       126] <- "N"
	iv5Out[tolower(odk[ , indexData])=="upper_lower_abdomen", 126] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="lower_abdomen",       126] <- "Y"

	#128) Did (s)he have a more than usually protruding abdomen for less than 2 weeks before death? abd pr <2w
	indexData  <- which(stri_endswith_fixed(odkNames, "id10200"))
	indexDatad <- which(stri_endswith_fixed(odkNames, whoNames[128]))
	indexDatam <- which(stri_endswith_fixed(odkNames, "id10202"))

	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14 & odk[ , indexDatam]==0,     128] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14 & is.na(odk[ , indexDatam]), 128] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14 & odk[ , indexDatam]==0,     128] <- "N"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14 & is.na(odk[ , indexDatam]), 128] <- "N"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatam]>=1,                              128] <- "N"
	iv5Out[tolower(odk[ , indexData])=="no",                                                       128] <- "N"

	#129) Did (s)he have a more than usually protruding abdomen for at least 2 weeks before death? abd pr 2+w
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatam]>=1,                              129] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14 & odk[ , indexDatam]==0,     129] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14 & is.na(odk[ , indexDatam]), 129] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14 & odk[ , indexDatam]==0,     129] <- "N"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14 & is.na(odk[ , indexDatam]), 129] <- "N"
	iv5Out[tolower(odk[ , indexData])=="no",                                                       129] <- "N"

	#130) Did (s)he develop the protruding abdomen rapidly? abd pr rap
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[130]))
	iv5Out[tolower(odk[ , indexData])=="rapidly", 130] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="slowly",  130] <- "N"

	#132) Did (s)he have a mass in the abdomen for less than 2 weeks before death? ab ms <2w
	indexData  <- which(stri_endswith_fixed(odkNames, "id10204"))
	indexDatad <- which(stri_endswith_fixed(odkNames, whoNames[132]))
	indexDatam <- which(stri_endswith_fixed(odkNames, "id10206"))

	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14 & odk[ , indexDatam]==0,     132] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14 & is.na(odk[ , indexDatam]), 132] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14 & odk[ , indexDatam]==0,     132] <- "N"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14 & is.na(odk[ , indexDatam]), 132] <- "N"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatam]>=1,                              132] <- "N"
	iv5Out[tolower(odk[ , indexData])=="no",                                                       132] <- "N"

	#133) Did (s)he have a mass in the abdomen for at least 2 weeks before death? ab ms 2+w
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14 & odk[ , indexDatam]==0,     133] <- "N"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 14 & is.na(odk[ , indexDatam]), 133] <- "N"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14 & odk[ , indexDatam]==0,     133] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=14 & is.na(odk[ , indexDatam]), 133] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatam]>=1,                              133] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="no",                                                       133] <- "N"

	#136) Did (s)he have a stiff neck for less than one week before death? st n <1w
	indexDatad <- which(stri_endswith_fixed(odkNames, whoNames[136]))
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 7, 136] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=7, 136] <- "N"
	iv5Out[tolower(odk[ , indexData])=="no",                          136] <- "N"

	#137) Did (s)he have a stiff neck for at least one week before death? st n 1+w
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=7, 137] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 7, 137] <- "N"
	iv5Out[tolower(odk[ , indexData])=="no",                          137] <- "N"

	#139) Did (s)he have a painful neck for at least one week before death? pa n 1+w
	indexDatad <- which(stri_endswith_fixed(odkNames, whoNames[139]))
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]>=7, 139] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="yes" & odk[ , indexDatad]< 7, 139] <- "N"
	iv5Out[tolower(odk[ , indexData])=="no",                          139] <- "N"

	#144) Was (s)he unsconscious for at least 6 hours before death?	unc 6+h
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[144]))
	iv5Out[odk[ , indexData]>=6, 144] <- "Y"
	iv5Out[odk[ , indexData]< 6, 144] <- "N"

	#149) Did the convulsions last for less than 10 minutes?	conv <10m
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[149]))
	iv5Out[odk[ , indexData]< 10, 149] <- "Y"
	iv5Out[odk[ , indexData]>=10, 149] <- "N"

	#150) Did the convulsions last for at least 10 minutes?	conv 10+m
	iv5Out[odk[ , indexData]< 10, 150] <- "N"
	iv5Out[odk[ , indexData]>=10, 150] <- "Y"

	#161) Did the ulcer ooze pus for at least 2 weeks?	sk ul 2+w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[161]))
	iv5Out[odk[ , indexData]>=14, 161] <- "Y"
	iv5Out[odk[ , indexData]< 14, 161] <- "N"

	#163) Did (s)he have the skin rash for less than one week?	sk ra <1w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[163]))
	iv5Out[odk[ , indexData]< 7, 163] <- "Y"
	iv5Out[odk[ , indexData]>=7, 163] <- "N"

	#164) Did (s)he have the skin rash for at least one week?	sk ra 1+w
	iv5Out[odk[ , indexData]>=7, 164] <- "Y"
	iv5Out[odk[ , indexData]< 7, 164] <- "N"

	#165) Did (s)he have a rash on the face?	sk ra face
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[165]))
	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "face"),                165] <- "Y"
	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), negate = TRUE, "face"), 165] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",                                       165] <- "."

	#166) Did (s)he have a rash on the trunk or abdomen?	sk ra abd
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[166]))
	iv5Out[stri_detect_regex(tolower(odk[ , indexData]), "abdomen|trunk"),                166] <- "Y"
	iv5Out[stri_detect_regex(tolower(odk[ , indexData]), negate = TRUE, "abdomen|trunk"), 166] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",                                                166] <- "."

	#167) Did (s)he have a rash on the extremities?	sk ra ext
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[167]))
	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "extremities"),                167] <- "Y"
	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), negate = TRUE, "extremities"), 167] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",                                              167] <- "."

	#168) Did (s)he have a rash everywhere?	sk ra all
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[168]))
	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "everywhere"),                168] <- "Y"
	iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), negate = TRUE, "everywhere"), 168] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",                                             168] <- "."

	#181) Did (s)he have puffiness of the face for at least one week before death?	sw p f 1+w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[181]))
	iv5Out[odk[ , indexData]>=7, 181] <- "Y"
	iv5Out[odk[ , indexData]< 7, 181] <- "N"

	#183) Did the swelling last for at least 3 days before death?	sw lf 3+d
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[183]))
	iv5Out[odk[ , indexData]>=3, 183] <- "Y"
	iv5Out[odk[ , indexData]< 3, 183] <- "N"

	#193) Was only the right side of the body paralysed?	par rs
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[193]))
	iv5Out[tolower(odk[ , indexData])=="right_side", 193] <- "Y"
	iv5Out[tolower(odk[ , indexData])!="right_side", 193] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",           193] <- "."

	#194) Was only the left side of the body paralysed?	par ls
	iv5Out[tolower(odk[ , indexData])=="left_side", 194] <- "Y"
	iv5Out[tolower(odk[ , indexData])!="left_side", 194] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",          194] <- "."

	#195) Was only the lower part of the body paralysed?	par lo
	iv5Out[tolower(odk[ , indexData])=="lower_part_of_body", 195] <- "Y"
	iv5Out[tolower(odk[ , indexData])!="lower_part_of_body", 195] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",                   195] <- "."

	#196) Was only the upper part of the body paralysed?	par up
	iv5Out[tolower(odk[ , indexData])=="upper_part_of_body", 196] <- "Y"
	iv5Out[tolower(odk[ , indexData])!="upper_part_of_body", 196] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",                   196] <- "."

	#197) Was only one leg paralysed?	par leg
	iv5Out[tolower(odk[ , indexData])=="one_leg_only", 197] <- "Y"
	iv5Out[tolower(odk[ , indexData])!="one_leg_only", 197] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",             197] <- "."

	#198) Was only one arm paralysed?	par arm
	iv5Out[tolower(odk[ , indexData])=="one_arm_only", 198] <- "Y"
	iv5Out[tolower(odk[ , indexData])!="one_arm_only", 198] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",             198] <- "."

	#199) Was the entire body paralysed?	par all
	iv5Out[tolower(odk[ , indexData])=="whole_body", 199] <- "Y"
	iv5Out[tolower(odk[ , indexData])!="whole_body", 199] <- "N"
	iv5Out[tolower(odk[ , indexData])=="",           199] <- "."

	#201) Did (s)he have difficulty swallowing for at least one week before death?	swa 1+w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[201]))
	iv5Out[odk[ , indexData]>=7, 201] <- "Y"
	iv5Out[odk[ , indexData]< 7, 201] <- "N"

	#202) Did (s)he have difficulty with swallowing solids?	swa sol
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[202]))
	iv5Out[tolower(odk[ , indexData])=="solids",  202] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="both",    202] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="liquids", 202] <- "N"

	#203) Did (s)he have difficulty with swallowing liquids?	swa liq
	iv5Out[tolower(odk[ , indexData])=="solids",  203] <- "N"
	iv5Out[tolower(odk[ , indexData])=="both",    203] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="liquids", 203] <- "Y"

	#206) Did (s)he have the yellow discolouration for at least 3 weeks before death?	yell 3+w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[206]))
	iv5Out[odk[ , indexData]>=21, 206] <- "Y"
	iv5Out[odk[ , indexData]< 21, 206] <- "N"

	#214) Did the baby stop suckling on the 2nd day of life or later?	suck st d1
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[214]))
	iv5Out[odk[ , indexData[1]]>=2, 214] <-  "Y"
	iv5Out[odk[ , indexData[1]]< 2, 214] <-  "N"

	#224) Was the baby more than 3 days old when it started feeling cold to touch?	cold 3+d
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[224]))
	iv5Out[odk[ , indexData]> 3, 224] <- "Y"
	iv5Out[odk[ , indexData]<=3, 224] <- "N"

	#239) Had her period been overdue for at least 4 weeks?	men l 4+w
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[239]))
	iv5Out[odk[ , indexData]>=4, 239] <- "Y"
	iv5Out[odk[ , indexData]< 4, 239] <- "N"

	#252) Did she die during or after her first pregnancy?	1st pr. 
	#     WHO question is "How many births, including stillbirths, did she/the mother have before this baby?"
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[252]))
	iv5Out[odk[ , indexData]==0, 252] <-  "Y"
	iv5Out[odk[ , indexData]> 0, 252] <-  "N"

	#253) Did she have four or more pregnancies before this one?	4+ pr
	iv5Out[odk[ , indexData]>=4, 253] <- "Y"
	iv5Out[odk[ , indexData]< 4, 253] <- "N"

	#266) Did her labour last longer than 24 hours?	lab 24+h
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[266]))
	iv5Out[odk[ , indexData]> 24, 266] <- "Y"
	iv5Out[odk[ , indexData]<=24, 266] <- "N"

	#271) Did the mother deliver at a health facility or clinic?	del hfac
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[271]))
	iv5Out[tolower(odk[ , indexData])=="hospital",                         271] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="other_health_facility",            271] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="home",                             271] <- "N"
	iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 271] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other",                            271] <- "N"

	#272) Did the mother deliver at home?	del home
	iv5Out[tolower(odk[ , indexData])=="hospital",                         272] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other_health_facility",            272] <- "N"
	iv5Out[tolower(odk[ , indexData])=="home",                             272] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 272] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other",                            272] <- "N"

	#273) Did the mother deliver elsewhere (not at a health facility nor at home)?	del else
	iv5Out[tolower(odk[ , indexData])=="hospital",                         273] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other_health_facility",            273] <- "N"
	iv5Out[tolower(odk[ , indexData])=="home",                             273] <- "N"
	iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 273] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="other",                            273] <- "Y"

	#281) If the child was part of a multiple birth, was it born first?	mult fir
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[281]))
	iv5Out[tolower(odk[ , indexData])=="first",           281] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="second_or_later", 281] <- "N"

	#283) Did the child's mother die during or shortly after the delivery?	moth d del
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[283]))
	iv5Out[tolower(odk[ , indexData])=="after_delivery",   283] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="during_delivery" , 283] <- "Y"

	#284) Did the child's mother die in the baby's first year of life?	moth d y1
	indexDatam <- which(stri_endswith_fixed(odkNames, whoNames[284]))
	indexDatad <- which(stri_endswith_fixed(odkNames, "id10359"))
	nMonths <- odk[ , indexDatam]
	nDays   <- odk[ , indexDatad]
	nMonths[is.na(nMonths) & !is.na(nDays)] <- 0
	nDays[is.na(nDays) & !is.na(nMonths)] <- 0
	iv5Out[nMonths + nDays/30.4 <=12, 284] <- "Y"
	iv5Out[nMonths + nDays/30.4 > 12, 284] <- "N"

	#285) Was the baby born in a health facility or clinic?	born fac
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[285]))
	iv5Out[tolower(odk[ , indexData])=="hospital",                         285] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="other_health_facility",            285] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="home",                             285] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other",                            285] <- "N"
	iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 285] <- "N"

	#286) Was the baby born at home?	born home
	iv5Out[tolower(odk[ , indexData])=="hospital",                         286] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other_health_facility",            286] <- "N"
	iv5Out[tolower(odk[ , indexData])=="home",                             286] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="other",                            286] <- "N"
	iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 286] <- "N"

	#287) Was the baby born somewhere else (e.g. on the way to a clinic)?	born on way
	iv5Out[tolower(odk[ , indexData])=="hospital",                         287] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other_health_facility",            287] <- "N"
	iv5Out[tolower(odk[ , indexData])=="home",                             287] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other",                            287] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 287] <- "Y"

	#293) Was the baby born during the ninth month (at least 37 weeks) of pregnancy?	gest 9m
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[293]))
	iv5Out[odk[ , indexData]>=9 & odk[ , indexData]< 88, 293] <- "Y"
	iv5Out[odk[ , indexData]< 9,                         293] <- "N"

	#294) Was the baby born during the eighth month (34 to 37 weeks) of pregnancy?	gest 8m
	iv5Out[odk[ , indexData]> 8 & odk[ , indexData]< 88, 294] <- "N"
	iv5Out[odk[ , indexData]==8,                         294] <- "Y"
	iv5Out[odk[ , indexData]< 8,                         294] <- "N"

	#295) Was the baby born before the eighth month (less than 34 weeks) of pregnancy?	gest 7m
	iv5Out[odk[ , indexData]>=8 & odk[ , indexData]< 88, 295] <- "N"
	iv5Out[odk[ , indexData]< 8                         , 295] <- "Y"

	#304) Did labour and delivery take more than 24 hours?	lab 24+h
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[304]))
	iv5Out[odk[ , indexData]> 24, 304] <- "Y"
	iv5Out[odk[ , indexData]<=24, 304] <- "N"

	#307) Was the liquor a green or brown colour when the waters broke?	liq gr-br
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[307]))
	iv5Out[tolower(odk[ , indexData])=="green_or_brown", 307] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="clear",          307] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other",          307] <- "N"

	#313) Was this baby born from the mother's first pregnancy?	born 1st pr
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[313]))
	iv5Out[odk[ , indexData]==0, 313] <- "Y"
	iv5Out[odk[ , indexData]> 0, 313] <- "N"

	#314) Did the baby's mother have four or more births before this one?	born 4+ pr
	iv5Out[odk[ , indexData]>=4, 314] <- "Y"
	iv5Out[odk[ , indexData]< 4, 314] <- "N"

	#331) Did (s)he use non-smoking tobacco?	tobac ns
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[331]))
	iv5Out[tolower(odk[ , indexData])=="chewing_tobacco",       331] <- "Y"
	iv5Out[tolower(odk[ , indexData])=="cigarettes",            331] <- "N"
	iv5Out[tolower(odk[ , indexData])=="pipe",                  331] <- "N"
	iv5Out[tolower(odk[ , indexData])=="local_form_of_tobacco", 331] <- "N"
	iv5Out[tolower(odk[ , indexData])=="other",                 331] <- "N"

	#332) Did (s)he smoke at least 10 cigarettes daily?	cigs >10
	indexData <- which(stri_endswith_fixed(odkNames, whoNames[332]))
	iv5Out[odk[ , indexData]>=10, 332] <- "Y"
	iv5Out[odk[ , indexData]< 10, 332] <- "N"

	# Add IDD as first column
	iv5Out <- cbind(as.character(odk$meta.instanceID), iv5Out)

	# Attach column names
	colnames(iv5Out) <- c("ID", iv5Names)

	return(as.data.frame(iv5Out))
}

