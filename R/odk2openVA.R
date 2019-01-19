#' Map VA records to InterVA5 & InSilico (with option data.type = "WHO2016").
#'
#' \code{odk2openVA} transforms data collected with the 2016 WHO VA instrument
#'   into a format that can be used with the InterVA5 and InSilicoVA alogrithms
#'   for coding cause of death. It is a wrapper for functions that handle
#'   specific versions of the 2016 WHO VA instrument --  namely, 1.4.1 and 1.5.1.
#'   If the input (odk) includes a column containing the string:
#'   "age_neonate_hours", then the function assumes the questionnaire version is
#'   1.4.1 (and assumes version 1.5.1 if the string is not located).
#'
#' @param odk A dataframe, obtained from reading an ODK Briefcase
#'   export of records collected with the WHO questionnaire.
#'
#' @examples
#' ## Example with 2016 WHO VA instrument version 1.5.1
#' record_f_name151 <- system.file("sample", "who151_odk_export.csv", package = "CrossVA")
#' records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)
#' output151 <- odk2openVA(records151)
#'
#' ## Example with 2016 WHO VA instrument version 1.4.1
#' record_f_name141 <- system.file("sample", "who141_odk_export.csv", package = "CrossVA")
#' records141 <- read.csv(record_f_name141, stringsAsFactors = FALSE)
#' output141 <- odk2openVA(records141)
#'
#' @export
#'
odk2openVA <- function(odk){

    hasAgeNeonateHours <- grep("age_neonate_hours", tolower(names(odk)))
    whoVersion <- ifelse(length(hasAgeNeonateHours) == 1, "1.4.1", "1.5.1")

    cat(paste("Assuming WHO questionnaire version is ", whoVersion, "\n", sep = ""))

    if (whoVersion == "1.5.1") return( odk2openVA_v151(odk) )
    if (whoVersion == "1.4.1") return( odk2openVA_v141(odk) )

}
