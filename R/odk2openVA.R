#' Map VA records to InterVA5 & InSilico (with option data.type = "WHO2016").
#'
#' \code{odk2openVA} transforms data collected with the 2016 WHO VA instrument
#'   into a format that can be used with the InterVA5 and InSilicoVA alogrithms
#'   for coding cause of death. It is a wrapper for functions that handle
#'   specific versions of the 2016 WHO VA instrument --  namely, 1.4.1 and 1.5.1.
#'
#' @param odk A dataframe, obtained from reading an ODK Briefcase
#'   export of records collected with the WHO questionnaire.
#'
#' @param version A string equal to "1.5.1" (default) or "1.4.1" which
#'     indicates the version of the 2016 WHO VA instrument used to collect the
#'     data.
#'
#' @examples
#' ## Example with 2016 WHO VA instrument version 1.5.1
#' record_f_name151 <- system.file("sample", "who151_odk_export.csv", package = "CrossVA")
#' records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)
#' output151 <- odk2openVA(records151, version = "1.5.1")
#'
#' ## Example with 2016 WHO VA instrument version 1.4.1
#' record_f_name141 <- system.file("sample", "who141_odk_export.csv", package = "CrossVA")
#' records141 <- read.csv(record_f_name141, stringsAsFactors = FALSE)
#' output141 <- odk2openVA(records, version = "1.4.1")
#'
#' @export
#'
odk2openVA <- function(odk, version = c("1.5.1", "1.4.1")[1]){

    if (!(version %in% c("1.5.1", "1.4.1"))) {
        cat(
            paste("Error: The option 'version' must be '1.5.1' or '1.4.1' \n",
                  sep = "")
        )
    }

    if (version == "1.5.1") return( odk2openVA_v151(odk) )
    if (version == "1.4.1") return( odk2openVA_v141(odk) )


}
