# Test that the odk2openVA returns a data frame of the appropriate size
# with the appropriate names.
context("Test return objects.")

test_that("odk2openVA returns a data frame", {

    record_f_name151 <- system.file("sample",
                                    "who151_odk_export.csv",
                                    package = "CrossVA")
    records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)
    output151 <- odk2openVA(records151, version = "1.5.1")

    expect_true(is.data.frame(output151))
})
