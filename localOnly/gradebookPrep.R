library(openxlsx)
library(dplyr)
library(openssl)

# Encryption Information ---
passphrase <- "setThisToSomething"
key <- sha256(charToRaw(passphrase))

# Process Attendance Information ----
## Load the Attendance Record
rawAttendance <- openxlsx::readWorkbook(xlsxFile = "localOnly/attendanceFake.xlsx")

## Calculate Attendance proportion
rawAttendance <- rawAttendance %>%
  dplyr::mutate(
    atdProp = (1 * present + 0.8 * tardy + 0 * absent) / (mtgCount)
  )

## Encrypt and Save Attendance ----
encryptedAttendance <- aes_cbc_encrypt(
  serialize(rawAttendance, NULL),
  key = key
)

saveRDS(encryptedAttendance, "dataFiles/encryptedAttendance.rds")

remove(list = c("rawAttendance", "encryptedAttendance"))

# Process Gradebook ----
## Load the gradebook
rawGradeBook <- openxlsx::loadWorkbook(file = "localOnly/gradebookFake.xlsx")

## Encrypt and Save Gradebook ----
encryptedGrades <- aes_cbc_encrypt(serialize(rawGradeBook, NULL), key = key)

saveRDS(encryptedGrades, "dataFiles/encryptedGrades.rds")

remove(list = c("rawGradeBook", "encryptedGrades"))

# Clean up ----
remove(list = c("key","passphrase"))
