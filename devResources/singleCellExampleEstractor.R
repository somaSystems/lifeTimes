x <- sample(1000)
x <- as.data.frame(x)
usethis::use_data(x, mtcars, overwrite = TRUE)

str(x)

nrow

dim

.Primitive

dim <-function(x) {c(1, 1)}
dim(mtcars)
nrow(mtcars)

search()

old <- search()
old
testthat::expect_equal(1, 1)
setdiff(search(), old)

expect_true(TRUE)



getwd()
list.files()
toSubSet <- read.csv(file="./data/clusterData.csv")

subSetTs <- toSubSet;


treatLevels <- unique(toSubSet$Treatment)

list_TreatAndCell <- list()
for(treat in treatLevels){
  current_TreatAndCell <- data.frame()
  current_TreatAndCell <- c(Treatment = paste0(treat), cellNumber =head(toSubSet[toSubSet$Treatment == treat,],1)$cellNumber)
  list_TreatAndCell[[treat]] <- current_TreatAndCell
}
keyTreatAndCell <- do.call(rbind, list_TreatAndCell)
keyTreatAndCell <- as.data.frame(keyTreatAndCell)

str(keyTreatAndCell)

listOf_extractedCell <- list()


for(treatmentInstance in 1:length(list_TreatAndCell)){
  print(paste("index is..",treatmentInstance))
  print(paste("current instance is", list_TreatAndCell[[treatmentInstance]][1]))
  print(list_TreatAndCell[[treatmentInstance]][1])
  extractedCell <- subset(toSubSet, toSubSet$cellNumber == list_TreatAndCell[[treatmentInstance]][2])
  listOf_extractedCell[[treatmentInstance]] <- extractedCell
  }


extractedCells <- do.call(rbind, listOf_extractedCell)
extractedCells <- as.data.frame(extractedCells)

write.csv(extractedCells, file = "extractedCells.csv",row.names = FALSE)

# listOf_extractedCell
#
# list_TreatAndCell[]
# str(list_TreatAndCell)
# df_TreatAndCell
#
# head(toSubSet[toSubSet$Treatment == "DMSO",],1)$cellNumber
# head(toSubSet[toSubSet$Treatment == "DMSO",],1)$cellNumber
# head(toSubSet[toSubSet$Treatment == "DMSO",],1)$cellNumber
# head(toSubSet[toSubSet$Treatment == "DMSO",],1)$cellNumber
# head(toSubSet[toSubSet$Treatment == "DMSO",],1)$cellNumber
#

log2(8917416)
2^23
8388608
getwd()
