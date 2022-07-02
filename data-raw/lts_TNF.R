#TNF washout

lts_tnf <- read.csv(file = "data-raw/211021_TNF_washout_selected_tracks_FILTERED.csv")

colnames(lts_tnf)

library(ggplot2)


# hist(lts_tnf$Cell_Area, breaks = 100,)



ggplot(lts_tnf, aes(x = Time_min, y = lts_tnf$RELAratio, color = as.character(Position_TrackID), group = Position_TrackID))+
         geom_line()+
  xlim(0,200)

#subset so that max time is 500


#filter to remove tracks beyond 290

# ?subset()
#subset to reduced time
f_lts_tnf <- subset(lts_tnf,lts_tnf$Time_min <300)

# f_lts_tnf

###subset Data
library(dplyr)
TNF_matrix <- f_lts_tnf %>% select(Time_min,RELAratio,Position_TrackID, contains("Cell"), contains("Nucleus"))


TNF_matrix$sizeSplit <- ifelse(TNF_matrix$Cell_Area > mean(TNF_matrix$Cell_Area),"LargerCell","SmallerCell")

library(tidyr)
dim(TNF_matrix)

colnames(w_TNF_matrix)


l_TNF_matrix <- TNF_matrix %>%
  pivot_longer(cols = contains("Cell") | contains("Nucleus") | contains("ratio"),
               names_to = "measure")

colnames(l_TNF_matrix)

w_TNF_matrix <- pivot_wider(l_TNF_matrix,
                            id_cols = Time_min,
                            names_from = c(Position_TrackID, measure),
                            values_from = value)

w_TNF_matrix
# ?pivot_wider()

# w_TNF_matrix


#Impute missing values
library(imputeTS)

library(dplyr)
library(imputeTS)

wide_w_TNF_matrix_impute <- w_TNF_matrix %>%
  # filter()
  # select((where(is.numeric)))%>%
  # group_by(c_IDcellNumber_frame, geomFeature)%>%
  purrr::map_dfc(~na_ma(.x,k=1, maxgap =5))

wide_w_TNF_matrix_impute


rl_TNF <- pivot_longer(wide_w_TNF_matrix_impute,
                       cols = c(2:length(wide_w_TNF_matrix_impute)),
                       names_to = "posID_measureName",
                       values_to = "measureValue")


TNF_measure_name <- strsplit(sub('(^[^_]+_[^_]+)_(.*)$', '\\1 \\2', rl_TNF$posID_measureName), ' ')
head(TNF_measure_name)

table_TNF_measure_name <- do.call(rbind,TNF_measure_name)


# df_TNF_measure_name <- data.frame(TNF_measure_name)


rl_TNF$posID <- table_TNF_measure_name[,1]
rl_TNF$measureName <- table_TNF_measure_name[,2]

# w_rl_TNF$Cell_Area
w_rl_TNF <- pivot_wider(rl_TNF,
            id_cols = c(posID,Time_min),
            names_from = measureName,
            values_from = measureValue)


mw_rl_TNF <-w_rl_TNF %>%
  group_by(posID)%>%
  dplyr::mutate(avSize = mean(Cell_Area))


mw_rl_TNF$cellCategory <-  ifelse(mw_rl_TNF$avSize > median(mw_rl_TNF$avSize, na.rm = TRUE), "largerCell","smallerCell")
mw_rl_TNF$oneCategory <-  "aCell"

# w_rl_TNF$cellCategory

library(lifeTimes)

colnames(w_rl_TNF)

#pair RELAratio with every other

#Use for writing rela data
# write.csv(mw_rl_TNF,"lts_RELAdata.csv")

TNFvars <- mw_rl_TNF %>% dplyr::select(contains("Area"),contains("Solidity"),contains("Eccentricity"))%>%colnames()

TNFvars <- TNFvars[-1]

TNFratio <- rep("RELAratio",length(TNFvars))


TNFvars

myPairs <- cbind(TNFratio,TNFvars)

lts_RELApairs <- lts_pairsMaker(myPairs, defined = TRUE)

lts_RELApairs

mw_rl_TNF$posIDnew <- paste0(mw_rl_TNF$posID,mw_rl_TNF$cellCategory)

dim(mw_rl_TNF)
mw_rl_TNF <- mw_rl_TNF %>%
  filter(!is.na(cellCategory))
dim(mw_rl_TNF)
levels(as.factor(mw_rl_TNF$cellCategory))


str(mw_rl_TNF)

#had error with labels changing categories


lts_RELA <- lts_in(.in_tsData = mw_rl_TNF,
       .in_time = "Time_min",
       .in_compare_categorical = "oneCategory",
       .in_plot_measured_variables = TRUE,
       .in_pairedComparisons = lts_RELApairs,
       .in_uniqueID_colname = "posIDnew")


#start trouble shooting here!
#must have more than 2 objects to cluster
#if less than 2 objects, don't cluster,


lts_plot_ccfs(lts_RELA)
lts_plot_ClustSum(lts_RELA)
lts_plot_coupled(lts_RELA, .lts_facet_by = "cat1", .lts_colour_by = "cat2")


#remove NA factor levels

w_rl_TNF$posIDnew
w_rl_TNF %>%
  filter(posID == "17_1")


col

pair1 <- c("RELAratio","Cell_Area")
pair2

lts_pairsMaker(c("RELAratio",
                 "Cell_Area",
                 "Cell_Solidity",
                 "Nucleus_Area" ))


w_TNF_matrix

lts_tnf[,c("Time_min","RELAratio")]

colnames(lts_tnf)
vars_lts_tnf <- lts_tnf[,10:length(lts_tnf)]
vars_lts_tnf


#https://stackoverflow.com/questions/8805298/quickly-remove-zero-variance-variables-from-a-data-frame

#remove no variance
noVar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}

novar_rm <- noVar(vars_lts_tnf)
num_novar_rm <- as.numeric(novar_rm)

pre_pc_vars_lts_tnf <- vars_lts_tnf[,-num_novar_rm] #remove no var

#remove non variables
sel_pre_pc_vars_lts_tnf <- pre_pc_vars_lts_tnf %>%
  dplyr::select(!contains("TrackID")) %>%
  dplyr::select(!contains("BoundingBox"))
          # !contains("BoundingBox"))

# pre_pc_vars_lts_tnf %>%
  # which(apply(pre_pc_vars_lts_tnf, 2, var)==0)

#remove NA

narm_sel_pre_pc_vars_lts_tnf <- sel_pre_pc_vars_lts_tnf %>%
  tidyr::drop_na()

pc_tnf <- prcomp(narm_sel_pre_pc_vars_lts_tnf, center = TRUE, scale. = TRUE)


PC1 <- pc_tnf$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)

# coolVars <- c("Cell_Mean_RELAGFP","Cell_UpperQuartile_RELAGFP","Cytoplasm_Mean_RELAGFP","Cell_Mean_PCNA","Cell_MajorAxisLength","Cell_FormFactor","Cell_Compactness","Cell_Area")


coolVars <- c("Cell_Mean_RELAGFP","Cytoplasm_Mean_RELAGFP","Cell_MajorAxisLength","Cell_FormFactor","Cell_Compactness","Cell_Area")


sub_lts_tnf <- lts_tnf %>%
  select(c(1:9),all_of(coolVars))%>%
  tidyr::drop_na()


library(lifeTimes)

tnf_pairs <- lts_pairsMaker(coolVars)
tnf_pairs
str(tnf_pairs)

str(sub_lts_tnf)
sub_lts_tnf$Time_after_TNF

max(sub_lts_tnf$Time_after_TNF)

#make 6 hour blocks
sub_lts_tnf$hour6 <- ceiling(sub_lts_tnf$Time_after_TNF/360)
sub_lts_tnf$hour6 <- as.character(paste("hour",sub_lts_tnf$hour6,sep = "_"))

#make new track ID from combining categoricals

sub_lts_tnf$tnf_key <- paste(sub_lts_tnf$Position_TrackID, "hour",as.character(sub_lts_tnf$hour6), sep = "_")

sub_lts_tnf$tnf_key

colnames(sub_lts_tnf)
str(sub_lts_tnf);



#final cleanup
#find cells present for certain number of time
#set first appearance as timepoint zero for each









lts_tnf_in <- lts_in(.in_tsData = sub_lts_tnf,
                     .in_compare_categorical = "hour6", #problem if this is a factor as input
                      .in_time = "Timepoint",
       .in_plot_measured_variables = TRUE,
       .in_pairedComparisons = tnf_pairs,
       .in_uniqueID_colname = "tnf_key",
            .in_metaData = NULL  )

View(sub_lts_tnf)

lts_input <- lifeTimes::lts_input(.tsData = sub_lts_tnf,
                     .time = "Timepoint",
                     .compare_categorical = "hour6",
                     .plot_measured_variables = TRUE,
                     .pairedComparisons = tnf_pairs,
                     .uniqueID_colname = "tnf_key",
                     .metaData = NULL)

tnf_wide <- lifeTimes:::lts_tsToWide(lts_input) ##get list cols

View(tnf_wide)


# This error occurs when you attempt to convert a list of multiple elements to numeric without first using the unlist() function.
tnf_pairs
lts_pairs
lts_garmin <- lts_in(.in_tsData = sub_garmin,
                     .in_compare_categorical = "session_split",
                     .in_time = "two_min_time",
                     .in_plot_measured_variables = TRUE,
                     .in_pairedComparisons = lts_pairs,
                     .in_uniqueID_colname = "unq_key_garmin",
                     .in_metaData = NULL
)

# pc_tnf


#
# #remove infinite
#
# colnames(sel_pre_pc_vars_lts_tnf)
#
#
#
# # pizzas <- copy(data)
# # pizzas <- pizza[, brand := NULL]
# pc_tnf <- prcomp(sel_pre_pc_vars_lts_tnf, center = TRUE, scale. = TRUE)

# pca <- prcomp(pizzas, center = TRUE, scale. = TRUE)


pca_1_2 <- data.frame(pca$x[, 1:2])

plot(pca$x[,1], pca$x[,2])

pca_var <- pca$sdev^2
pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)
barplot(pca_var_perc, main = "Variation Plot", xlab = "PCs", ylab = "Percentage Variance", ylim = c(0, 100))

PC1 <- pca$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)
