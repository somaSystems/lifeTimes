#TNF washout

lts_tnf <- read.csv(file = "data-raw/211021_TNF_washout_selected_tracks_FILTERED.csv")


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
