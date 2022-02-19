#testing script

lts <- lifeTimes:::dev_lts_inputNoCalls()

lts_wide <- lifeTimes:::lts_tsToWide(lts)
lts_ccf <- lifeTimes:::lts_wide_ts_to_ccf(.lts_cast_ts = lts_wide ,.lts_variables = lts )
lts_df <- lifeTimes:::lts_ccf_df(lts_ccf, lts)
lts_metadf <- lifeTimes:::lts_metaData_ccf_join(lts_df,lts)
lts_summs <- lifeTimes:::lts_summarise_ccf(lts_metadf, lts)
lts_cluster <- lifeTimes:::lts_cluster_ccf_summs(lts_summs, lts)

lifeTimes:::lts_tsToWide()

output <- lts_in()

lts_clustPlot(output)

lts_sumClustPlot(output)

lts_couplePlot(output)



ltsC <- lts_calc()

ltsC$lts_CCFcalcs

singleSums <- lts_cluster$lts_ccf_summaries$lts_singleton_summ_metadata


.lts_compare_by <- as.vector(lts_cluster$lts_variables$lts_compare_by)
.lts_unique_ID_colnames <- lts_cluster$lts_variables$lts_uniqueID_colname
.lts_measures <- unlist(lts_cluster$lts_variables$lts_pariedComparisons)
names(.lts_measures) <- .lts_measures #fix names to be the same as the variables


valsFromKey <- c(.lts_compare_by,.unique_ID_colnames)

library(tidyr)
colnames(singleSums)

# valsFromKey

'%!in%' <- function(x,y)!('%in%'(x,y))

valsFrom <- which(!(colnames(singleSums) %in% c(.lts_compare_by,.lts_unique_ID_colnames)))
valsFrom

dim(singleSums)

w_singleSums <- pivot_wider(singleSums,
            names_from = lag_range,
            values_from = all_of(valsFrom),
            names_glue = "{lag_range}_{.value}")

head(w_singleSums)

colnarm_w_singleSums <- w_singleSums[,colSums(is.na(w_singleSums))<nrow(w_singleSums)]



#remove rows where some are na

# install.packages("dplyr")
rownarm_colnarm_w_singleSums <- colnarm_w_singleSums %>%
  tidyr::drop_na()



#select_numeric
num_cols <- unlist(lapply(rownarm_colnarm_w_singleSums, is.numeric))         # Identify numeric columns
# num_cols

label_cols <- unlist(lapply(rownarm_colnarm_w_singleSums, is.numeric))

num_w_singleSums <- rownarm_colnarm_w_singleSums[ , num_cols]                        # Subset numeric columns of data
colnames(num_w_singleSums)

label_w_singleSums <- rownarm_colnarm_w_singleSums[ , !num_cols]
label_w_singleSums
label_w_singleSums$catchSeas <- paste0(label_w_singleSums$season,label_w_singleSums$catchmentRegion)

# num_w_singleSums <- subset(rownarm_colnarm_w_singleSums, is.numeric(rownarm_colnarm_w_singleSums))

pc_num_w_singleSums <- prcomp(num_w_singleSums, scale. = TRUE, center = TRUE)

.lts_unique_ID_colnames

colnames(lts_cluster$lts_variables$lts_data)

is.list(.lts_compare_by)
is.list(.lts_unique_ID_colnames)

c(rlang::sym(.lts_unique_ID_colnames),rlang::syms(.lts_compare_by))


#define mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

unique(lts_cluster$lts_variables$lts_data$key_num)

sumOriginal <- lts_cluster$lts_variables$lts_data %>%
  # dplyr::select(key_num, season, catchmentRegion) %>%
  dplyr::select(!!!rlang::syms(c(.lts_compare_by, .lts_unique_ID_colnames, .lts_measures))) %>%
  dplyr::group_by(!!!rlang::syms(c(.lts_unique_ID_colnames,.lts_compare_by)))%>% #keep categoricals, although not of interest for summary, the metadata is important
  dplyr::summarise_at(c(.lts_measures), c(mean = mean,
                                          median = median,
                                          sd = sd,
                                          var = var,
                                          IQR = IQR,
                                          # quantile = quantile,
                                          min = min,
                                          max = max,
                                          sum = sum))
# View(sumOriginal)



#Apply prcomp in a loop
#add name of list to output
#so get
#first list_ (results_labels)
#so apply funtions in a loop and add to list in r

lapply(lts_prcomps, lts_calc_prcomp)



str(pcTest$lts_pc_values)

lts_calc_prcomp <- function(lts_summary_data){
#For these three dataframes
#original, ccf and merge
#look at PCA space

#remove columns which are all NA
colnarm_w_singleSums <- lts_summary_data[,colSums(is.na(lts_summary_data))<nrow(lts_summary_data)]

rownarm_colnarm_w_singleSums <- colnarm_w_singleSums %>% #remove rows where some are na
  tidyr::drop_na()

num_cols <- unlist(lapply(rownarm_colnarm_w_singleSums, is.numeric))         # Identify numeric columns

# label_cols <- unlist(lapply(rownarm_colnarm_w_singleSums, is.numeric))

num_w_singleSums <- rownarm_colnarm_w_singleSums[ , num_cols]                        # Subset numeric columns of data

lts_noVar <- as.numeric(which(apply(num_w_singleSums, 2, var) ==0)) #find columns without variance
if(length(lts_noVar) >0){    #remove columns without variance
hasvar_num_w_singleSums <- num_w_singleSums[ -lts_noVar]} else{
  hasvar_num_w_singleSums <- num_w_singleSums
}


label_w_singleSums <- rownarm_colnarm_w_singleSums[ , !num_cols] #only get labels after NA rows are removed
label_w_singleSums$combinedCategoricals <- paste0("combine_",.lts_compare_by[[1]],"_",.lts_compare_by[[2]])

# num_w_singleSums <- subset(rownarm_colnarm_w_singleSums, is.numeric(rownarm_colnarm_w_singleSums))
pc_num_w_singleSums <- prcomp(hasvar_num_w_singleSums, scale. = TRUE, center = TRUE)

lts_pc_results <- list(lts_pc_values = pc_num_w_singleSums,
     lts_pc_labels = label_w_singleSums
     )
# return(lts_pc_results)
# }


join_original_ccf_summ <- merge(sumOriginal, w_singleSums, by = c(.lts_unique_ID_colnames))

dim(sumOriginal)
dim(w_singleSums)
dim(join_original_ccf_summ)


lts_prcomps <- list(sumOriginal, w_singleSums,join_original_ccf_summ)

pcTest <- lts_calc_prcomp(sumOriginal)
# pcTest$lts_pc_labels


join_original_ccf_summ

autoplot(pcTest$lts_pc_values, data = pcTest$lts_pc_labels, colour = "catchmentRegion"
         # shape = "season"
         )+
  scale_color_manual(values = c("dodgerblue","darkorange"))+
  # scale_color_viridis(discrete = TRUE, option = "A")+
  theme_classic()


autoplot(pc_num_w_singleSums, data = label_w_singleSums, colour = "catchmentRegion")
autoplot(pc_num_w_singleSums, data = label_w_singleSums, colour = "catchSeas")


warnings()
w_singleSums


#remove columns which are all NA

#get numeric columns

#plot PCA

colnames(w_singleSums)

?pivot_wider()



###PLSR

library(pls)
model <- plsr(hp~mpg+disp+drat+wt+qsec, data=mtcars, scale=TRUE, validation="CV")


#remove NA and zero variance for PLSR

lts_prcomps <- list(sumOriginal, w_singleSums,join_original_ccf_summ)


PLSR_colnarm_w_singleSums <- join_original_ccf_summ[,colSums(is.na(join_original_ccf_summ))<nrow(join_original_ccf_summ)]


PLSR_rownarm_colnarm_w_singleSums <- PLSR_colnarm_w_singleSums %>% #remove rows where some are na
  tidyr::drop_na()

PLSR_num_cols <- unlist(lapply(PLSR_rownarm_colnarm_w_singleSums, is.numeric))         # Identify numeric columns

# label_cols <- unlist(lapply(PLSR_rownarm_colnarm_w_singleSums, is.numeric))

PLSR_num_w_singleSums <- PLSR_rownarm_colnarm_w_singleSums[ , PLSR_num_cols]                        # Subset numeric columns of data

lts_noVar <- as.numeric(which(apply(PLSR_num_w_singleSums, 2, var) ==0)) #find columns without variance
if(length(lts_noVar) >0){    #remove columns without variance
  PLSR_hasvar_num_w_singleSums <- PLSR_num_w_singleSums[ -lts_noVar]} else{
    PLSR_hasvar_num_w_singleSums <- PLSR_num_w_singleSums}

PLSR_hasvar_num_w_singleSums
label_w_singleSums <- PLSR_rownarm_colnarm_w_singleSums[ , !PLSR_num_cols] #only get labels after NA rows are removed
label_w_singleSums$combinedCategoricals <- paste0("combine_",.lts_compare_by[[1]],"_",.lts_compare_by[[2]])


PLSR_vals_and_labels <- cbind(PLSR_hasvar_num_w_singleSums, label_w_singleSums)

#generalise this to categorical variables
PLSR_vals_and_labels$catchmentRegion
PLSR_vals_and_labels$num_CatchmentRegion <- as.numeric(PLSR_vals_and_labels$catchmentRegion)
PLSR_vals_and_labels$num_Season <- as.numeric(PLSR_vals_and_labels$season)

# sumOriginal$num_CatchmentRegion <- as.numeric(sumOriginal$catchmentRegion)

# catchment <- plsr()
colnames(PLSR_vals_and_labels)
# colnames(sumOriginal)

colnames(PLSR_vals_and_labels)
str(PLSR_vals_and_labels)

#Get all columns that are not categorical variables or response variables
lts_plsr_variables <- colnames(PLSR_vals_and_labels[c(1:length(PLSR_hasvar_num_w_singleSums))])

# pls_var_syms <- rlang::syms((paste0(colnames(PLSR_vals_and_labels[c(1:5)]), collapse = " + " )))
# # pls_var_syms
# str(unlist(pls_var_syms))
# eval(pls_var_syms)

# lts_plsr_sym_variables <- paste0(rlang::syms(lts_plsr_variables), collapse = " + ")


# lts_plsr_sym_variables <- rlang::syms(lts_plsr_variables)
librar
# str(paste0(lts_plsr_sym_variables, collapse ="+"))
colnames(PLSR_vals_and_labels)
sub_PLSR_vals_and_labels <- PLSR_vals_and_labels[-c(120:126)]
colnames(sub_PLSR_vals_and_labels)
catch_model <- plsr(num_Season ~ ., data=sub_PLSR_vals_and_labels, scale=TRUE, validation="CV")

summary(catch_model)
plot(catch_model)
#
#
# str(pls::yarn)
#
# yarn.pls <- plsr(density ~ NIR, 6, data = pls::yarn
# , validation = "CV")
#
# summary(yarn.pls)

summary(catch_model)



catch_model <- plsr(num_Season ~
                      rainfall_cm_mean +
                      flow_m3s_mean +
                      rainfall_cm_median +
                      flow_m3s_median+
                      rainfall_cm_sd +
                      flow_m3s_sd +
                      rainfall_cm_var +
                      flow_m3s_var +
                      rainfall_cm_IQR +
                      flow_m3s_IQR +
                      rainfall_cm_min +
                      flow_m3s_min+
                      rainfall_cm_max +
                      flow_m3s_max +
                      rainfall_cm_sum +
                      flow_m3s_sum

                      , data=PLSR_vals_and_labels, scale=TRUE, validation="CV")

summary(catch_model)

plot(catch_model)

