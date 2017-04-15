install.packages("data.table")
install.packages("dplyr")
install.packages("zoo")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("cluster")
install.packages("Rtsne")
library(cluster)
library(data.table)
library(dplyr)
library(zoo)
library(Hmisc)
library(corrplot)
library(Rtsne)



LendingClubData <- fread("combinedData_Clean.csv", sep =',')

###############################################################################################################
##############     DROP UNNECESSARY COLUMNS       #############################################################
###############################################################################################################

LendingClubData_Subset <- select(LendingClubData, -c(member_id,V1,url,desc,emp_title,title,
                                                     zip_code,pymnt_plan,next_pymnt_d,last_credit_pull_d,
                                                     last_fico_range_high,last_fico_range_low, collections_12_mths_ex_med,
                                                     open_rv_12m,open_rv_24m,max_bal_bc,all_util,inq_fi,total_cu_tl,out_prncp,
                                                     total_pymnt,num_tl_120dpd_2m,num_tl_30dpd,num_tl_90g_dpd_24m,
                                                     num_tl_op_past_12m,total_il_high_credit_limit,funded_amnt,emp_length,
                                                     mths_since_last_record,total_rec_late_fee,last_pymnt_d,inq_last_12m,mo_sin_old_il_acct,
                                                     mo_sin_old_rev_tl_op,earliest_cr_line,mths_since_recent_inq,mo_sin_rcnt_tl,
                                                     mo_sin_rcnt_rev_tl_op,open_il_6m, open_il_12m, open_il_24m,
                                                     collection_recovery_fee,bc_open_to_buy, 
                                                     verification_status_joint))


##############################################################################################################
########  CATEGORIZE THE COLUMNS #############################################################################
##############################################################################################################

LendingClubData_Subset$term <- as.factor(LendingClubData_Subset$term)
LendingClubData_Subset$grade <- as.factor(LendingClubData_Subset$grade)
LendingClubData_Subset$sub_grade <- as.factor(LendingClubData_Subset$sub_grade)
LendingClubData_Subset$home_ownership <- as.factor(LendingClubData_Subset$home_ownership)
LendingClubData_Subset$verification_status <- as.factor(LendingClubData_Subset$verification_status)
LendingClubData_Subset$loan_status <- as.factor(LendingClubData_Subset$loan_status)
LendingClubData_Subset$purpose <- as.factor(LendingClubData_Subset$purpose)
LendingClubData_Subset$addr_state <- as.factor(LendingClubData_Subset$addr_state)
LendingClubData_Subset$policy_code <- as.factor(LendingClubData_Subset$policy_code)
LendingClubData_Subset$application_type <- as.factor(LendingClubData_Subset$emp_length_clean)
LendingClubData_Subset$issue_d <- as.factor(LendingClubData_Subset$issue_d)

summary(LendingClubData_Subset)


##############################################################################################################
############# Check The Columns with nulls ###################################################################
##############################################################################################################

NullDataCheck <- filter(LendingClubData_Subset, grade == 0)	


###############################################################################################################
##############     INTERPOLATION                  #############################################################
###############################################################################################################

LendingClubData_Subset$total_bal_ex_mort_interpolate <- na.approx(LendingClubData_Subset$total_bal_ex_mort)
LendingClubData_Subset$num_accts_ever_120_pd_interpolate <- na.approx(LendingClubData_Subset$num_accts_ever_120_pd)
LendingClubData_Subset$num_actv_bc_tl_interpolate <- na.approx(LendingClubData_Subset$num_actv_bc_tl)
LendingClubData_Subset$num_actv_rev_tl_interpolate <- na.approx(LendingClubData_Subset$num_actv_rev_tl)
LendingClubData_Subset$num_bc_sats_interpolate <- na.approx(LendingClubData_Subset$num_bc_sats)
LendingClubData_Subset$num_bc_tl_interpolate <- na.approx(LendingClubData_Subset$num_bc_tl)
LendingClubData_Subset$num_il_tl_interpolate <- na.approx(LendingClubData_Subset$num_il_tl)
LendingClubData_Subset$num_op_rev_tl_interpolate <- na.approx(LendingClubData_Subset$num_op_rev_tl)
LendingClubData_Subset$num_rev_accts_interpolate <- na.approx(LendingClubData_Subset$num_rev_accts)
LendingClubData_Subset$num_rev_tl_bal_gt_0_interpolate <- na.approx(LendingClubData_Subset$num_rev_tl_bal_gt_0)
LendingClubData_Subset$num_stats_interpolate <- na.approx(LendingClubData_Subset$num_sats)
LendingClubData_Subset$pct_tl_nvr_dlq_interpolate <- na.approx(LendingClubData_Subset$pct_tl_nvr_dlq)
LendingClubData_Subset$percent_bc_gt_75_interpolate <- na.approx(LendingClubData_Subset$percent_bc_gt_75)
LendingClubData_Subset$total_bc_limit_interpolate <- na.approx.default(LendingClubData_Subset$total_bc_limit)

summary(LendingClubData_Subset)



###############################################################################################################
##########   NOW REMOVE THE COLUMNS WITH NULLS ################################################################
###############################################################################################################

LendingClubData_Subset <- select(LendingClubData_Subset, -c(num_accts_ever_120_pd, num_actv_bc_tl,
                                                            num_actv_rev_tl,num_bc_sats, num_bc_tl,
                                                            num_il_tl, num_op_rev_tl, num_rev_accts, 
                                                            num_rev_tl_bal_gt_0, num_sats, pct_tl_nvr_dlq,
                                                            percent_bc_gt_75,total_bal_ex_mort,total_bc_limit))

summary(LendingClubData_Subset)


################################################################################################################
####### WITH NA's and NO Interpolation #########################################################################
################################################################################################################

# Assuming for the NA's as 0

LendingClubData_Subset$mort_acc[is.na(LendingClubData_Subset$mort_acc)] <- 0
LendingClubData_Subset$chargeoff_within_12_mths[is.na(LendingClubData_Subset$chargeoff_within_12_mths)] <- 0
LendingClubData_Subset$pub_rec_bankruptcies[is.na(LendingClubData_Subset$pub_rec_bankruptcies)] <- 0


summary(LendingClubData_Subset)



################################################################################################################
######### DERIVED COLUMNS ######################################################################################
################################################################################################################

fico_avg <- function(x,y) {
  (x+y)/2
}

LendingClubData_Subset$Fico_Avg <- fico_avg(LendingClubData_Subset$fico_range_low,LendingClubData_Subset$fico_range_high)


##################################################################################################################
######### SELECTED FEATURES ######################################################################################
##################################################################################################################

### Lending_CLub_Features <- select(LendingClubData_Subset, c(loan_amnt,term,int_rate,installment,grade,home_ownership,
###                                                          annual_inc,verification_status,purpose,addr_state,
###                                                          emp_length_clean,dti,Fico_Avg,
###                                                          delinq_2yrs,inq_last_6mths,open_acc,revol_bal,revol_util,pub_rec))
###

Lending_CLub_Features <- select(LendingClubData_Subset, c(loan_amnt,term,int_rate,installment,grade,home_ownership,
                                                          annual_inc,verification_status,purpose,addr_state,
                                                          emp_length_clean,dti,
                                                          delinq_2yrs,inq_last_6mths,open_acc,revol_bal,revol_util))

Lending_CLub_Features$term_1 <- as.numeric(Lending_CLub_Features$term)
Lending_CLub_Features$grade_1 <- as.numeric(Lending_CLub_Features$grade)
Lending_CLub_Features$home_ownership_1 <- as.numeric(Lending_CLub_Features$home_ownership)
Lending_CLub_Features$verification_status_1 <- as.numeric(Lending_CLub_Features$verification_status)
Lending_CLub_Features$purpose_1 <- as.numeric(Lending_CLub_Features$purpose)
Lending_CLub_Features$addr_state_1 <- as.numeric(Lending_CLub_Features$addr_state)
Lending_CLub_Features$emp_length_clean <- as.numeric(Lending_CLub_Features$emp_length_clean)

#write.csv(Lending_CLub_Features, "Lending_CLub_Data.csv")


LendingClubLoan_Cluster <- Lending_CLub_Features[sample(1:nrow(Lending_CLub_Features),15000, replace=FALSE), ]


gower_dist <- daisy(LendingClubLoan_Cluster, metric = "gower")

gower_mat <- as.matrix(gower_dist)

### Step 3 Selecting number of clusters 
sil_width <- c(NA)

for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width (higher is better, based on the result, 3  clusters yeil the highest value)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit <- pam(gower_dist, diss = TRUE, k = 3)


pam_results <- LendingClubLoan_Cluster %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

Clustering <- LendingClubLoan_Cluster %>%
  mutate(Cluster = pam_fit$clustering)

pam_results$the_summary

ggplot(aes(x = loan_amnt, y = grade), data = Clustering) +
  geom_point(aes(color = factor(Cluster))) +
  ggtitle("Clusters on two variables")

ggplot(aes(x = int_rate, y = grade), data = Clustering) + 
  geom_point(aes(color = factor(Cluster))) +
  ggtitle("Clusters on two variables")

ggplot(aes(x = loan_amnt, y = int_rate), data = Clustering) + 
  geom_point(aes(color = factor(Cluster))) +
  ggtitle("Clusters on two variables")

Cluster_1 <- filter(Clustering, Cluster == 1)
Cluster_2 <- filter(Clustering, Cluster == 2)
Cluster_3 <- filter(Clustering, Cluster == 3)


## Step 4-3 Visualization with reduced-dimension 
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  ggtitle("Clusters on reduced-dimension")




