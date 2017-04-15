install.packages("data.table")
install.packages("dplyr")
install.packages("zoo")
install.packages("Hmisc")
install.packages("corrplot")
library(data.table)
library(dplyr)
library(zoo)
library(Hmisc)
library(corrplot)



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

summary(LendingClubData_Subset$fico_range_low)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 630.0   670.0   690.0   694.8   710.0   845.0 
summary(LendingClubData_Subset$fico_range_high)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 634.0   674.0   694.0   698.8   714.0   850.0 
summary(LendingClubData_Subset$Fico_Avg)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 632.0   672.0   692.0   696.8   712.0   847.5

## 75 Columns present in the dataset



  
###############################################################################################################
#########   NORMALIZE THE NUMERIC COLUMNS #####################################################################
###############################################################################################################

# Normalize Function
normalize <-  function(x) {
  (x-min(x))/(max(x)-min(x))
}

LendingClubData_Subset$Fico_Avg <- normalize(LendingClubData_Subset$Fico_Avg)
LendingClubData_Subset$total_bal_ex_mort <- normalize(LendingClubData_Subset$total_bal_ex_mort_interpolate)
LendingClubData_Subset$num_accts_ever_120_pd <- normalize(LendingClubData_Subset$num_accts_ever_120_pd_interpolate)
LendingClubData_Subset$num_actv_bc_tl <- normalize(LendingClubData_Subset$num_actv_bc_tl_interpolate)
LendingClubData_Subset$num_actv_rev_tl <- normalize(LendingClubData_Subset$num_actv_rev_tl_interpolate)
LendingClubData_Subset$num_bc_sats <- normalize(LendingClubData_Subset$num_bc_sats_interpolate)
LendingClubData_Subset$num_bc_tl <- normalize(LendingClubData_Subset$num_bc_tl_interpolate)
LendingClubData_Subset$num_il_tl <- normalize(LendingClubData_Subset$num_il_tl_interpolate)
LendingClubData_Subset$num_op_rev_tl <- normalize(LendingClubData_Subset$num_op_rev_tl_interpolate)
LendingClubData_Subset$num_rev_accts <- normalize(LendingClubData_Subset$num_rev_accts_interpolate)
LendingClubData_Subset$num_rev_tl_bal_gt_0 <- normalize(LendingClubData_Subset$num_rev_tl_bal_gt_0_interpolate)
LendingClubData_Subset$num_stats <- normalize(LendingClubData_Subset$num_stats_interpolate)
LendingClubData_Subset$pct_tl_nvr_dlq <- normalize(LendingClubData_Subset$pct_tl_nvr_dlq_interpolate)
LendingClubData_Subset$percent_bc_gt_75 <- normalize(LendingClubData_Subset$percent_bc_gt_75_interpolate)
LendingClubData_Subset$loan_amnt <- normalize(LendingClubData_Subset$loan_amnt)
LendingClubData_Subset$funded_amnt_inv <- normalize(LendingClubData_Subset$funded_amnt_inv)
LendingClubData_Subset$int_rate <- normalize(LendingClubData_Subset$int_rate)
LendingClubData_Subset$annual_inc <- normalize(LendingClubData_Subset$annual_inc)
LendingClubData_Subset$dti <- normalize(LendingClubData_Subset$dti)
LendingClubData_Subset$delinq_2yrs <- normalize(LendingClubData_Subset$delinq_2yrs)
LendingClubData_Subset$inq_last_6mths <- normalize(LendingClubData_Subset$inq_last_6mths)
LendingClubData_Subset$mths_since_last_delinq <- normalize(LendingClubData_Subset$mths_since_last_delinq)
LendingClubData_Subset$open_acc <- normalize(LendingClubData_Subset$open_acc)
LendingClubData_Subset$pub_rec <- normalize(LendingClubData_Subset$pub_rec)
LendingClubData_Subset$revol_bal <- normalize(LendingClubData_Subset$revol_bal)
LendingClubData_Subset$revol_util <- normalize(LendingClubData_Subset$revol_util)
LendingClubData_Subset$total_acc <- normalize(LendingClubData_Subset$total_acc)
LendingClubData_Subset$out_prncp_inv <- normalize(LendingClubData_Subset$out_prncp_inv)
LendingClubData_Subset$total_pymnt_inv <- normalize(LendingClubData_Subset$total_pymnt_inv)
LendingClubData_Subset$total_rec_prncp <- normalize(LendingClubData_Subset$total_rec_prncp)
LendingClubData_Subset$total_rec_int <- normalize(LendingClubData_Subset$total_rec_int)
LendingClubData_Subset$recoveries <- normalize(LendingClubData_Subset$recoveries)
LendingClubData_Subset$last_pymnt_amnt <- normalize(LendingClubData_Subset$last_pymnt_amnt)
LendingClubData_Subset$mths_since_last_major_derog <- normalize(LendingClubData_Subset$mths_since_last_major_derog)
LendingClubData_Subset$annual_inc_joint <- normalize(LendingClubData_Subset$annual_inc_joint)
LendingClubData_Subset$dti_joint <- normalize(LendingClubData_Subset$dti_joint)
LendingClubData_Subset$acc_now_delinq <- normalize(LendingClubData_Subset$acc_now_delinq)
LendingClubData_Subset$tot_coll_amt <- normalize(LendingClubData_Subset$tot_coll_amt)
LendingClubData_Subset$tot_cur_bal <- normalize(LendingClubData_Subset$tot_cur_bal)
LendingClubData_Subset$open_acc_6m <- normalize(LendingClubData$open_acc_6m)
LendingClubData_Subset$mths_since_rcnt_il <- normalize(LendingClubData_Subset$mths_since_rcnt_il)
LendingClubData_Subset$total_bal_il <- normalize(LendingClubData_Subset$total_bal_il )
LendingClubData_Subset$il_util <- normalize(LendingClubData_Subset$il_util)
LendingClubData_Subset$total_rev_hi_lim <- normalize(LendingClubData_Subset$total_rev_hi_lim)
LendingClubData_Subset$acc_open_past_24mths <- normalize(LendingClubData_Subset$acc_open_past_24mths)
LendingClubData_Subset$avg_cur_bal <- normalize(LendingClubData_Subset$avg_cur_bal)
LendingClubData_Subset$bc_util <- normalize(LendingClubData_Subset$bc_util)
LendingClubData_Subset$chargeoff_within_12_mths <- normalize(LendingClubData_Subset$chargeoff_within_12_mths)
LendingClubData_Subset$delinq_amnt <- normalize(LendingClubData_Subset$delinq_amnt)
LendingClubData_Subset$mort_acc <- normalize(LendingClubData_Subset$mort_acc)
LendingClubData_Subset$mths_since_recent_bc <- normalize(LendingClubData_Subset$mths_since_recent_bc)
LendingClubData_Subset$mths_since_recent_bc_dlq <- normalize(LendingClubData_Subset$mths_since_recent_bc_dlq)
LendingClubData_Subset$mths_since_recent_revol_delinq <- normalize(LendingClubData_Subset$mths_since_recent_revol_delinq)
LendingClubData_Subset$pub_rec_bankruptcies <- normalize(LendingClubData_Subset$pub_rec_bankruptcies)
LendingClubData_Subset$tax_liens <- normalize(LendingClubData_Subset$tax_liens)
LendingClubData_Subset$tot_hi_cred_lim <- normalize(LendingClubData_Subset$tot_hi_cred_lim)
LendingClubData_Subset$total_bc_limit <- normalize(LendingClubData_Subset$total_bc_limit_interpolate)

LendingClubData_Subset <- select(LendingClubData_Subset, -c(num_accts_ever_120_pd_interpolate, num_actv_bc_tl_interpolate,
                                                            num_actv_rev_tl_interpolate,num_bc_sats_interpolate, 
                                                            num_bc_tl_interpolate,total_bc_limit_interpolate,
                                                            num_il_tl_interpolate, num_op_rev_tl_interpolate, 
                                                            num_rev_accts_interpolate, 
                                                            num_rev_tl_bal_gt_0_interpolate, num_stats_interpolate, 
                                                            pct_tl_nvr_dlq_interpolate,
                                                            percent_bc_gt_75_interpolate,total_bal_ex_mort_interpolate))

#########################################################################################################################
################## CORRELATION ##########################################################################################
##########################################################################################################################

#Create a separate dataframe for Correlation 

Correlation_df <- select(LendingClubData_Subset, -c(id,term,int_rate,installment,grade,sub_grade,home_ownership,verification_status,
                                                    loan_status,purpose,addr_state, fico_range_low, fico_range_high,initial_list_status,
                                                    policy_code,application_type,emp_length_clean))

cor <- rcorr(as.matrix(Correlation_df), type="pearson")  # type can be pearson or spearman
cor$r

cor_r <- as.matrix(cor$r)

write.csv(cor_r, "cor_r.csv")

corrplot(cor_r, method="circle")


#Remove Columns with high correlation
#num_stats <-> open_acc
#tax_liens <-> pub_rec
#percent_bc_gt_75 <-> revol_util 
#last_pymnt_amnt <-> total_rec_prncp
#total_rev_hi_lim <-> total_bc_limit, total_cur_bal
#mths_since_recent_bc_dlq <-> mths_since_recent_revol_delinq
#num_bc_tl <-> num_bc_sats
#num_op_rev_tl <-> num_bc_sats
#num_op_rev_tl <-> num_rev_accts
#total_accts <-> num_rev_accts
LendingClubData_Subset_2 <- select(LendingClubData_Subset, -c(funded_amnt_inv,total_pymnt_inv,
                                                              num_rev_accts,num_stats,open_acc,
                                                              pub_rec,percent_bc_gt_75,last_pymnt_amnt,
                                                              total_rev_hi_lim,mths_since_recent_revol_delinq,num_bc_tl,
                                                              num_op_rev_tl))



pairs_df <- select(LendingClubData_Subset_2, -c(id,term,int_rate,installment,grade,sub_grade,home_ownership,verification_status,
                                                loan_status,purpose,addr_state, fico_range_low, fico_range_high,initial_list_status,
                                                policy_code,application_type,emp_length_clean))
pairs(pairs_df)

pca1 <- princomp(pairs_df, scores = TRUE , cor = TRUE)
summary(pca1)
plot(pca1)
plot(pca1, type="1")

biplot(pca1)

write.cpca1$loadings


