# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("RCurl")
#install.packages("rjson")
#install.packages("shiny")


library(shiny)
library(RCurl)
library(rjson)

# Accept SSL certificates issued by public Certificate Authorities
#options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

############################################################################################################################
######################### CLASSIFICATION FUNCTION ##########################################################################
############################################################################################################################

http_function_classification  <- function(a,b,c,d,e){
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "input1" = list(
        "ColumnNames" = list("Amount Requested", "Debt-To-Income Ratio", "State", "emp_length", "grade_new"),
        "Values" = list( list( a, b, c, d, e ) )
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "qvIpISawdz+skof96JoywQ9+NZtOAVal9KlS2JtW8elnE9JajrSe4Eo8bpX66BxbnFI28LIYsY/sakiaOj78PQ==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/aba4b61d30814babbe06ae6b36e7a3ca/services/25553e2f25b84936ab553e964e4337a6/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  
  
  print("Result:")
  result = h$value()
  result1 <- fromJSON(result)
  LoanResult <- result1$Results$output1$value$Values[[1]][6]
  if (LoanResult == 1)
    LoanResult <- "Yes"
  else LoanResult <- "No"
  return(LoanResult)
  # return(fromJSON(result))
}


############################################################################################################################
######################### PREDICTION FUNCTION #############################################################################
############################################################################################################################


########################################### A ############################################################################

http_function_prediction_A  <- function(loan_amount_name,dti_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_bal_name,revol_util_name){
  
  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "input1" = list(
        "ColumnNames" = list("loan_amnt", "term", "installment", "home_ownership", "annual_inc", "verification_status", "issue_d", "purpose", "addr_state", "emp_length_clean", "dti", "Fico_Avg", "delinq_2yrs", "inq_last_6mths", "open_acc", "revol_bal", "revol_util", "pub_rec"),
        "Values" = list( list( loan_amount_name, term_name , 200 , ho_name , ai_name, "Verified", "2017-04-14", purpose_name, state_name, emp_len_name, dti_name, fico_name, 1, 0, open_acc_name, revol_bal_name, revol_util_name, 2 ))
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "aNcqcRnLKj8dEOwGb/XQ2Ms6HPGCGs20xPN7QZSj2g9iBPbgRpBHdAM90MvbjU6LL5cVKs5x2WlmCJnCEBxdTQ==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/549e3a96205f41fbac51955b386464a0/services/54f3080e5291423ba8cb7c4b4854c552/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  
  print("Interest Rate is:")
  result = h$value()
  resultpred <- fromJSON(result)
  resultpred1 <- resultpred$Results$output1$value$Values[[1]][19]
  return(resultpred1)
  
}


########################################### B ############################################################################

http_function_prediction_B  <- function(loan_amount_name,dti_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_bal_name,revol_util_name){
  
  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "input1" = list(
        "ColumnNames" = list("loan_amnt", "term", "installment", "home_ownership", "annual_inc", "verification_status", "issue_d", "purpose", "addr_state", "emp_length_clean", "dti", "Fico_Avg", "delinq_2yrs", "inq_last_6mths", "open_acc", "revol_bal", "revol_util", "pub_rec"),
        "Values" = list( list( loan_amount_name, term_name , 200 , ho_name , ai_name, "Verified", "2017-04-14", purpose_name, state_name, emp_len_name, dti_name, fico_name, 1, 0, open_acc_name, revol_bal_name, revol_util_name, 2 ))
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "u1KiUzJTuJH8i449Ms2LxC9xUcgiQEAsM2nR/2wkd7d137aluRzPYVGn27QVrH0GxX/aoV/e4xCTg4aQogxhnw==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/549e3a96205f41fbac51955b386464a0/services/a11ed3f68c7a4c188c297c644d2238de/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  
  print("Interest Rate is:")
  result = h$value()
  resultpred <- fromJSON(result)
  resultpred1 <- resultpred$Results$output1$value$Values[[1]][19]
  return(resultpred1)
  
}



########################### C #############################################################################################

http_function_prediction_C  <- function(loan_amount_name,dti_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_bal_name,revol_util_name){
  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "input1" = list(
        "ColumnNames" = list("loan_amnt", "term", "installment", "home_ownership", "annual_inc", "verification_status", "issue_d", "purpose", "addr_state", "emp_length_clean", "dti", "Fico_Avg", "delinq_2yrs", "inq_last_6mths", "open_acc", "revol_bal", "revol_util", "pub_rec"),
        "Values" = list( list( loan_amount_name, term_name , 200 , ho_name , ai_name, "Verified", "2017-04-14", purpose_name, state_name, emp_len_name, dti_name, fico_name, 1, 0, open_acc_name, revol_bal_name, revol_util_name, 2 ))
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "BDszVBVJNeg018R+YGUub4QECv/oiOMPnN8pc4tj1j/WwXkS5ImZz6Vk56GNc+U8km98O4ZrexkQ66d9E6KyAA==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/549e3a96205f41fbac51955b386464a0/services/70d09e3d923f40d0a7f0c7da714e7164/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  
  
  print("Interest Rate is:")
  result = h$value()
  resultpred <- fromJSON(result)
  resultpred1 <- resultpred$Results$output1$value$Values[[1]][19]
  return(resultpred1)
  
}


########################################### D ############################################################################

http_function_prediction_D  <- function(loan_amount_name,dti_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_bal_name,revol_util_name){
  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "input1" = list(
        "ColumnNames" = list("loan_amnt", "term", "installment", "home_ownership", "annual_inc", "verification_status", "issue_d", "purpose", "addr_state", "emp_length_clean", "dti", "Fico_Avg", "delinq_2yrs", "inq_last_6mths", "open_acc", "revol_bal", "revol_util", "pub_rec"),
        "Values" = list( list( loan_amount_name, term_name , 200 , ho_name , ai_name, "Verified", "2017-04-14", purpose_name, state_name, emp_len_name, dti_name, fico_name, 1, 0, open_acc_name, revol_bal_name, revol_util_name, 2 ))
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "BZnhRmzXwVLphoWfO2eLqhq1rN6HZ4O4PGDAQvklnI3gEJ1cR9TTxW3wO8oILALALYNDbY9QC23muaGbyMNA1Q==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/549e3a96205f41fbac51955b386464a0/services/0e518c4ee94546219fe447d4fd6b005b/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  
  print("Interest Rate is:")
  result = h$value()
  resultpred <- fromJSON(result)
  resultpred1 <- resultpred$Results$output1$value$Values[[1]][19]
  return(resultpred1)
}


########################################### E ############################################################################

http_function_prediction_E  <- function(loan_amount_name,dti_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_bal_name,revol_util_name){
  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "input1" = list(
        "ColumnNames" = list("loan_amnt", "term", "installment", "home_ownership", "annual_inc", "verification_status", "issue_d", "purpose", "addr_state", "emp_length_clean", "dti", "Fico_Avg", "delinq_2yrs", "inq_last_6mths", "open_acc", "revol_bal", "revol_util", "pub_rec"),
        "Values" = list(list( loan_amount_name, term_name , 200 , ho_name , ai_name, "Verified", "2017-04-14", purpose_name, state_name, emp_len_name, dti_name, fico_name, 1, 0, open_acc_name, revol_bal_name, revol_util_name, 2 ))
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "gx82jADrVaia5iUSdfwCS25BgVIE8yeCPxf/wIN5CyVHX6YjyPVBCBp/ccx3nh7DiCh8pp5SuOTFbBm9hceWZQ==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/549e3a96205f41fbac51955b386464a0/services/af01a53916004cf09d577db9ceba233c/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  
  print("Interest Rate is:")
  result = h$value()
  resultpred <- fromJSON(result)
  resultpred1 <- resultpred$Results$output1$value$Values[[1]][19]
  return(resultpred1)
}


########################### F #############################################################################################

http_function_prediction_F  <- function(loan_amount_name,dti_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_bal_name,revol_util_name){
  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "input1" = list(
        "ColumnNames" = list("loan_amnt", "term", "installment", "home_ownership", "annual_inc", "verification_status", "issue_d", "purpose", "addr_state", "emp_length_clean", "dti", "Fico_Avg", "delinq_2yrs", "inq_last_6mths", "open_acc", "revol_bal", "revol_util", "pub_rec"),
        "Values" = list(list( loan_amount_name, term_name , 200 , ho_name , ai_name, "Verified", "2017-04-14", purpose_name, state_name, emp_len_name, dti_name, fico_name, 1, 0, open_acc_name, revol_bal_name, revol_util_name, 2 ))
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "RsmAjNBtHfQFed5iePPFBh7wTpfzlA/ZBXTCqjzYIHZoDAuZvS7i3roynn59/LJ7R3MHJ186XbRV0amTWBaBng=="# Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/549e3a96205f41fbac51955b386464a0/services/f60d73cdaa4e434284cb9866e66dc9fa/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  
  print("Interest Rate is:")
  result = h$value()
  resultpred <- fromJSON(result)
  resultpred1 <- resultpred$Results$output1$value$Values[[1]][19]
  return(resultpred1)
}


########################################### G ############################################################################

http_function_prediction_G  <- function(loan_amount_name,dti_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_bal_name,revol_util_name){
  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "input1" = list(
        "ColumnNames" = list("loan_amnt", "term", "installment", "home_ownership", "annual_inc", "verification_status", "issue_d", "purpose", "addr_state", "emp_length_clean", "dti", "Fico_Avg", "delinq_2yrs", "inq_last_6mths", "open_acc", "revol_bal", "revol_util", "pub_rec"),
        "Values" = list( list( loan_amount_name, term_name , 200 , ho_name , ai_name, "Verified", "2017-04-14", purpose_name, state_name, emp_len_name, dti_name, fico_name, 1, 0, open_acc_name, revol_bal_name, revol_util_name, 2 ))
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "pQiiyxdkeKLLZJNSKB+/1z610caryEzQCbIIYHAku/K5VANyzovyRGQJRIO85gmX1kZVtN/bjNqCzLN90RdIDw==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/549e3a96205f41fbac51955b386464a0/services/5347f62db8324133802bc9e45bf31136/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  
  print("Interest Rate is:")
  result = h$value()
  resultpred <- fromJSON(result)
  resultpred1 <- resultpred$Results$output1$value$Values[[1]][19]
  return(resultpred1)
}

################################################################################################################
########################################### KMEANS #############################################################
################################################################################################################


http_function_KMEANS  <- function(loan_amount_name,dti_name,ai_name,open_acc_name,revol_bal_name,revol_util_name,emp_len_name){
  
# Accept SSL certificates issued by public Certificate Authorities
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

h = basicTextGatherer()
hdr = basicHeaderGatherer()


req = list(
  
  Inputs = list(
    
    
    "input1" = list(
      "ColumnNames" = list("Column 0", "loan_amnt", "term", "int_rate", "installment", "grade", "home_ownership", "annual_inc", "verification_status", "purpose", "addr_state", "emp_length_clean", "dti", "delinq_2yrs", "inq_last_6mths", "open_acc", "revol_bal", "revol_util", "term_1", "grade_1", "home_ownership_1", "verification_status_1", "purpose_1", "addr_state_1"),
      "Values" = list( list( 0, loan_amount_name, 0, 0, 600, NULL, NULL, ai_name, "Verified", NULL, NULL, "0",dti_name , emp_len_name, 15, open_acc_name, revol_bal_name, revol_util_name, 0, 0, 0, 0, 0, 0 ))
    )                ),
  GlobalParameters = setNames(fromJSON('{}'), character(0))
)

body = enc2utf8(toJSON(req))
api_key = "mM+TejUgJksVAAkqk0X8Q7wsyAZxPmMWcrYk2qa1zC47qUSnVJoMNEdV0TzGBSZzGTs19GxjdPmqlxsFmmq0/g==" # Replace this with the API key for the web service
authz_hdr = paste('Bearer', api_key, sep=' ')

h$reset()
curlPerform(url =  "https://ussouthcentral.services.azureml.net/workspaces/e7e077be891445fb8c7a748c7c7df031/services/ec03e62e4c8c4da284cf96cb2b4ca615/execute?api-version=2.0&details=true",
            httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
            postfields=body,
            writefunction = h$update,
            headerfunction = hdr$update,
            verbose = TRUE
)

headers = hdr$value()


print("Assigned Cluster:")
result = h$value()
resultpred <- fromJSON(result)
resultpred1 <- resultpred$Results$output1$value$Values[[1]][25]
return(resultpred1)

}

######################################## Prediction after clustering ###################################################


http_function_Cluster_1  <- function(loan_amount_name,dti_name,ai_name,open_acc_name,revol_bal_name,revol_util_name){
  
# Accept SSL certificates issued by public Certificate Authorities
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

h = basicTextGatherer()
hdr = basicHeaderGatherer()


req = list(
  
  Inputs = list(
    
    
    "input1" = list(
      "ColumnNames" = list("loan_amnt", "annual_inc", "dti", "delinq_2yrs", "inq_last_6mths", "open_acc", "revol_bal", "revol_util"),
      "Values" = list( list( loan_amount_name, ai_name, dti_name, 2, 5, open_acc_name, revol_bal_name, revol_util_name) )
    )                ),
  GlobalParameters = setNames(fromJSON('{}'), character(0))
)

body = enc2utf8(toJSON(req))
api_key = "uu/6eO/TvxUWZKOz0ZDZFIKJacLQVZaeTW/yRXJ0rknpJzInn4cWpQLmu3TDsmqH6ghGDsrFoT+RrJTe0EgmIQ==" # Replace this with the API key for the web service
authz_hdr = paste('Bearer', api_key, sep=' ')

h$reset()
curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/aba4b61d30814babbe06ae6b36e7a3ca/services/7d7550659cbd4f489de40800011102fc/execute?api-version=2.0&details=true",
            httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
            postfields=body,
            writefunction = h$update,
            headerfunction = hdr$update,
            verbose = TRUE
)

headers = hdr$value()


print("Result:")
result = h$value()
return(fromJSON(result))


}


########################################### NO CLUSTER #############################################################
http_function_prediction_NC  <- function(loan_amount_name,dti_name,sub_grade_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_util_name){
  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "input1" = list(
        "ColumnNames" = list("loan_amnt", "term", "sub_grade", "pub_rec", "home_ownership", "annual_inc", "verification_status", "purpose", "addr_state", "dti", "delinq_2yrs", "inq_last_6mths", "mths_since_last_delinq", "meanfico", "emp_length_clean", "open_acc", "revol_util"),
        "Values" = list( list( loan_amount_name, term_name, sub_grade_name, 1, ho_name,ai_name,"Verified",purpose_name,state_name, dti_name, 1, 0, 0, fico_name,emp_len_name,open_acc_name, revol_util_name  ) )
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "8GiuaxcgyMwcfm6OXnqAzIB8mBuQpZpSfonEz+RjWdTklp8Hng4j5TzvRroi1S/6ktGUVkcQoTLIXAHcjoMAvw=="# Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/549e3a96205f41fbac51955b386464a0/services/f7d551a7e511413089ea821ff45c2895/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  
  
  
  
  print("Interest Rate for non clustered is:")
  result = h$value()
  resultpred <- fromJSON(result)
  resultpred1 <- resultpred$Results$output1$value$Values[[1]][18]
  return(resultpred1)
}

##########################################################################################################################
###################################### WEB UI CODE ######################################################################
##########################################################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Lending Club Prediction"),
  
  textInput("LA", "Loan Amount", 500, placeholder = NULL),
  textInput("DTI", "DTI", 1, placeholder = NULL),
  textInput("STATE", "State", "NY", placeholder = NULL),
  textInput("Emp_Length", "Emp_Length", 10, placeholder = NULL),
  textInput("Grade", "Grade", "A", placeholder = NULL),
  textInput("TERM","TERM",36 , placeholder = NULL),
  textInput("HO","HOME OWNERSHIP", "ANY", placeholder = NULL),
  textInput("AI", "ANNUAL INCOME", 1000, placeholder = NULL),
  textInput("PURPOSE", "PURPOSE", "car", placeholder = NULL),
  textInput("FICO", "FICO SCORE", 400, placeholder = NULL),
  textInput("OPEN_ACCOUNT","OPEN ACCOUNT", 2, placeholder = NULL),
  textInput("REVOL_BAL","REVOLVING BALANCE", 2, placeholder = NULL),
  textInput("REVOL_UTIL","REVOLVING UTIL", 2, placeholder = NULL),
  textInput("SUB_GRADE","SUB_GRADE", "A1", placeholder = NULL),
  
  submitButton("Submit"),
  
  # Show a plot of the generated distribution
  mainPanel(
    textOutput("value_classification"),
    textOutput("db_select"),
    textOutput("Cluster_Output"),
    textOutput("Value_noclass"),
    textOutput("Value_cluster1")
    
  )
  
)


##########################################################################################################################
###################################### WEB SERVER CODE #####################################################################
##########################################################################################################################

# Normalize Function
normalize <-  function(x) {
  log(x)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #output$LA <-renderPrint({input$LA})
  
  output$value_classification <-renderPrint({http_function_classification(input$LA,input$DTI,input$STATE,input$Emp_Length,input$Grade)})
  
  output$Value_noclass <-renderPrint({http_function_prediction_NC(input$LA,input$DTI,input$SUB_GRADE,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_UTIL)})
  
  
  
  manualcluster <- reactive({ 
    if(input$Grade == 'A') {
      test <- http_function_prediction_A(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)
    } else if (input$Grade == 'B') {
      test <- http_function_prediction_B(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)
    } else if (input$Grade == 'C') {
      test <- http_function_prediction_C(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)
    } else if (input$Grade == 'D') {
      test <- http_function_prediction_D(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)
    } else if (input$Grade == 'E') {
      test <- http_function_prediction_E(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)
    } else if (input$Grade == 'F') {
      test <- http_function_prediction_F(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)
    } else if (input$Grade == 'G'){
      test <- http_function_prediction_G(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)
    }
  })
  
  output$db_select <- renderText(manualcluster())
  
  
  output$Cluster_Output <-renderPrint({http_function_KMEANS(input$LA,input$DTI,input$AI,input$OPEN_ACCOUNT,input$REVOL_BAL,input$REVOL_UTIL,input$Emp_Length)})
  output$Value_cluster1 <- renderPrint({http_function_Cluster_1(normalize(as.numeric(input$LA)),normalize(as.numeric(input$DTI)),normalize(as.numeric(input$AI)),normalize(as.numeric(input$OPEN_ACCOUNT)),normalize(as.numeric(input$REVOL_BAL)),normalize(as.numeric(input$REVOL_UTIL)))})
}

# Run the application 
shinyApp(ui = ui, server = server)