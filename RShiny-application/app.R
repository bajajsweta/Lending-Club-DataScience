#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
  
}


########################################### B ############################################################################

http_function_prediction_B  <- function(loan_amount_name,dti_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_bal_name,revol_util_name){
  
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
  httpStatus = headers["status"]
  if (httpStatus >= 400)
  {
    print(paste("The request failed with status code:", httpStatus, sep=" "))
    
    # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
    print(headers)
  }
  
  print("Interest Rate is:")
  result = h$value()
  resultpred <- fromJSON(result)
  resultpred1 <- resultpred$Results$output1$value$Values[[1]][19]
  return(resultpred1)

}


########################################### D ############################################################################

http_function_prediction_D  <- function(loan_amount_name,dti_name,state_name,emp_len_name,term_name,ho_name,ai_name,purpose_name,fico_name,open_acc_name,revol_bal_name,revol_util_name){
  
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
        "Values" = list( list( loan_amount_name, term_name , 200 , ho_name , ai_name, "Verified", "2017-04-14", purpose_name, state_name, emp_len_name, dti_name, fico_name, 1, 0, open_acc_name, revol_bal_name, revol_util_name, 2 ))
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "1MI5hQSHXfFzhVWaD3v3Jhvg3k6kVTs3s2/X/t+HYWljSNa8rQzp/hZuqRMFJpzIh3Dg+550QMggqgitFfSUQQ==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/549e3a96205f41fbac51955b386464a0/services/6697bac913654d64a96b5e3ae4e4e70c/execute?api-version=2.0&details=true",
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
   textInput("Grade", "Grade", "B", placeholder = NULL),
   textInput("TERM","TERM",36 , placeholder = NULL),
   textInput("HO","HOME OWNERSHIP", "ANY", placeholder = NULL),
   textInput("AI", "ANNUAL INCOME", 1000, placeholder = NULL),
   textInput("PURPOSE", "PURPOSE", "car", placeholder = NULL),
   textInput("FICO", "FICO SCORE", 400, placeholder = NULL),
   textInput("OPEN_ACCOUNT","OPEN ACCOUNT", 2, placeholder = NULL),
   textInput("REVOL_BAL","REVOLVING BALANCE", 2, placeholder = NULL),
   textInput("REVOL_UTIL","REVOLVING UTIL", 2, placeholder = NULL),

   submitButton("Submit"),
   
   # Show a plot of the generated distribution
   mainPanel(
     textOutput("value_classification"),
     textOutput("value_predictionA"),
     textOutput("value_predictionB"),
     textOutput("value_predictionC"),
     textOutput("value_predictionD"),
     textOutput("value_predictionE"),
     textOutput("value_predictionF"),
     textOutput("value_predictionG")
     
   )
    
)


##########################################################################################################################
###################################### WEB SERVER CODE #####################################################################
##########################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #output$LA <-renderPrint({input$LA})
  
  output$value_classification <-renderPrint({http_function_classification(input$LA,input$DTI,input$STATE,input$Emp_Length,input$Grade)})
 
  if(input$Grade == 'A') {
  output$value_predictionA <- renderPrint({http_function_prediction_A(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)})
  } else if (input$Grade == 'B') {
  output$value_predictionB <- renderPrint({http_function_prediction_B(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)})
  } else if (input$Grade == 'C') {
  output$value_predictionC <- renderPrint({http_function_prediction_C(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)})
  } else if (input$Grade == 'D') {
  output$value_predictionD <- renderPrint({http_function_prediction_D(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)})
  } else if (input$Grade == 'E') {
  output$value_predictionE <- renderPrint({http_function_prediction_E(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)})
  } else if (input$Grade == 'F') {
  output$value_predictionF <- renderPrint({http_function_prediction_F(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)})
  } else {
  output$value_predictionG <- renderPrint({http_function_prediction_G(input$LA,input$DTI,input$STATE,input$Emp_Length,input$TERM, input$HO, input$AI, input$PURPOSE, input$FICO, input$OPEN_ACCOUNT, input$REVOL_BAL, input$REVOL_UTIL)})
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)

