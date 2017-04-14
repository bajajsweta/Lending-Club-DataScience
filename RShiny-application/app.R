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
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

http_function  <- function(a,b,c,d,e){
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
  return(fromJSON(result))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Lending Club Prediction"),
   
   textInput("LA", "Loan Amount", 0, placeholder = NULL),
   textInput("DTI", "DTI", 0, placeholder = NULL),
   textInput("STATE", "State", "NY", placeholder = NULL),
   textInput("Emp_Length", "Emp_Length", 10, placeholder = NULL),
   textInput("Grade", "Grade", "B", placeholder = NULL),
   submitButton("Submit"),
   
   # Show a plot of the generated distribution
   mainPanel(
     textOutput("value")
   )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #output$LA <-renderPrint({input$LA})
  
  output$value <-renderPrint({http_function(input$LA,input$DTI,input$STATE,input$Emp_Length,input$Grade)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

