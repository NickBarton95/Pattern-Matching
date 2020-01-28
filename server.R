server <- function(input, output, session){
  
  
  callModule(PatternMatching, "test3", reactive({data}))
  
 
}

