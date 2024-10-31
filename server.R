# MetaImpact Server


#----------------#
# Server Content #
#----------------#
function(input, output, session) {
  
  home_page_server(id = "home")
  
  
  ### Walk-through demonstration ###
  #------------------------------#
  
  walk_through_page_server(id = "walk_through")
  
  
  ### Load and present Data ###
    #-----------------------#
  
  data <- data_page_server(id = "data")
  
  
  ### Calculator Functionality ###
    #--------------------------#
  
  calculator_page_server(id = "calculator",
                         data = data)

}
