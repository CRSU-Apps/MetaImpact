# MetaImpact UI

fluidPage(
  useShinyjs(),
  includeCSS(path = "www/app.css"),
  rintrojs::introjsUI(),
  navbarPage(
    id = "MetaImpact",
    title = "MetaImpact",
    theme = shinytheme(theme = "readable"),
    
    # Home Tab
    tabPanel(
      title = "Home",
      home_page_ui(id = "home")
    ),
    
    # Walk-Through
    tabPanel(
      title = "Walk-Through",
      walk_through_section_ui(id = "walk_through")
    ),

    # Data Tab
    tabPanel(
      title = "Data",
      data_page_ui(id = "data")
    ),

    # Evidence Synthesis Tab
    tabPanel(
      title = "Calculator",
      calculator_page_ui(id = "calculator")
    )
  )
)
