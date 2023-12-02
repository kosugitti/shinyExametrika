shinyUI(
    navbarPage(
    "Shiny Exametrika",
    tabPanel(
      "Settings",
      h2("Welcome to Shiny Exametrika!"),
      h4("The Exametrika package is designed for test data engineering and corresponds to the text by Shojima (2022). "),
      tags$hr(),
      fileInput("file", label = h3("Input Data File")),
      textInput("mi",label = h5("Missing Value"),value = ""),
      numericInput("itemLabel",label = h5("Item Label Row #"),value = 0,min=0),
      numericInput("idLabel",label = h5("Student ID Col #"),value = 0,min=0),
      tags$hr(),
      h3("Reference"),
      h4(tags$a(href = "https://amzn.to/42eNArd", "Shojima, Kojiro (2022)Test Data Engineering,Springer")),
      h4(tags$a(href = "http://shojima.starfree.jp/tde/", "Accompanying Website for Test Data Engineering")),
      h4(tags$a(href = "https://kosugitti.github.io/Exametrika/", "Exametrika(R pacage)"))
    ),
    tabPanel(
      "Check the settings",
    ),
    tabPanel(
      "Analysis"
    )
  )
)
