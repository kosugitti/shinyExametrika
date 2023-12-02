tabItem_About <- tabItem("tab_About",
                         h2("Welcome to Shiny Exametrika!"),
                         h4("The Exametrika package is designed for test data engineering and corresponds to the text by Shojima (2022). "),
                         tags$hr(),
                         h3("Reference"),
                         h4(tags$a(href = "https://amzn.to/42eNArd", "Shojima, Kojiro (2022)Test Data Engineering,Springer")),
                         h4(tags$a(href = "http://shojima.starfree.jp/tde/", "Accompanying Website for Test Data Engineering")),
                         h4(tags$a(href = "https://kosugitti.github.io/Exametrika/", "Exametrika(R pacage)"))
)
