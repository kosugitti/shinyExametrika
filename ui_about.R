tabItem_About <- tabItem("tab_About",
                         h2("Welcome to Shiny Exametrika!"),
                         h4("The Exametrika package is designed for test data engineering and corresponds to the text by Shojima (2022). "),
                         tags$ul(
                             tags$li("Test data engineering involves analyzing test response patterns to classify item difficulty and respondent ranking."),
                             tags$ul(
                                 tags$li("Classical Test Theory"),
                                 tags$li("Item response theory: 2PL, 3PL, 4PL model"),
                                 tags$li("Latent Class Analysis"),
                                 tags$li("Latent Rank Analysis"),
                                 tags$li("Biclustering and Ranklustering"),
                                 tags$li("Infinite Relational Model for optimal number of classes and fields"),
                                 tags$li("Bayesian Network Analysis"),
                                 tags$li("Structure Learning for Bayesian Network Analysis by Genetic Algorithm"),
                                 tags$li("Local Dependence Latent Rank Analysis"),
                                 tags$li("Structure Learning for LDLRA by PBIL"),
                                 tags$li("Local Dependence Biclustering"),
                                 tags$li("Biclister Network Model")
                             ),
                             tags$li("Exametrika is originally implemented and published as a Mathematica and Excel Add-in. Please refer to the following website for more information.")
                         ),
                         tags$hr(),
                         h3("Reference"),
                         h4(tags$a(href = "https://amzn.to/42eNArd", "Shojima, Kojiro (2022)Test Data Engineering,Springer")),
                         h4(tags$a(href = "http://shojima.starfree.jp/tde/", "Accompanying Website for Test Data Engineering")),
                         h4(tags$a(href = "https://kosugitti.github.io/Exametrika/", "Exametrika(R pacage)"))
)
