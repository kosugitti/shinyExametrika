pacman::p_load(shinydashboard)
source('ui_about.R', local = TRUE)
source('ui_input.R', local = TRUE)
source('ui_skelton.R', local = TRUE)

dashboardPage(
    dashboardHeader(
        title = "Shiny Exametrika",

        dropdownMenu(type = "tasks", badgeStatus = "success",
                     taskItem(value = 90, color = "green",
                              "Documentation"
                     ),
                     taskItem(value = 17, color = "aqua",
                              "Project X"
                     ),
                     taskItem(value = 75, color = "yellow",
                              "Server deployment"
                     ),
                     taskItem(value = 80, color = "red",
                              "Overall project"
                     )
        )
    ),


    dashboardSidebar(
        sidebarMenu(
            menuItem("About", icon=icon("info"), tabName = "tab_About"),
            menuItem("Input", icon=icon("cogs"), tabName = "tab_Input"),
            menuItem("Data Check", icon=icon("cogs"), tabName = "tab_About"),
            menuItem("IRT models", icon=icon("line-chart"),
                     menuSubItem("2plmodel", tabName = "tab_CPU"),
                     menuSubItem("3plmodel", tabName = "tab_CPU"),
                     menuSubItem("4plmodel", tabName = "tab_CPU")
            ),
            menuItem("Latent Class Analysis", icon=icon("line-chart"), tabName = "tab_CPU"),
            menuItem("Latent Rank Analysis", icon=icon("line-chart"), tabName = "tab_CPU"),
            menuItem("Biclustering", icon=icon("line-chart"),
                     menuSubItem("Biclustering", tabName = "tab_CPU"),
                     menuSubItem("Ranklustering", tabName = "tab_CPU")
            ),
            menuItem("Bayesian Network Model", icon=icon("line-chart"), tabName = "tab_CPU"),
            menuItem("Local Dependent LRA", icon=icon("line-chart"), tabName = "tab_CPU"),
            menuItem("Local Dependence Biclustring", icon=icon("line-chart"), tabName = "tab_CPU"),
            menuItem("Bicluster Network Model", icon=icon("line-chart"), tabName = "tab_CPU")
        )
    ),


    dashboardBody(
        tabItems(
            tabItem_About,
            tabItem_Input,
            tabItem_skelton
        )
    ),



    skin="green"
)
