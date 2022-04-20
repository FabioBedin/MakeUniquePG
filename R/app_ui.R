#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::bs4DashPage(
      dark = TRUE,
      header = bs4Dash::bs4DashNavbar(
        title = bs4Dash::bs4DashBrand(
          title = "PG Cleaner",
          color = "warning",
          href = "https://github.com/FabioBedin",
          image = NULL
        )
      ),
      sidebar = bs4Dash::bs4DashSidebar(
        skin = "dark",
        status = "primary",
        id = "SidebarStatus",
        bs4Dash::bs4SidebarMenu(
          id = "SidebarMenu",
          bs4Dash::bs4SidebarMenuItem(
            text = "Remove Dups",
            tabName = "RemoveDuplicates",
            icon = icon("clone")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Visualize Dups",
            tabName = "VisualizeDuplicates",
            icon = icon("chart-bar")
          )
        )
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::bs4TabItems(
          bs4Dash::tabItem(
            tabName = "RemoveDuplicates",
            fluidRow(
              bs4Dash::bs4ValueBoxOutput("rows", width = 4),
              bs4Dash::bs4ValueBoxOutput("duplicate", width = 4),
              bs4Dash::bs4ValueBoxOutput("empty", width = 4)
            ),
            fluidRow(
              bs4Dash::bs4Card(
                title = "Gene names before",
                status = "warning",
                solidHeader = FALSE,
                background = NULL,
                width = 6,
                collapsible = TRUE,
                collapsed = FALSE,
                closable = FALSE,
                maximizable = TRUE,
                gradient = FALSE,
                headerBorder = TRUE,
                id = "before",
                DT::dataTableOutput("before")
              ),
              bs4Dash::bs4Card(
                title = "Gene names after",
                status = "warning",
                solidHeader = FALSE,
                background = NULL,
                width = 6,
                collapsible = TRUE,
                collapsed = FALSE,
                closable = FALSE,
                maximizable = TRUE,
                gradient = FALSE,
                headerBorder = TRUE,
                id = "after",
                DT::dataTableOutput("after")
              )
            )
          ),
          bs4Dash::tabItem(tabName = "VisualizeDuplicates", fluidRow(
            bs4Dash::bs4Card(
              title = "Select Gene",
              footer = NULL,
              status = "warning",
              solidHeader = FALSE,
              background = NULL,
              width = 3,
              height = "440px",
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              maximizable = TRUE,
              gradient = FALSE,
              boxToolSize = "sm",
              elevation = NULL,
              headerBorder = TRUE,
              label = NULL,
              dropdownMenu = NULL,
              sidebar = NULL,
              id = "duplicate_plot",
              htmlOutput("select_dupe")
            ),
            bs4Dash::bs4Card(
              title = "Visualization of duplicate",
              footer = NULL,
              status = "warning",
              solidHeader = FALSE,
              background = NULL,
              width = 9,
              height = NULL,
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              maximizable = TRUE,
              gradient = FALSE,
              boxToolSize = "sm",
              elevation = NULL,
              headerBorder = TRUE,
              label = NULL,
              dropdownMenu = NULL,
              sidebar = NULL,
              id = "duplicate_plot",
              echarts4r::echarts4rOutput("duplicatePlot")
            )
          ))
        )),
      controlbar = bs4Dash::bs4DashControlbar(
        overlay = F,
        width = 300,
        collapsed = FALSE,
        pinned = TRUE,
        style = "padding:1.5em;",
        # fileInput("LoadFromFile", "Load ProteinGroups.txt:", accept = ".txt"),
        h4("Download new PG file:", style = "display: flex; justify-content: center;"),
        downloadButton("DownloadUpdatePG", "Download")
      ),
      footer = bs4Dash::dashboardFooter(left = "Buit by Fabio Bedin.", right = "fabio.bedin@ieo.it")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MakeUniquePG"
    )

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}
