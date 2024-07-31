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
    shinyjs::useShinyjs(),
    shiny::navbarPage(
      # add PHS logo to navigation bar
      title = div(style = "position: relative;
                       top: -15px;
                       margin-left: 10px;
                       margin-top: 5px;",
                  tags$a(img(src = "www/videohubheader.png",
                             width = 120,
                             alt = "link to AP VideoHub"),
                         href = "https://apvideohub.ap.org/",
                         target = "_blank")
      ),


      # make navigation bar collapse on smaller screens
      collapsible = TRUE,
      windowTitle = 'Test',
      header = tags$head(

        # include scotpho icon in web browser
        HTML("<html lang='en'>"),
        tags$link(rel = "shortcut icon",
                  href = "favicon.ico"),
        # include google analytics scripts
        HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=G-KE1C59RLNS"></script>'),

      ),


      # order of tabs --------------------------------
      mod_homePage_ui("homePage_1")
      , mod_summary_ui("summary_1")
      , mod_dl_trend_ui("dl_trend_1")
      , tabPanel(div(
            div(class="fa fa-percent", role = "navigation"),
            "Ratios"),
            value = "ratio", mod_upload_ratio_ui("upload_ratio_1"))
      , tabPanel(div(
            div(class="fa fa-pie-chart", role = "navigation"),
            "Categories"),
            value = "sunburst", mod_sunburst_ui("sunburst_1"))
      , mod_map_ui("map_1")
      , tabPanel(
        div(
          div(class="fa-solid fa-magnifying-glass-chart", role = "navigation"),
          'Data'),
        value = "map", mod_dl_data_ui("dl_data_1")
      )
      , mod_top_stories_ui("top_stories_1")
      # summaryTab,
      # trendTab,
      # rankTab,
      # inequalitiesTab,
      # , navbarMenu(tabPanel("Info"),
      #            tabPanel("About"),
      #            tabPanel("Submit")))

    ) # close navbarPage
  ) # close taglist
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
      app_title = "AP Video Hub HQ",
      path = app_sys("app/www")
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
