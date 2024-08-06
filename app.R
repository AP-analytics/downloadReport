# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
downloadReport::run_app(scr = 'lana', eAP = c('46865', '46866'),
                        prod_num = c('100973', '100974'),
                        title = "Latin America News Agency") # add parameters here (if any)
