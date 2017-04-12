# Install the latest version of the `typeformR` package
#devtools::install_github('anthonypileggi/typeformR')

# Run 'Typeform Data Explorer'
# - approach 1
#typeformR::runShinyApp('data_explorer')
# - approach 2
dir <- system.file("shiny-apps/data_explorer", package = "typeformR")
setwd(dir)
shiny::shinyAppDir(".")