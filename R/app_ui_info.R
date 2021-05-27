# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu May 27 08:23:04 2021
# --------------------------------------------------- #

#' @keywords internal
mod_info_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 8, 
        offset = 2,
        tabsetPanel(
          tabPanel(
            "About", 
            includeMarkdown(
              system.file('app/www/doc_about.md', 
                          package = 'MortalityCauses')
            )
          ),
          tabPanel(
            "Sources", 
            includeMarkdown(
              system.file('app/www/doc_sources.md', 
                          package = 'MortalityCauses')
            )
          ),
          tabPanel(
            "Methods Protocol", 
            includeMarkdown(
              system.file('app/www/doc_methods.md', 
                          package = 'MortalityCauses')
            )
          ),
          tabPanel(
            "Contact", 
            includeMarkdown(
              system.file('app/www/doc_contact.md', 
                          package = 'MortalityCauses')
            )
          )
        )
      )
    )
  )
}