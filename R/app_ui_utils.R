
# Columns wrappers
# 
# These are convenient wrappers around 
# `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
# 
# @export
# @rdname columns
col_12 <- function(...){
  column(12, ...)
}

col_10 <- function(...){
  column(10, ...)
}

col_8 <- function(...){
  div(class = "col-xs-12 col-sm-12 col-md-8", ...)
}

col_6 <- function(...){
  div(class = "col-xs-12 col-sm-12 col-md-6", ...)
}

col_5 <- function(...){
  column(5, ...)
}

col_4 <- function(...){
  div(class = "col-xs-12 col-sm-12 col-md-4", ...)
}

col_3 <- function(...){
  column(3, ...)
}

col_2 <- function(...){
  column(2, ...)
}

col_1 <- function(...){
  column(1, ...)
}



tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    text-align: left;
    padding-left: 2px; 
    padding-right: 2px; 
    color: rgba(85, 85, 85);
    font-size: 12px;
    font-family: Arial;
    background: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    border-radius: 5px;
  }
"))
