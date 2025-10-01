# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Wed Oct  1 20:54:58 2025
# ------------------------------------------------- #

# Create a global connection pool (to be reused across calls)
#' @keywords internal
create_db_pool <- function(run_db = TRUE) {
  if (!run_db) {
    # Return NULL or a dummy pool when running locally, no DB connection needed
    return(NULL)
  }
  
  pool <- pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = "postgres",
    dbname   = "gbd2021",
    user     = "lemur",
    password = "tx*Oj3HjwAlNbNY0XrY3288E#", # yeah, i know...
    port     = 5432,
    minSize  = 2,
    maxSize  = 20
  )
  
  return(pool)
}


#' Filter dataset using data.table methods
#' This is used when the app runs in local mode in R
#' @keywords internal
dt_filter_local <- function(data, mode, region1, region2, gender, year, db_pool) {
  
  # we use data.table method to filter here because is faster
  # and we will do this all a lot
  p  <- db_pool
  dt <- as.data.table(data)
  dt <- dt[period == year]
  dt <- dt[region %in% c(region1, region2)]
  
  if (mode != "mode_sex") {
    dt <- dt[sex == gender]
  }
  
  return(as_tibble(dt))
}



#' Query data from a PostgresSQL. 
#' This would replace the local data and the dt_filter_local() function
#' @keywords internal
dt_filter_sql <- function(data, mode, region1, region2, gender, year, db_pool) {
  # Build base query
  query <- paste0(
    "SELECT * FROM ", DBI::dbQuoteIdentifier(db_pool, data),
    " WHERE period = $1 AND region IN ($2, $3)"
  )
  params <- list(year, region1, region2)
  
  # Add gender filter if needed
  if (mode != "mode_sex") {
    query <- paste0(query, " AND sex = $4")
    params <- c(params, list(gender))
  }
  
  tryCatch({
    res <- DBI::dbGetQuery(db_pool, query, params = params)
    return(tibble::as_tibble(res))
    
  }, error = function(e) {
    message("Database query error: ", e$message)
    return(tibble::tibble())
  })
}

