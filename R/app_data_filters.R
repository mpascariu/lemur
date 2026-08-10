# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun Oct 19 21:08:03 2025
# ------------------------------------------------- #

# Read a database setting from an environment variable, falling back to a
# default when the variable is unset or empty. Every setting except the
# password has a default so local development works out of the box; production
# values are supplied via the deployment environment (see .env.example).
#' @noRd
db_setting <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) default else value
}

# Create a global connection pool (to be reused across calls)
#' @keywords internal
create_db_pool <- function(run_db = TRUE) {
  if (!run_db) {
    # Return NULL or a dummy pool when running locally, no DB connection needed
    return(NULL)
  }

  # The password deliberately has no default: if serverMode = TRUE the app
  # must fail loudly rather than silently connect with wrong credentials.
  password <- db_setting("LEMUR_DB_PASSWORD", "")
  if (!nzchar(password)) {
    stop(
      "serverMode = TRUE but no database password is configured.\n",
      "Set the environment variable LEMUR_DB_PASSWORD before starting the app ",
      "(see .env.example at the repo root)."
    )
  }

  pool <- pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = db_setting("LEMUR_DB_HOST", "postgres"),
    dbname   = db_setting("LEMUR_DB_NAME", "gbd2021"),
    user     = db_setting("LEMUR_DB_USER", "lemur"),
    password = password,
    port     = as.integer(db_setting("LEMUR_DB_PORT", "5432")),
    minSize  = 2,
    maxSize  = 20
  )

  return(pool)
}


#' Filter dataset using data.table methods
#' This is used when the app runs in local mode in R
#' @keywords internal
dt_filter_local <- function(data, mode, region1, region2, gender, year, db_pool) {
  
  if(mode != "mode_cntr") region2 <- region1
  
  # we use data.table method to filter here because is faster
  # and we will do this all a lot
  p  <- db_pool
  # The app pre-converts the static datasets to data.table once at startup;
  # skip the conversion here when that's already the case.
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }
  dt <- data[period == year]
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
  
  if(mode != "mode_cntr") region2 <- region1
  
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

