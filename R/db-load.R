#' Save data frame as SQLite table.
#'
#' @param remote_db_path path to the remote database (SQLite).
#' @param copy copy bring data into an R (boolean).
#'
#' @return The SQLite database.
#' @export
#'
#' @examples
db_load <- function(remote_db_path, copy = TRUE) {
  # Vorhandene Datenbank löschen
  if (file.exists(remote_db_path))
  {
    # Datenbank lesen
    remote_db <- dplyr::src_sqlite(remote_db_path, create = FALSE)
    # Tabellen auslesen
    remote_tables <- dplyr::src_tbls(remote_db)
    # Tabellen lokal verfügbar machen
    for (n in 1:length(remote_tables)) {
      if(copy) assign(remote_tables[n], dplyr::collect(dplyr::tbl(remote_db, remote_tables[n])), envir = globalenv())
      else assign(remote_tables[n], dplyr::tbl(remote_db, remote_tables[n]), envir = globalenv())
    }
  }
  else
  {
    stop("ERROR: Datenbank nicht gefunden")
  }
  return(print("Datenbank erfolgreich geladen"))
}
