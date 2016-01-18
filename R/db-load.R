#' Save data frame as SQLite table.
#'
#' @param remote_db_path
#'
#' @return The SQLite database.
#' @export
#'
#' @examples
#' db_load('/path/to/database.sqlite')
db_load <- function(remote_db_path) {
  # Vorhandene Datenbank löschen
  if (file.exists(remote_db_path))
  {
    # Datenbank lesen
    remote_db <- src_sqlite(remote_db_path, create = FALSE)
    # Tabellen auslesen
    remote_tables <- src_tbls(remote_db)
    # Tabellen lokal verfügbar machen
    for (n in 1:length(remote_tables)) {
      assign(remote_tables[n], tbl(remote_db, remote_tables[n]), envir = globalenv())
    }
  }
  else
  {
    stop("ERROR: Datenbank nicht gefunden")
  }
  return(print("Datenbank erfolgreich geladen"))
}
