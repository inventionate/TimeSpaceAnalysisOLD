#' Load data frame form a SQLite database.
#'
#' @param remote_db_path path to the remote database (SQLite).
#' @param copy copy bring data into an R (boolean).
#' @param convert_encoding convert data encoding. Windows only. copy have to be TRUE  (boolean).
#'
#' @return The SQLite data tables.
#' @export
db_load <- function(remote_db_path, copy = FALSE, convert_encoding = FALSE) {
  # Vorhandene Datenbank löschen
  if (file.exists(remote_db_path))
  {
    # Datenbank lesen
    remote_db <- dplyr::src_sqlite(remote_db_path, create = FALSE)

    # Tabellen auslesen
    remote_tables <- dplyr::src_tbls(remote_db)

    # Tabellen lokal verfügbar machen
    for (n in 1:length(remote_tables)) {

      df_db <- dplyr::tbl(remote_db, remote_tables[n])

      if(copy) {

        df_copy <- dplyr::collect(df_db)

        if(convert_encoding) {
          # Kodierung der Katgeorien für Windows anpassen
          df_enc <- mutate_each(df_copy, funs(iconv(., "UTF-8", "cp1250")))
          # Kodierung der Variablennamen anpassen
          names(df_enc) <- iconv(names(df_enc), "UTF-8", "cp1250")
          # Kodierten Datensatz speichern
          assign(iconv(remote_tables[n], "UTF-8", "cp1250"), df_enc, envir = globalenv())
        } else {
          assign(remote_tables[n], df_copy, envir = globalenv())
        }
      } else {
        assign(remote_tables[n], df_db, envir = globalenv())
      }
    }
  }
  else
  {
    stop("ERROR: Datenbank nicht gefunden")
  }
  return(print("Datenbank erfolgreich geladen"))
}
