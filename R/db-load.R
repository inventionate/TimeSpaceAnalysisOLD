# db_load Funktion Bereinigte und
# sortierte Daten in eine remote
# Datenbank speichern.  remote_db_path:
# (string) Dateipfad zur SQLite
# Datenbank.  source_df: (dplyr)
# data_frame, tbl, die als
# Quelldatenbank dient.
# Copyright © 2015 Fabian Mundt

# Funktion
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
