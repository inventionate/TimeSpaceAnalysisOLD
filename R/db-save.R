# db_save Funktion Bereinigte und
# sortierte Daten in eine remote
# Datenbank speichern.  remote_db_path:
# (string) Dateipfad zur SQLite
# Datenbank.  source_df: (dplyr)
# data_frame, tbl, die als
# Quelldatenbank dient.
# Copyright © 2015 Fabian Mundt


# Funktion
db_save <- function(remote_db_path, source_df) {
    # Vorhandene Datenbank löschen
    if (file.exists(remote_db_path)) {
        file.remove(remote_db_path)
    }
    # Datenbank neu erzeugen
    remote_db <- src_sqlite(remote_db_path,
        create = TRUE)
    # Datenbank befüllen
    for (n in 1:length(source_df)) {
        copy_to(remote_db, source_df[[n]],
            name = names(source_df[n]),
            temporary = FALSE)
    }
    return(print("Datenbank erfolgreich gespeichert."))
}
