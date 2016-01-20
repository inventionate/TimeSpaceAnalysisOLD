#' Saves data into a SQLite database.
#'
#' @param remote_db_path
#' @param source_df
#'
#' @return SQLite database file.
#' @export
#'
#' @examples
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
