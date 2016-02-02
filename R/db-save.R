#' Saves data into a SQLite database.
#'
#' @param remote_db_path path to the remote database (SQLite).
#' @param source_df source data frame (should be a dplyr tbl object).
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
    remote_db <- dplyr::src_sqlite(remote_db_path,
        create = TRUE)
    # Datenbank befüllen
    for (n in 1:length(source_df)) {
        dplyr::copy_to(remote_db, source_df[[n]],
            name = names(source_df[n]),
            temporary = FALSE)
    }
    return(print("Datenbank erfolgreich gespeichert."))
}
