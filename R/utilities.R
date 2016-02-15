# Eine Sammlung hilfreicher Funktionen
.count_distinct_ind <- function(res_gda) {
  # Koordinaten der Individuen vorbereiten
  # Punkte an den gleichen Stellen vergößern und multiple entfernen.
  res_gda <- data.frame(res_gda$ind$coord[, 1:2]) %>%
    group_by(Dim.1, Dim.2) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    as.data.frame()
}
