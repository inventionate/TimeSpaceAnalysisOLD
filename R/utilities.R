# Eine Sammlung hilfreicher Funktionen
.count_distinct_ind <- function(res_gda, axes = 1:2) {
  # Koordinaten der Individuen vorbereiten
  # Punkte an den gleichen Stellen vergößern und multiple entfernen.
  res_gda <- data_frame(Dim.1 = res_gda$ind$coord[, axes[1]], Dim.2 = res_gda$ind$coord[, axes[2]]) %>%
    group_by(Dim.1, Dim.2) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    as.data.frame()
}
