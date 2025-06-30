#' Count GBIF Occurrences by Biome Layer
#'
#' Downloads GBIF species occurrence records using the GBIF API, optionally cleans coordinates,
#' and counts occurrences per biome class in a specified raster layer.
#'
#' @param species Scientific species name (e.g. "Panthera leo").
#' @param username GBIF username.
#' @param pwd GBIF password.
#' @param email Email associated with the GBIF account.
#' @param raster_path Path to the biome raster stack (GeoTIFF).
#' @param layer_index Index of the raster layer to use for counting.
#' @param save_dir_GBIF Directory to save downloaded GBIF ZIP file.
#' @param save_dir_results Directory to save result CSV file.
#' @param filter_clean Logical; if TRUE, cleans coordinates using `CoordinateCleaner`.
#' @param limit Optional integer; maximum number of occurrences to download.
#' @param legend_table Optional data frame linking biome IDs to biome names.
#'
#' @return A data frame with biome IDs, names (if provided), and occurrence counts.
#' @export
biomer_GBIF_count <- function(...) {
  stop("This function is not yet implemented.")
}




# ---------------------------------------------
# Funktion: biomer_GBIF_count
# Ziel: Lädt Fundpunkte einer Art automatisch von GBIF über occ_download()
#       und zählt, wie viele dieser Punkte in welchem Biom-Layer vorkommen.
# ---------------------------------------------

# Data:

# 1. data/biome_legend.rds
  # Colums: layer source   id_1  id_2  id_3  id_4 (each id is one biom)

# 2. Biome Raster: data/Biome_Inventory_RasterStack.tif"


# Benötigte Eingaben:
# - species: Wissenschaftlicher Artname als String
# - username: GBIF-Benutzername
# - pwd: GBIF-Passwort
# - email: Mit dem GBIF-Konto verknüpfte E-Mail-Adresse
# - limit (Optional) - default is all
# - raster_path: Pfad zur .tif-Datei mit dem Biome-Rasterstack (immer: "data/Biome_Inventory_RasterStack.tif")
# - layer_index: Index des Rasterlayers, in dem gezählt werden soll
# - save_dir_GBIF: Speicherort für den GBIF-Download (ZIP)
# - save_dir_results: Speicherort für CSV-Datei
# - filter_clean: TRUE/FALSE – ob Fundpunkte mit CoordinateCleaner bereinigt werden sollen
# - limit (Optional) - default is all
# - legend_table: (optional) Dataframe mit Biome-ID und Biome-Namen zur besseren Zuordnung data/biome_legend.rds)

# Ablauf:
# 1. GBIF taxonKey für die angegebene Art abrufen
# 2. GBIF-Download über occ_download() starten (mit Login)
# 3. Warten, bis der Download auf GBIF abgeschlossen ist
# 4. ZIP-Datei herunterladen und entpacken
# 5. occurrence.txt einlesen (Koordinaten, ggf. weitere Felder)
# 6. Optional: Bereinigung der Koordinaten mit CoordinateCleaner
# 7. Koordinaten in SpatVector umwandeln und an CRS des Biome-Layers anpassen
# 8. Vorkommen je Biom zählen
# 9. Biome-Namen aus legend_table zuordnen
# 10. Ergebnis als CSV speichern und als Dataframe zurückgeben
# 11. Visualisierung (z.B. Balkendiagramm)

# Hinweis:
# - occ_download() ist asynchron → occ_download_wait() wird verwendet
# - Die Funktion benötigt Internetverbindung und gültige GBIF-Zugangsdaten
