# Libraries needed
library(tibble)   # for tibble_row
library(dplyr)    # for bind_rows
library(readr)    # for read_lines + encoding

# Load & convert lines
legend_lines <- read_lines("data/Biome_Inventory_Legends.txt", locale = locale(encoding = "latin1"))

# Build rows
wide_rows <- list()

for (line in legend_lines) {
  match <- regexec("Biome Inventory layer (\\d{2}) based on (.*?):\\s*(.*)", line)
  parsed <- regmatches(line, match)[[1]]
  
  if (length(parsed) == 4) {
    layer <- as.integer(parsed[2])
    source <- parsed[3]
    biome_entries <- strsplit(parsed[4], ";\\s*")[[1]]
    
    biome_vector <- sapply(biome_entries, function(x) {
      parts <- strsplit(x, ",\\s*", fixed = FALSE)[[1]]
      if (length(parts) >= 2) {
        biome <- paste(parts[-1], collapse = ", ")
        return(biome)
      } else {
        return(NA_character_)
      }
    })
    
    names(biome_vector) <- paste0("id_", seq_along(biome_vector))
    row <- tibble_row(layer = layer, source = source, !!!as.list(biome_vector))  # !!! for unsplicing
    wide_rows[[length(wide_rows) + 1]] <- row
  }
}

# Combine into a data.frame
legend_wide_df <- bind_rows(wide_rows)

#save
saveRDS(legend_wide_df, file = "data/biome_legend.rds")
usethis::use_data(legend_wide_df, internal = TRUE)

library(writexl)

# Speichern als Excel-Datei
write_xlsx(legend_wide_df, path = "data/biome_legend.xlsx")
