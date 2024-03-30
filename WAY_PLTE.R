# Pengolahan WAY PLTE untuk lelang SBSN
# developed by Elva n Ali
# Jakarta, 15/03/2024

## ------------------------------------------------------------

library(readxl) # karena csv valuesnya berubah - kita pake excel nya - package ini lebih mudah
library(openxlsx) # tuk print dalam bentu xlsx 
library(dplyr)

#alur:
## impor data PLTE
## combine semua file excelnya
## tambah satu kolom Yield*Volume
## pilih seri yang akan digunakan
## deteksi outlier (pake coding yg udah ada saat ini)
## ilangin +- 50bps dari IBPA t-1 (krn range tenor area (smisal di bawah 10 th atau di atas 10 th) g lebih dari 50 bps)
## calculate WAY per seri
## bungkus dari pilih seri sampai WAY calculation dalam satu loop function
## combine semua WAY dari masing-masing seri jadi dataset baru


##########################

rm(list=ls())

setelmen<-"C:/Users/DJPPR/OneDrive - Kemenkeu/LELANG & RENCANA LELANG/2024/12 Lelang 19 Maret 2024/WAY_PLTE"
tempat_IBPA<-"C:/Users/DJPPR/OneDrive - Kemenkeu/LELANG & RENCANA LELANG/2024/12 Lelang 19 Maret 2024"
tempat_Raw_PLTE<-"C:/Users/DJPPR/OneDrive - Kemenkeu/LELANG & RENCANA LELANG/2024/12 Lelang 19 Maret 2024/WAY_PLTE/Raw_ PLTE"

date <- "18032024"  # replace it with your actual date, cukup disini seting tanggalnya

#input raw data, combine, dan IBPA t-1

# input raw data
setwd(tempat_Raw_PLTE)
files <- list.files(pattern = "\\.xlsx$") # List the Excel files in the directory
all_data <- lapply(files, function(file) read_excel(file))  # Reads each Excel file into a list #Read and combine the data
MainData <- bind_rows(all_data)  # Combines the list into a single data frame

# input IBPA t-1
setwd(tempat_IBPA) # tempat ngambil file IBPA, jadi satu saja dengan PLTE
ibpa_file <- paste("IBPA_t-1_", date, ".xlsx", sep = "") # Construct file names with the date
IBPA <- read_excel(ibpa_file) # Read Excel files for IBPA

#working directory nya dibalikin lagi ke WAY PLTE tuk kerja lebih lanjut
setwd(setelmen) 

#put additional column, multiplying nominal and yield
MainData$VolYield<-MainData$YIELD*MainData$VOLUME

#################
# mulai olah WAY
#################

# preparation
series_name <-colnames(IBPA) #bikin daftar seri tuk looping per seri  
way_data <- data.frame(SERI = character(), WAY = numeric()) # Initialize an empty data frame to store the WAY values for each series

# Loop over each series
for (seri in series_name) {
  # Subset MainData for the current series
  new_data <- subset(MainData, SECURITIES_ID == seri)
  
  # Select specific columns
  column_select <- c("VOLUME", "YIELD", "VolYield")
  new_data <- new_data[, column_select]
  
  # outlier filter 1: remove values outside of +- 0.5 from IBPA t-1
  value <- IBPA[, seri]
  atas <- as.numeric(value + 0.5)
  bawah <- as.numeric(value - 0.5)
  filtered1 <- as.data.frame(new_data[new_data$YIELD <= atas & new_data$YIELD >= bawah, ])
  
  # outlier filter 2: remove values outside of quantiles
  iqr <- IQR(filtered1$YIELD, na.rm = TRUE)
  up <- quantile(filtered1$YIELD, 0.75, na.rm = TRUE) + 1.5 * iqr
  low <- quantile(filtered1$YIELD, 0.25, na.rm = TRUE) - 1.5 * iqr
  filtered1_clean <- filtered1[filtered1$YIELD > low & filtered1$YIELD < up, ]
  
  # Calculating WAY
  way <- sum(filtered1_clean$VolYield) / sum(filtered1_clean$VOLUME)
  
  # Add WAY value to the data frame
  way_data <- rbind(way_data, data.frame(SERI = seri, WAY = way))
}

# Print WAY result for each series
way_file <- paste("WAY_PLTE_", date, ".xlsx", sep = "") # untuk naruh output
write.xlsx(way_data, file = way_file, rowNames = FALSE)

#############################################
## code tanpa loop ###

## take only 1 seri 
#new_data<-subset(MainData, SERI == 'PBS032')

## pilih kolom2 tertentu: Nominal, yield. VolYield
#column_select<-c("NOMINAL","YIELD","VolYield")
#new_data<-new_data[,column_select]

## outlier filter 1: ilangin +- 50bps dari IBPA t-1
#seri<-"PBS032"
#value<-IBPA[,seri]
#atas<-as.numeric(value+0.5) #klo mau merubah parameter filter 1, ubah di bagian ini (yg saat ini 50 bps menjadi 0.5)
#bawah<-as.numeric(value-0.5) #klo mau merubah parameter filter 1, ubah di bagian ini (yg saat ini 50 bps menjadi 0.5)
#filtered1<-as.data.frame(new_data[new_data$YIELD <= atas & new_data$YIELD>=bawah, ]) # pilih rows yg masuk kriteria atas dan bawah

# outlier filter 2: pake quantile
#iqr <- IQR(filtered1$YIELD, na.rm = TRUE)
#up <- quantile(filtered1$YIELD, 0.75, na.rm = TRUE) #+ 1.5 * iqr # Upper Range  
#low <- quantile(filtered1$YIELD, 0.25, na.rm = TRUE) #- 1.5 * iqr # Lower Range

#filtered1_clean_rows <-subset(filtered1$YIELD, filtered1$YIELD > low & filtered1$YIELD < up)

#series_rows <- which(filtered1$YIELD %in% filtered1_clean_rows)
#filtered1_clean <- filtered1[c(series_rows),]

## Calculating WAY
#way <- sum(filtered1_clean$VolYield)/sum(filtered1_clean$NOMINAL)

########################################################################
