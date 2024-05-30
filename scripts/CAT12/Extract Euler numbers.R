#### Extract euler numbers from QC pdf report ####
library(pdftools)
library(tesseract)
library(stringr)
library(dplyr)

# Set the path to the directory containing the PDF files
pdf_directory <- "./report/"

# List all PDF files in the directory
pdf_files <- list.files(path = pdf_directory, pattern = "\\.pdf$", full.names = TRUE)

# Initialize OCR engine and a dataframe to collect the results
engine <- tesseract()
results <- data.frame(File = character(), EulerNumber = character(), stringsAsFactors = FALSE)

# Function to apply OCR on each image and extract text
apply_ocr <- function(image_file, pdf_file, page_number) {
  text <- ocr(image_file, engine = engine)
  # Extract defect number from the OCR text
  euler_number <- str_extract(text, "(?<=Surface Euler / defect number: )\\d+")
  # Print extracted values
  print(paste("Euler number:", euler_number))
  # Append results to the dataframe
  result <- data.frame(File = basename(pdf_file),  EulerNumber = euler_number,  stringsAsFactors = FALSE)
  return(result)
}

# Function to process each PDF file
process_pdf <- function(pdf_file) {
  # Print the file being processed
  print(paste("Processing:", pdf_file))
  # Convert the PDF pages to images
  images <- pdf_convert(pdf_file, dpi = 300)
  # Apply OCR to each image and extract text, gather results
  texts <- lapply(seq_along(images), function(i) apply_ocr(images[[i]], pdf_file, i))
  # Delete the image files after processing
  file.remove(images)
  # Combine all results for the current PDF
  pdf_results <- do.call(rbind, texts)
  return(pdf_results)
}

# Apply the processing function to each PDF file and collect all results
all_results <- do.call(rbind, lapply(pdf_files, process_pdf))

# Save results
write.csv(all_results, "./Euler numbers of surface based measures.csv", row.names = FALSE)
