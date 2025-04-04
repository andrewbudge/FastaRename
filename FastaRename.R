library(seqinr)

# Function to rename FASTA headers based on TSV file
rename_fasta_headers <- function(fasta_file, tsv_file, output_fasta) {
  alias_data <- read.delim(tsv_file, header = FALSE, stringsAsFactors = FALSE)
  colnames(alias_data) <- c("original_header", "alias")
  fasta_data <- read.fasta(fasta_file, seqtype = "DNA", as.string = TRUE)
  
  new_fasta_data <- lapply(fasta_data, function(seq) {
    original_header <- attr(seq, "name")
    alias_row <- alias_data[alias_data$original_header == original_header, ]
    if (nrow(alias_row) > 0) {
      new_header <- alias_row$alias
      attr(seq, "name") <- new_header
    } else {
      message("No alias found for header: ", original_header)
    }
    
    return(seq)
  })
  
  # Create the output in the correct FASTA format
  new_fasta_data <- lapply(new_fasta_data, function(seq) {
    header <- attr(seq, "name")
    sequence <- toupper(as.character(seq))
    return(paste(">", header, "\n", sequence, sep = ""))
  })
  
  writeLines(unlist(new_fasta_data), output_fasta)
  cat("Modified FASTA file saved to:", output_fasta, "\n")
}

# Command-line arguments
args <- commandArgs(trailingOnly = TRUE)
fasta_file <- args[1]
tsv_file <- args[2]
output_fasta <- args[3]

rename_fasta_headers(fasta_file, tsv_file, output_fasta)

