# Create aggregate .bib file from individuals

# Directory assumed to be "./consult/00-references"
## If working in project directory, no change
## If working in References directory, setwd to parent

if(!dir.exists("./consult/00-references")){setwd("..")}

# List .bib files within "References" directory 
bib.files <- list.files(path="./consult/00-references", pattern="\\.bib$", recursive = TRUE, full.names = TRUE)

print(paste("There are", length(bib.files), ".bib files in the References directory."))

# Read .bib files into a single object
bibs <- lapply(bib.files,readLines)

# Write new .bib file for book project bibliography
write.table(unlist(bibs), file = "./consult/00-references/references.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)
