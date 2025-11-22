

```R
# 1. Ensure 'remotes' package is installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
````

### Install from GitHub

Use the `install_github` function to download and install the package.

```r
# 2. Install the package from GitHub
# We use force=TRUE to ensure the latest version is pulled.
remotes::install_github("Passpoor/biofree.qyKEGGtools", 
                        force = TRUE, 
                        upgrade = "never")

# 3. Load the package
library(biofree.qyKEGGtools)
```

## 🛠 Usage Example

Once installed, you can use the package's core function (e.g., for Human enrichment):

```r
# Example gene IDs (Entrez ID format)
gene_list <- c("100", "108", "111", "115", "121", "124", "126", "127", "128", "131")

# Replace 'enrich_local_KEGG' with your actual function name
results <- biofree.qyKEGGtools::enrich_local_KEGG(
  gene = gene_list,
  organism = "hsa", 
  keyType = "ncbi-geneid"
)

# View top results
head(results)
```
