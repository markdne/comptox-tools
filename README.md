# comptoxtools

An R package providing functions to search and retrieve chemical data from the [EPA CompTox Chemical Dashboard API](https://comptox.epa.gov/dashboard/).

## Installation

Install from GitHub using the `remotes` package:

```r
# install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/useful_funcs")
```

> Replace `YOUR_GITHUB_USERNAME` with your GitHub username after pushing this repo.

## API Key

Most functions require a CompTox Dashboard API key. Register for free at <https://comptox.epa.gov/dashboard/>.

## Functions

| Function | Description |
|---|---|
| `comptox_chem_search()` | Search by name, CAS-RN, DTXSID, or any identifier |
| `deduplicate_chem_results()` | Collapse duplicates when multiple ID columns map to the same chemical |
| `get_struc_from_dtxsid()` | Retrieve structural/physico-chemical properties by DTXSID or DTXCID |
| `get_structure_image()` | Fetch PNG or SVG structure images |
| `convert_date_to_casrn()` | Recover CAS-RNs that Excel mis-formatted as dates |

## Usage

### Search for chemicals

```r
library(comptoxtools)

df <- tibble::tibble(
  chem_name = c("Benzene", "Toluene", "Xylene"),
  casrn     = c("71-43-2", "108-88-3", "1330-20-7")
)

# Search by a single column
results <- comptox_chem_search(
  input_data   = df,
  api_key      = "your_api_key_here",
  chem_id_cols = chem_name
)

# Search across multiple columns and collapse duplicates
results <- comptox_chem_search(
  input_data   = df,
  api_key      = "your_api_key_here",
  chem_id_cols = c(chem_name, casrn)
)

deduped <- deduplicate_chem_results(
  results      = results,
  input_data   = df,
  chem_id_cols = c(chem_name, casrn)
)
```

### Retrieve structural properties

```r
props <- get_struc_from_dtxsid(
  input_data   = c("DTXSID5020281", "DTXSID8020961"),
  api_key      = "your_api_key_here"
)
```

### Fetch structure images

```r
imgs <- get_structure_image(
  input_data  = c("DTXSID5020281", "DTXSID8020961"),
  api_key     = "your_api_key_here",
  save_images = TRUE,
  output_dir  = "structure_images"
)
```

### Fix Excel-mangled CAS-RNs

```r
casrns <- convert_date_to_casrn(c("3/1/2038", "100-00-5", "3/7/2680"))
# Returns: "1-Mar-38" → "100-01-6", etc.
```

## Parallel execution and progress bars

All API functions use [`furrr`](https://furrr.futureverse.org/) for parallel requests and [`progressr`](https://progressr.futureverse.org/) for progress reporting.

```r
library(future)
library(progressr)

# Enable parallel requests
plan(multisession, workers = 4)

# Show a progress bar
with_progress({
  results <- comptox_chem_search(
    input_data   = df,
    api_key      = "your_api_key_here",
    chem_id_cols = chem_name
  )
})
```

## Package setup (for developers)

After cloning, regenerate documentation and check the package:

```r
# install.packages(c("devtools", "roxygen2"))
devtools::document()   # regenerates NAMESPACE and man/ pages
devtools::check()      # runs R CMD check
devtools::install()    # installs locally
```
