# Merge MET Nordic files (hourly)

This function lets you combine the hourly files downloaded by
`download_metnordic()` into single files. **Note**, these files are
separate per variable!

## Usage

``` r
metnordic_merge_hourly(
  folderpath,
  variable,
  outpath,
  n_cores = NULL,
  overwrite = FALSE,
  verify = FALSE,
  verbose = FALSE
)
```

## Arguments

- folderpath:

  (String) folder where individual files are located

- variable:

  (String) MET Nordic variable to combine

- outpath:

  (String) folder where to write the file

- n_cores:

  (Integer) Number of cores to use for parallel processing (Defaults to
  max - 2)

- overwrite:

  (Boolean) overwrite existing file?

- verify:

  (Boolean) an optional check to see if all files to be merged are
  incremental (Recommended!)

- verbose:

  (Boolean) print status?

## Value

path of written file

## See also

[`metnordic_download()`](https://moritzshore.github.io/miljotools/reference/metnordic_download.md)
[`metnordic_extract()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract.md)
