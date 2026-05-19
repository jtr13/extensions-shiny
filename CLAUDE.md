# extensions-shiny

Shiny app for exploring the ggplot2 extension ecosystem. Deployed to shinyapps.io.

## Repo structure

- `app.R` — the Shiny app
- `data/` — all dated CSV data files; app always loads the most recent one
- `scripts/` — data creation script (not used at runtime)

## Data file conventions

- Filename format: `in_grammar_cran_v{YYYY}_{MM}_{DD}.csv`
- All CSV versions are kept in `data/` (needed for changelog comparisons)
- `app.R` auto-detects the latest CSV using `list.files()` — no hardcoded filename
- The data creation date displayed in the app UI is parsed from the filename automatically
- `scripts/create_in_grammar_cran.R` auto-generates the dated filename using `Sys.Date()`

## Updating the data

1. Run `scripts/create_in_grammar_cran.R` (requires the `exttools` package)
2. Test the app locally
3. Commit and push

## Branches

- Always create a branch before experimental changes

## Future work

- [ ] Add a changelog tab to the app showing packages added/removed since the last update
- [ ] Investigate automating the data update and publishing to shinyapps.io
