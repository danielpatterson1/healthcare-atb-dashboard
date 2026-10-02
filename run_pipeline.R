# One command build: generate synthetic extracts, run ETL, validate, render.
#   Rscript run_pipeline.R

source("R/generate_synthetic_data.R")
source("R/etl.R")
source("R/validate.R")

as_of <- as.Date("2026-09-30")

generate_atb_extracts(as_of = as_of)
res    <- run_etl(as_of = as_of)
checks <- validate_atb(res)
saveRDS(checks, "data/curated/checks.rds")

message(sprintf("ETL complete: %s accounts, %s open, %s duplicate transactions removed, %s of %s checks passed",
                format(nrow(res$accounts), big.mark = ","), format(nrow(res$atb), big.mark = ","),
                res$dupes_removed, sum(checks$passed), nrow(checks)))

rmarkdown::render("atb_dashboard.Rmd", output_file = "index.html", quiet = TRUE)
message("Dashboard written to index.html")
