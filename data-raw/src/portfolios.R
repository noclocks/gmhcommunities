portfolio_assignments <- readr::read_csv("data-raw/data/working/portfolio_assignments.csv")

portfolios <- portfolio_assignments |>
  dplyr::select(
    portfolio_id,
    portfolio_name = portfolio
  ) |>
  dplyr::distinct()
