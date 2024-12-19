
portfolios <- tibble::tibble(
  portfolio_name = c(
    "Medistar",
    "CRG",
    "AGC",
    "Principal",
    "CBRE"
  ),
  portfolio_type = c(
    "Owner",
    "Equity Partner",
    "Equity Partner",
    "Equity Partner",
    "Equity Partner"
  ),
  number_of_properties = c(
    1,
    1,
    10,
    4,
    2
  )
)

portfolio_assignments <- readr::read_csv("data-raw/data/working/portfolio_assignments.csv")

portfolios <- portfolio_assignments |>
  dplyr::select(
    portfolio_id,
    portfolio_name = portfolio
  ) |>
  dplyr::distinct()
