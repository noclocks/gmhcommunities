CREATE TABLE gmh.portfolio_assignments (
  portfolio_id INTEGER NOT NULL REFERENCES gmh.portfolios(portfolio_id),
  property_id INTEGER NOT NULL REFERENCES gmh.properties(property_id),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)
