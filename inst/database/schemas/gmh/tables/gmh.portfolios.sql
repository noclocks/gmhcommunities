CREATE TABLE gmh.portfolios (
  portfolio_id SERIAL PRIMARY KEY,
  portfolio_name TEXT NOT NULL,
  portfolio_type TEXT NOT NULL CHECK (portfolio_type IN ('Property Type', 'Owner', 'Equity Parner')),
  portfolio_description TEXT,
  investment_partner_id INT NOT NULL REFERENCES gmh.investment_partners(investment_partner_id),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
