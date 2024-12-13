CREATE TABLE gmh.property_portfolios (
  property_id BIGINT NOT NULL REFERENCES gmh.properties(property_id),
  portfolio_id INT NOT NULL REFERENCES gmh.portfolios(portfolio_id),
  PRIMARY KEY (property_id, portfolio_id)
);
