-- !preview conn=conn

CREATE TABLE gmh.portfolio_assignments (
  portfolio_id INT NOT NULL REFERENCES gmh.portfolios(portfolio_id),
  property_id BIGINT NOT NULL REFERENCES gmh.properties(gmh_property_id),
  PRIMARY KEY (portfolio_id, property_id)
);
