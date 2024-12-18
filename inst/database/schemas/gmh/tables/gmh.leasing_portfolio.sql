-- Student Housing Portfolio; Innovative Live Portfolio; and Conventional Portfolio

CREATE TABLE gmh.leasing_portfolio (
  leasing_portfolio_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  name TEXT NOT NULL,
  description TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
