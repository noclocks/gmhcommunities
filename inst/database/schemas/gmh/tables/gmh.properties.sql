DROP TABLE IF EXISTS gmh.properties CASCADE;

CREATE TABLE IF NOT EXISTS gmh.properties (
  gmh_property_id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  entrata_property_id BIGINT PRIMARY KEY REFERENCES entrata.properties(property_id),
  property_name TEXT NOT NULL,
  property_type TEXT NOT NULL,
  property_leasing_portfolio TEXT NOT NULL,
  property_manager_id INT NOT NULL REFERENCES auth.users(user_id),
  is_active BOOLEAN DEFAULT TRUE,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

