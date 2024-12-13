CREATE TABLE gmh.properties (
  gmh_property_id SERIAL PRIMARY KEY,
  entrata_property_id BIGINT PRIMARY KEY REFERENCES entrata.properties(property_id),
  property_name TEXT NOT NULL,
  property_manager_id INT NOT NULL REFERENCES auth.users(user_id),
  is_active BOOLEAN DEFAULT TRUE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
