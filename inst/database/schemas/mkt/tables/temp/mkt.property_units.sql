CREATE TABLE mkt.property_units (
  unit_id SERIAL PRIMARY KEY,
  property_id TEXT NOT NULL REFERENCES mkt.properties(property_id),
  unit_rating DOUBLE(2,1) DEFAULT 0.0,
  unit_levels INT DEFAULT 1,
)
