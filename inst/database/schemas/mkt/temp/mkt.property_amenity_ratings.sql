CREATE TABLE mkt.property_amenity_ratings (
  rating_id SERIAL PRIMARY KEY,
  property_id TEXT NOT NULL REFERENCES mkt.properties(property_id),
  property_common_area_rating DOUBLE(2,1) DEFAULT 0.0,
  unit_rating DOUBLE(2,1) DEFAULT 0.0,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP;
)
