CREATE TABLE mkt.amenity_premiums (
  premium_id SERIAL PRIMARY KEY,
  property_id TEXT REFERENCES mkt.properties(property_id),
  amenity_
  tv_rentable_rate DECIMAL(5,2) DEFAULT 0.00,
  tv_bedroom_premium DECIMAL(5,2) DEFAULT 0.00,
  tv_common_area_premium DECIMAL(5,2) DEFAULT 0.00,
  furniture_bedroom_premium DECIMAL(5,2) DEFAULT 0.00,
)
