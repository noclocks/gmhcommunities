CREATE TABLE gmh.property_locations (
  property_id BIGINT PRIMARY KEY REFERENCES gmh.properties(property_id),
  address TEXT,
  place_id TEXT,
  maps_url TEXT,
  latitude DOUBLE PRECISION,
  longitude DOUBLE PRECISION
);
