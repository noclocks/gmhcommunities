CREATE TABLE mkt.property_images (
  image_id SERIAL PRIMARY KEY,
  property_id BIGINT NOT NULL REFERENCES mkt.properties(property_id),
  image_url TEXT NOT NULL,
  caption TEXT,
  width INT,
  height INT,
  format TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
