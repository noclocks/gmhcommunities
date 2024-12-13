CREATE TABLE mkt.property_summary (
  property_id TEXT NOT NULL PRIMARY KEY REFERENCES mkt.properties(property_id),
  property_name TEXT NOT NULL,
  website TEXT,
  address TEXT,
  phone_number TEXT,
  property_image TEXT,
  developer TEXT,
  manager TEXT,
  owner TEXT,
  property_type TEXT,
  property_status TEXT,
  property_rating NUMERIC(2, 1),
  comp_status TEXT,
  year_built INT,
  most_recent_sale DATE,
  distance_from_campus NUMERIC(5, 2),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
