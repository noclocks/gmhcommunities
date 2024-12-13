CREATE TABLE mkt.properties (
  property_id TEXT PRIMARY KEY,
  entrata_property_id BIGINT REFERENCES mkt.entrata_properties(entrata_property_id),
  property_name TEXT NOT NULL,
  is_competitor BOOLEAN DEFAULT FALSE,
  created_by UUID NOT NULL REFERENCES auth.users(user_id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
