-- Competitors
CREATE TABLE mkt.competitors (
    competitor_id TEXT NOT NULL PRIMARY KEY,
    associated_property_id TEXT REFERENCES mkt.properties(property_id),
    competitor_name TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    created_by INT REFERENCES auth.users(user_id) ON DELETE SET NULL
);
