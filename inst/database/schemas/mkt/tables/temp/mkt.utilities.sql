CREATE TABLE mkt.utilities (
    utility_id SERIAL PRIMARY KEY,
    property_id TEXT NOT NULL REFERENCES mkt.properties (property_id),
    utility_name TEXT NOT NULL,
    utility_amount DECIMAL(10, 2) NOT NULL DEFAULT 0.00,
    utility_category TEXT NOT NULL CHECK (utility_category IN ('Main', 'Other')),
    per TEXT CHECK (per IN ('Per Unit', 'Per Bed')) DEFAULT 'Per Unit',
    included BOOLEAN DEFAULT FALSE,
    capped BOOLEAN DEFAULT FALSE,
    allowance BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
