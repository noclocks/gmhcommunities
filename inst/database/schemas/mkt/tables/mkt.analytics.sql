CREATE TABLE mkt.analytics (
    metric_id SERIAL PRIMARY KEY,
    property_id INT REFERENCES mkt.properties(property_id) ON DELETE CASCADE,
    metric_name TEXT NOT NULL, -- e.g., "average_rent"
    metric_value NUMERIC,
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
