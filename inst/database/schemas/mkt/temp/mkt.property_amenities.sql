CREATE TABLE mkt.property_amenities (
    amenity_id SERIAL PRIMARY KEY,
    property_id TEXT REFERENCES mkt.properties(property_id),
    leasing_week_id INT REFERENCES mkt.leasing_weeks(leasing_week_id),
    amenity TEXT,
    availabile BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
