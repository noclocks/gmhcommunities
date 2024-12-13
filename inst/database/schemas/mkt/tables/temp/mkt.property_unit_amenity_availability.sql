CREATE TABLE mkt.property_unit_amenity_availability (
    amenity_id SERIAL PRIMARY KEY,
    property_id TEXT REFERENCES mkt.properties(property_id),
    amenity TEXT,
    availabile BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
