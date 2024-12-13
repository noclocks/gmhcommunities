CREATE TABLE mkt.property_amenities (
    amenity_id SERIAL PRIMARY KEY,
    property_id TEXT REFERENCES mkt.properties(property_id),
    common_area_rating DECIMAL(5,2),
    amenity_name TEXT,
    amenity_availabile BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
