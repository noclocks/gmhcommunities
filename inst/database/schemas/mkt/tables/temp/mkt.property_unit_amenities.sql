CREATE TABLE mkt.property_unit_amenities (
    amenity_id SERIAL PRIMARY KEY,
    property_id TEXT REFERENCES mkt.properties(property_id),
    unit_levels INT DEFAULT 1,
    tv_renatable_rate DECIMAL(10, 2),
    floor_premium DECIMAL(10, 2),
    poolside_premium DECIMAL(10, 2),
    top_floor_premium DECIMAL(10, 2),
    view_premium DECIMAL(10, 2),
    furniture_rentable_rate DECIMAL(10, 2),
    other_premiums DECIMAL(10, 2),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
