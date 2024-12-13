CREATE TABLE mkt.property_floorplans (
    floor_plan_id SERIAL PRIMARY KEY,
    property_id TEXT NOT NULL REFERENCES mkt.properties (property_id),
    floorplan_unit_type TEXT NOT NULL CHECK (floorplan_unit_type IN ('Studio', '1 Bedroom', '2 Bedroom', '3 Bedroom', '4 Bedroom', '5 Bedroom', '6 Bedroom')),
    floorplan_code TEXT NOT NULL,
    floorplan_description TEXT,
    square_footage INT,
    bedrooms INT,
    bathrooms INT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
