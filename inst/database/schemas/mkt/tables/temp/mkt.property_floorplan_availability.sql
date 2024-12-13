CREATE TABLE mkt.property_floorplan_availability (
    availability_id SERIAL PRIMARY KEY,
    floor_plan_id INT NOT NULL REFERENCES mkt.property_floorplans (floor_plan_id),
    property_id TEXT NOT NULL REFERENCES mkt.properties (property_id),
    available BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
