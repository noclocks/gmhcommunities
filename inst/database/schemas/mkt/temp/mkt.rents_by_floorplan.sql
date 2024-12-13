CREATE TABLE mkt.rents_by_floorplan (
    rent_by_floorplan_id SERIAL PRIMARY KEY,
    property_id TEXT NOT NULL REFERENCES mkt.properties (property_id),
    floor_plan_id INT NOT NULL REFERENCES mkt.property_floorplans (floor_plan_id),
    leasing_week_id INT NOT NULL REFERENCES mkt.leasing_weeks (leasing_week_id),

    rent DECIMAL(10, 2) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)
