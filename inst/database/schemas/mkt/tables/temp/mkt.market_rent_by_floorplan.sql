CREATE TABLE mkt.market_rent_by_floorplan (
    property_id TEXT NOT NULL REFERENCES mkt.properties (property_id),
    floor_plan_id INT NOT NULL REFERENCES mkt.property_floorplans (floor_plan_id),
    leasing_week_id INT NOT NULL REFERENCES mkt.leasing_weeks (leasing_week_id),
    market_rent_per_bed DECIMAL(10, 2) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
