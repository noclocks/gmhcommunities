CREATE TABLE mkt.property_parking (
    parking_id SERIAL PRIMARY KEY,
    property_id TEXT NOT NULL REFERENCES mkt.properties (property_id),
    parking_type TEXT NOT NULL,
    is_required BOOLEAN DEFAULT FALSE,
    is_included BOOLEAN DEFAULT FALSE,
    amount DECIMAL(10, 2) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
