CREATE TABLE mkt.notes (
    note_id SERIAL PRIMARY KEY,
    property_id TEXT NOT NULL REFERENCES mkt.properties (property_id),
    leasing_special_notes TEXT NOT NULL,
    property_operations_notes TEXT NOT NULL,
    other_notes TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
