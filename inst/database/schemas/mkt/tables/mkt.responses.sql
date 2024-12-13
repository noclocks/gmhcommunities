CREATE TABLE mkt.responses (
    response_id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    section_id INT REFERENCES mkt.sections(section_id) ON DELETE CASCADE,
    field_name TEXT NOT NULL, -- Field name (e.g., current_occupancy)
    field_value TEXT, -- Current value
    prior_value TEXT, -- Prior submission value
    hint_value TEXT, -- Hint value from i.e. Entrata data
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by INT REFERENCES auth.users(user_id) ON DELETE SET NULL,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);



