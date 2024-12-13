/*
CREATE TYPE mkt.section_names AS ENUM (
  'Property Summary',
  'Leasing Summary',
  'Short Term Leases',
  'Fees',
  'Amenities',
  'Parking',
  'Utilities',
  'Notes',
  'Rents'
)
*/

DROP TABLE IF EXISTS mkt.sections;

CREATE TABLE IF NOT EXISTS mkt.sections (
    section_id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    survey_id INT REFERENCES mkt.surveys(survey_id) ON DELETE CASCADE,
    section_name TEXT NOT NULL,
    update_frequency TEXT,
    is_required BOOLEAN NOT NULL DEFAULT TRUE,
    section_status TEXT NOT NULL CHECK (section_status IN ('Complete', 'Incomplete')),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
