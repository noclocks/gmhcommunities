-- Schema: gmh
-- Table: manual_model_beds

CREATE TABLE gmh.model_beds (
    model_bed_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    property_id UUID REFERENCES gmh.properties(property_id) ON DELETE CASCADE,
    partner_id UUID REFERENCES gmh.investment_partners(partner_id) ON DELETE CASCADE,
    total_beds INT NOT NULL,
    occupied_beds INT NOT NULL,
    manual_beds INT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    modified_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.manual_model_beds IS 'GMH Manual Model Beds.';
COMMENT ON COLUMN gmh.manual_model_beds.model_bed_id IS 'Unique identifier for the manual model bed.';
COMMENT ON COLUMN gmh.manual_model_beds.property_id IS 'Property identifier for the manual model bed.';
COMMENT ON COLUMN gmh.manual_model_beds.partner_id IS 'Investment partner identifier for the manual model bed.';
COMMENT ON COLUMN gmh.manual_model_beds.total_beds IS 'Total number of beds for the manual model bed.';
COMMENT ON COLUMN gmh.manual_model_beds.occupied_beds IS 'Number of occupied beds for the manual model bed.';
COMMENT ON COLUMN gmh.manual_model_beds.manual_beds IS 'Number of manual beds for the manual model bed.';
COMMENT ON COLUMN gmh.manual_model_beds.created_at IS 'Timestamp when the manual model bed was created.';
COMMENT ON COLUMN gmh.manual_model_beds.modified_at IS 'Timestamp when the manual model bed was last modified.';

