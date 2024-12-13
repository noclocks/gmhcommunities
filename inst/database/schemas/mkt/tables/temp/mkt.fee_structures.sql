CREATE TABLE mkt.fee_structures (
  fee_structure_id SERIAL PRIMARY KEY,
  property_id TEXT NOT NULL REFERENCES mkt.properties (property_id),
  fee_structure TEXT NOT NULL CHECK (fee_structure IN ('App Fee Waived-Admin Due', 'Admin Fee Waived-App Due', 'Both Fees Waived', 'Both Fees Due')) DEFAULT 'Both Fees Waived',
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
