CREATE MATERIALIZED VIEW mkt.leasing_reconciliation AS
SELECT
    e.property_id,
    e.current_occupancy_percent AS entrata_occupancy,
    s.field_value::NUMERIC AS survey_occupancy
FROM entrata.leases e
JOIN mkt.survey_responses s ON e.property_id = s.property_id
WHERE s.field_name = 'current_occupancy_percent';
