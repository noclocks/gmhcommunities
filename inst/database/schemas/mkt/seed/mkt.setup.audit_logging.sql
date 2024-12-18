SELECT logs.remove_audit_triggers_from_schema('mkt');
SELECT logs.add_audit_triggers_to_schema('mkt');

-- TEST
UPDATE mkt.properties
SET updated_at = NOW()
WHERE property_id = 'boylston';
