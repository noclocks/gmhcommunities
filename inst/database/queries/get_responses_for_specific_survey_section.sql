SELECT sr.field_name, sr.field_value, sr.previous_value
FROM mkt.survey_responses sr
JOIN mkt.survey_sections ss ON sr.section_id = ss.section_id
WHERE ss.survey_id = 123 AND ss.name = 'Leasing Summary';
