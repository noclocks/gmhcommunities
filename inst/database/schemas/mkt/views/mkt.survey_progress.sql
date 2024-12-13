CREATE MATERIALIZED VIEW mkt.survey_progress AS
SELECT
  survey_id,
  section_id,
  section_name,
  COUNT(response_id) AS completed_fields,
  SUM(CASE WHEN field_value IS NOT NULL THEN 1 ELSE 0 END) AS filled_fields
FROM mkt.survey_sections
LEFT JOIN mkt.survey_responses USING (section_id)
GROUP BY survey_id, section_id, section_name;
