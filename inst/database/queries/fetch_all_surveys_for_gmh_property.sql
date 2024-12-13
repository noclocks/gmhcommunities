SELECT s.survey_id, s.survey_date, s.status, u.name AS created_by
FROM mkt.surveys s
JOIN mkt.properties p ON s.property_id = p.property_id
JOIN auth.users u ON s.created_by = u.user_id
WHERE p.is_gmh = TRUE;
