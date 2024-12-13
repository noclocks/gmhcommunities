INSERT INTO mkt.properties (name, is_gmh, gmh_property_id, portfolio_id)
VALUES
    ('1047 Commonwealth', TRUE, NULL, 1), -- GMH property
    ('1330 Boylston', FALSE, 1, NULL),   -- Competitor
    ('Van Ness', FALSE, 1, NULL),
    ('Bower', FALSE, 1, NULL);

INSERT INTO mkt.surveys (property_id, user_id, survey_date, status)
VALUES
    (1, 1, '2024-11-01', 'Draft');
