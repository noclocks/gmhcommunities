-- Function to update the is_current flag
CREATE OR REPLACE FUNCTION mkt.update_is_current_flag()
RETURNS TRIGGER AS $$
BEGIN
    -- Prevent recursion using pg_trigger_depth()
    IF pg_trigger_depth() > 1 THEN
        RETURN NULL;
    END IF;

    -- Reset all rows to FALSE
    UPDATE mkt.leasing_week
    SET is_current = FALSE;

    -- Set the current leasing week to TRUE
    UPDATE mkt.leasing_week
    SET is_current = TRUE
    WHERE CURRENT_DATE BETWEEN start_date AND end_date;

    RETURN NULL; -- Triggers on UPDATE/INSERT do not return a value
END;
$$ LANGUAGE plpgsql;
