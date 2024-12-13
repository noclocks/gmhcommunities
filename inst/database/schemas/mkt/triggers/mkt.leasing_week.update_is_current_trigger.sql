DROP TRIGGER IF EXISTS update_is_current_trigger ON mkt.leasing_week;

CREATE TRIGGER update_is_current_trigger
AFTER INSERT OR UPDATE ON mkt.leasing_week
FOR EACH STATEMENT
EXECUTE FUNCTION mkt.update_is_current_flag();
