CREATE TRIGGER update_timestamp
AFTER UPDATE ON mkt.properties
FOR EACH ROW
EXECUTE FUNCTION update_timestamp();
