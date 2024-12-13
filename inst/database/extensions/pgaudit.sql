CREATE EXTENSION logs.pgaudit;
ALTER SYSTEM SET pgaudit.log = 'write,ddl';
ALTER SYSTEM SET pgaudit.log_catalog = 'on';
