\set ON_ERROR_STOP true


BEGIN;

----------------------------------------
-- Tables
----------------------------------------



COPY "quoted_identifiers" FROM stdin;
1	foo
\.

----------------------------------------
-- Unique Constraints
----------------------------------------


----------------------------------------
-- Indexes
----------------------------------------


----------------------------------------
-- Reset Sequences
----------------------------------------


----------------------------------------
-- Foreign Keys
----------------------------------------


-- Constraints referencing tables that aren't part of the dump file.

SAVEPOINT imaginary_constraints;




