\set ON_ERROR_STOP true


BEGIN;

----------------------------------------
-- Tables
----------------------------------------

CREATE TABLE "quoted_identifiers" (
	"id" INTEGER,
	"name" VARCHAR(50) NOT NULL
);


COPY "quoted_identifiers" FROM stdin;
1	foo
\.

----------------------------------------
-- Unique Constraints
----------------------------------------

ALTER TABLE "quoted_identifiers" ADD PRIMARY KEY ("id");

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




