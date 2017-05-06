\set ON_ERROR_STOP true


BEGIN;

----------------------------------------
-- Tables
----------------------------------------

CREATE TABLE "enum_set_test" (
	"species" TEXT CONSTRAINT "species_allowed_values_ck" CHECK ("species" = any(ARRAY['cat','dog','mouse','hamster','parot','ferret','lizard'])),
	"diet" TEXT[] CONSTRAINT "diet_allowed_values_ck" CHECK ("diet" <@ ARRAY['meat','seeds','cheese','grains','insects','eggs'])
);


COPY "enum_set_test" FROM stdin;
cat	{"meat"}
dog	{"meat"}
mouse	{"cheese","grains"}
hamster	{"grains"}
parot	{"seeds"}
ferret	{"meat","grains","eggs"}
lizard	{"insects"}
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




