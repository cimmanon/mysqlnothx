\set ON_ERROR_STOP true


BEGIN;

----------------------------------------
-- Tables
----------------------------------------

CREATE TABLE "foreign_keyed_table" (
	"id" INTEGER,
	"ref" INTEGER NOT NULL
);

CREATE TABLE "indexes" (
	"id" INTEGER,
	"name" VARCHAR(50),
	"nickname" VARCHAR(50),
	"one" Text,
	"two" VARCHAR(100)
);

CREATE TABLE "integers" (
	"signed_tinyint" SMALLINT,
	"signed_smallint" SMALLINT,
	"signed_mediumint" SMALLINT,
	"signed_int" INTEGER,
	"signed_bigint" BIGINT,
	"unsigned_tinyint" BIGINT,
	"unsigned_smallint" BIGINT,
	"unsigned_mediumint" INTEGER,
	"unsigned_int" BIGINT,
	"unsigned_bigint" BIGINT,
	"signed_auto_tinyint" SMALLINT,
	"signed_auto_smallint" SMALLINT,
	"signed_auto_mediumint" SMALLINT,
	"signed_auto_int" INTEGER,
	"signed_auto_bigint" BIGINT,
	"unsigned_auto_tinyint" BIGINT,
	"unsigned_auto_smallint" BIGINT,
	"unsigned_auto_mediumint" INTEGER,
	"unsigned_auto_int" BIGINT,
	"unsigned_auto_bigint" BIGINT
);

CREATE TABLE "missing_reference_table" (
	"name" VARCHAR(50) NOT NULL,
	"foo" INTEGER NOT NULL,
	"bar" VARCHAR(50)
);

CREATE TABLE "quoted_identifiers" (
	"id" INTEGER,
	"name" VARCHAR(50) NOT NULL
);

CREATE TABLE "reference_table" (
	"id" INTEGER,
	"name" text NOT NULL,
	"not_unique_column" VARCHAR(50) NOT NULL
);

CREATE TABLE "types" (
	"test_bool" SMALLINT NOT NULL,
	"test_int" INTEGER,
	"test_int_unsigned" BIGINT,
	"test_tinyint" SMALLINT,
	"test_smallint" SMALLINT,
	"test_mediumint" SMALLINT,
	"test_bigint" BIGINT,
	"test_integer" INTEGER,
	"test_numeric" NUMERIC(10,2),
	"test_real" REAL,
	"test_double" REAL,
	"test_float" REAL,
	"test_float_precision" REAL,
	"test_char" CHAR(2),
	"test_varchar" VARCHAR(255) NOT NULL,
	"test_text" TEXT,
	"test_text_charset" TEXT COLLATE utf8_bin,
	"test_tinytext" TEXT,
	"test_mediumtext" TEXT,
	"test_longtext" TEXT,
	"test_blob" BYTEA,
	"test_tinyblob" BYTEA,
	"test_mediumblob" BYTEA,
	"test_longblob" BYTEA,
	"test_bit" bit(1) NOT NULL,
	"test_timestamp" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"test_timestamp_zero_default" TIMESTAMP NOT NULL DEFAULT '0000-00-00 00:00:00',
	"test_datetime" TIMESTAMP,
	"test_date" DATE NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"test_date_zero_default" DATE NOT NULL DEFAULT '0000-00-00',
	"test_enum" ENUM('chocolate', 'vanilla'),
	"test_set" SET('cat', 'dog', 'doge', 'other')
);



COPY "indexes" FROM stdin;
0	foo	bar	\N	\N
1	foo	bar	\N	\N
2	baz	buz	\N	\N
\.

----------------------------------------
-- Tables
----------------------------------------

CREATE TABLE "binary_test" (
	"test_bit" bit(1) NOT NULL
);



COPY "binary_test" FROM stdin;
0
1
\.

----------------------------------------
-- Tables
----------------------------------------

CREATE TABLE "blob_test" (
	"id" INTEGER NOT NULL,
	"data" BYTEA NOT NULL
);



COPY "blob_test" FROM stdin;
1	\\x696E6163746976652C4C49323235534836375548432D3231363330310D0A
2	\\x
\.

----------------------------------------
-- Unique Constraints
----------------------------------------

ALTER TABLE "blob_test" ADD PRIMARY KEY ("id");

ALTER TABLE "foreign_keyed_table" ADD PRIMARY KEY ("id");

ALTER TABLE "indexes" ADD PRIMARY KEY ("id");

ALTER TABLE "missing_reference_table" ADD PRIMARY KEY ("name");

ALTER TABLE "quoted_identifiers" ADD PRIMARY KEY ("id");

ALTER TABLE "reference_table" ADD PRIMARY KEY ("id");

ALTER TABLE "types" ADD PRIMARY KEY ("test_int");
ALTER TABLE "types" ADD UNIQUE ("test_smallint");
ALTER TABLE "types" ADD UNIQUE ("test_bigint");
ALTER TABLE "types" ADD UNIQUE ("test_text");

----------------------------------------
-- Indexes
----------------------------------------

CREATE INDEX "indexes_name_idx" ON "indexes" ("name");
CREATE INDEX "indexes_nickname_idx" ON "indexes" USING btree ("nickname" desc);
CREATE INDEX "indexes_name_nickname_idx" ON "indexes" ("name", "nickname");
CREATE INDEX "indexes_one_idx" ON "indexes" ("one");

CREATE INDEX  ON "reference_table" ("not_unique_column");

CREATE INDEX  ON "types" USING btree ("test_char");
CREATE INDEX  ON "types" ("test_varchar");

----------------------------------------
-- Reset Sequences
----------------------------------------

SELECT setval(pg_get_serial_sequence('"integers"', 'signed_auto_tinyint'), (SELECT COALESCE(max("signed_auto_tinyint"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"integers"', 'signed_auto_smallint'), (SELECT COALESCE(max("signed_auto_smallint"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"integers"', 'signed_auto_mediumint'), (SELECT COALESCE(max("signed_auto_mediumint"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"integers"', 'signed_auto_int'), (SELECT COALESCE(max("signed_auto_int"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"integers"', 'signed_auto_bigint'), (SELECT COALESCE(max("signed_auto_bigint"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"integers"', 'unsigned_auto_tinyint'), (SELECT COALESCE(max("unsigned_auto_tinyint"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"integers"', 'unsigned_auto_smallint'), (SELECT COALESCE(max("unsigned_auto_smallint"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"integers"', 'unsigned_auto_mediumint'), (SELECT COALESCE(max("unsigned_auto_mediumint"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"integers"', 'unsigned_auto_int'), (SELECT COALESCE(max("unsigned_auto_int"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"integers"', 'unsigned_auto_bigint'), (SELECT COALESCE(max("unsigned_auto_bigint"), 1) FROM "integers"));
SELECT setval(pg_get_serial_sequence('"reference_table"', 'id'), (SELECT COALESCE(max("id"), 1) FROM "reference_table"));
----------------------------------------
-- Foreign Keys
----------------------------------------

ALTER TABLE "foreign_keyed_table" ADD FOREIGN KEY ("ref") REFERENCES "reference_table" ("id");

-- Constraints referencing tables that aren't part of the dump file.

SAVEPOINT imaginary_constraints;

ALTER TABLE "missing_reference_table" ADD FOREIGN KEY ("foo") REFERENCES "imaginary_table" ("foo");
ALTER TABLE "missing_reference_table" ADD FOREIGN KEY ("bar") REFERENCES "reference_table" ("not_unique_column");



