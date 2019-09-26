-- NOTE: this file will not successfully import into MySQL

/* this is a multi-line comment,
it should be ignored by the parser */

-- this is an inline comment, it should also be ignored
-- the following comments would be found in a MySQL dump file

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

-- note that comments are not currently supported inside *any* statement

-- testing ignoring of the drop table statement
DROP TABLE IF EXISTS `quoted_identifiers`;
-- test our quoted identifiers parser
CREATE TABLE `quoted_identifiers` (
	`id` INT PRIMARY KEY,
	`name` VARCHAR(50) NOT NULL
);

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Types
\*----------------------------------------------------------------------------------------------------*/

CREATE TABLE types (
	test_bool TINYINT(1) NOT NULL DEFAULT 1,

	test_int INT,
	test_int_unsigned INT unsigned,
	test_tinyint TINYINT,
	test_smallint SMALLINT,
	test_mediumint MEDIUMINT,
	test_bigint BIGINT,
	test_integer INTEGER,

	test_numeric NUMERIC(10,2) unsigned,
	test_decimal DECIMAL(10,2) default NULL,
	test_real REAL,
	test_double DOUBLE,
	test_float FLOAT,
	test_float_precision FLOAT(10,2),

	test_char CHAR(2),
	test_varchar VARCHAR(255) NOT NULL,
	test_text TEXT,
	test_text_charset TEXT CHARACTER SET utf8 COLLATE utf8_bin,
	test_tinytext TINYTEXT,
	test_mediumtext MEDIUMTEXT,
	test_longtext LONGTEXT,

	test_blob BLOB,
	test_tinyblob TINYBLOB,
	test_mediumblob MEDIUMBLOB,
	test_longblob LONGBLOB,
	test_bit bit(1) NOT NULL DEFAULT b'0',

	test_timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
	test_timestamp_zero_default TIMESTAMP NOT NULL DEFAULT '0000-00-00 00:00:00'
		COMMENT 'This is a hack to circumvent the "only 1 timestamp per table can be set to the current time" limitation',
	test_datetime DATETIME,
	test_date DATE NOT NULL DEFAULT CURRENT_TIMESTAMP,
	test_date_zero_default DATE NOT NULL DEFAULT '0000-00-00'
		COMMENT 'This is a hack to circumvent the "only 1 timestamp per table can be set to the current time" limitation',

	test_enum ENUM('chocolate', 'vanilla'),
	test_set SET('cat', 'dog', 'doge', 'other'),

	PRIMARY KEY (test_int) USING BTREE,
	UNIQUE (test_smallint),
	UNIQUE KEY (test_bigint),
	UNIQUE (test_text(100)),
	INDEX (test_char) USING btree,
	KEY (test_varchar)
);

CREATE TABLE integers (
	signed_tinyint TINYINT,
	signed_smallint SMALLINT,
	signed_mediumint MEDIUMINT,
	signed_int INT,
	signed_bigint BIGINT,

	unsigned_tinyint TINYINT unsigned,
	unsigned_smallint SMALLINT unsigned,
	unsigned_mediumint MEDIUMINT unsigned,
	unsigned_int INT unsigned,
	unsigned_bigint BIGINT unsigned,

	signed_auto_tinyint TINYINT AUTO_INCREMENT,
	signed_auto_smallint SMALLINT AUTO_INCREMENT,
	signed_auto_mediumint MEDIUMINT AUTO_INCREMENT,
	signed_auto_int INT AUTO_INCREMENT,
	signed_auto_bigint BIGINT AUTO_INCREMENT,

	unsigned_auto_tinyint TINYINT unsigned AUTO_INCREMENT,
	unsigned_auto_smallint SMALLINT unsigned AUTO_INCREMENT,
	unsigned_auto_mediumint MEDIUMINT unsigned AUTO_INCREMENT,
	unsigned_auto_int INT unsigned AUTO_INCREMENT,
	unsigned_auto_bigint BIGINT unsigned AUTO_INCREMENT
);

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Foreign Keys
\*----------------------------------------------------------------------------------------------------*/

CREATE TABLE reference_table (
	id INT AUTO_INCREMENT PRIMARY KEY,
	name text NOT NULL,
	not_unique_column VARCHAR(50) NOT NULL,

	KEY (not_unique_column)
) ENGINE=InnoDB;

CREATE TABLE foreign_keyed_table (
	id INT PRIMARY KEY,
	ref INT NOT NULL,

	FOREIGN KEY (ref) REFERENCES reference_table (id)
) ENGINE=InnoDB;

-- this table is foreign keyed to a table that doesn't exist
-- it also references a column that's not unique for a table that does exist
CREATE TABLE missing_reference_table (
	name VARCHAR(50) NOT NULL PRIMARY KEY,
	foo INT NOT NULL,
	bar VARCHAR(50),

	FOREIGN KEY (foo) REFERENCES imaginary_table (foo),
	FOREIGN KEY (bar) REFERENCES reference_table (not_unique_column)
) ENGINE=InnoDB;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Indexes
\*----------------------------------------------------------------------------------------------------*/

CREATE TABLE indexes (
	id INT PRIMARY KEY,
	name VARCHAR(50),
	nickname VARCHAR(50),
	one Text,
	two VARCHAR(100)
);
-- index without specifying an index type
CREATE INDEX `indexes_name_idx` ON `indexes` (`name`);
-- standard order for columns with optional sort and index type
CREATE INDEX `indexes_nickname_idx` ON `indexes` (`nickname` desc) USING btree;
-- test multiple columns
CREATE INDEX `indexes_name_nickname_idx` ON `indexes` (`name`, `nickname`);
-- test specifying a specific index length
CREATE INDEX `indexes_one_idx` ON `indexes` (`one`(100));

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Inserts
\*----------------------------------------------------------------------------------------------------*/

-- locking the tables should be ignored as well
LOCK TABLES `indexes` WRITE;
/*!40000 ALTER TABLE `indexes` DISABLE KEYS */;
INSERT INTO `indexes` VALUES (0,'foo','bar', NULL, NULL),(1,'foo','bar', NULL, NULL),(2,'baz','buz', NULL, NULL);
/*!40000 ALTER TABLE `indexes` ENABLE KEYS */;
UNLOCK TABLES;

-- Binary
CREATE TABLE binary_test (
	test_bit bit(1) NOT NULL DEFAULT b'0'
);

INSERT INTO `binary_test` VALUES (b'0'),(b'1');

-- Blob
CREATE TABLE blob_test (
	id INTEGER NOT NULL PRIMARY KEY,
	data BLOB NOT NULL
);

INSERT INTO `blob_test` VALUES (1,0x696E6163746976652C4C49323235534836375548432D3231363330310D0A),(2,0x);
