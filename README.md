# What is mysqlnothx?

This is a dump file converter that takes MySQL dump files and converts them to PostgreSQL.  It is based on a converter I wrote while working at Zalora.  Thanks to the Pipes library, it is capable of parsing very large files without running out of memory.

# Cool features

* Insert statements are converted to using the `COPY tablename FROM stdin` command for faster importing
* Indexes and Constraints are moved to the very end of the file, after the data has been inserted, just like a real PostgreSQL dump
* It differentiates between real and "imaginary" foreign key constraints.  If the table it's referencing doesn't exist in the dump file, it's placed after a savepoint named `imaginary_constraints`, allowing you to partially rollback the import if the tables don't exist in the database either.

# Installation & Setup

TODO: write this part

# Supported MySQL Column Types

* Text (tiny, medium, large)
* Char
* Varchar
* Enum (partial)
* Set (partial)
* Date
* Time
* Datetime
* Timestamp
* Bit
* Blob
* Int (tiny, small, medium, big)
* Double
* Float

In addition to porting these column types to their closest compatible PostgreSQL data type, it has fixes for the following cases:

* CHAR(0) and VARCHAR(0) converts to CHAR(1)
* Default date values of `0000-00-00` for types that include a date (DATE, DATETIME, TIMESTAMP) are converted to `CURRENT_TIMESTAMP`

# TODO

List of features I'd like to add.

* Addressing time zone differences between source and destination (not sure if this is reasonable)
* Fixing out of bounds dates (eg. February 31st)
