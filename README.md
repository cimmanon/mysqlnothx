# What is mysqlnothx?

This is a dump file converter that takes MySQL dump files and converts them to PostgreSQL.  It is based on a converter I wrote while working at Zalora.  Thanks to the Pipes library, it is capable of parsing very large files without running out of memory.

# Installation & Setup

TODO: write this part

# Supported MySQL Column Types

* Text (tiny, medium, large)
* Char
* Varchar
* Date
* Time
* Timestamp
* Datetime
* Bit
* Blob
* Int (tiny, small, medium, big)
* Double
* Float

In addition to porting these column types to their closest compatible PostgreSQL data type, it has fixes for the following cases:

* CHAR(0) and VARCHAR(0) converts to CHAR(1)

# TODO

List of features I'd like to add.

* Addressing time zone differences between source and destination (not sure if this is reasonable)
* Fixing out of bounds dates (eg. February 31st)
