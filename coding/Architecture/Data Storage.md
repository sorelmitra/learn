# Data Storage

Depending on the app's needs, options for data storage can range from plain text files and AWS S3 to powerful RDBMS such as PostgreSQL or in-memory data stores such as Redis or to distributed data processing frameworks such as Hadoop.

---
---
---
---
---
---
---
---
---
---
---
---

# File System

An OS' file system can actually be a good storage option if you don't need a lot of interrogation and if the data structure fits into a hierarchical structure.

---
---
---
---
---
---
---
---
---
---
---
---

# Relational Database (RDBMS)

Here are some questions a developer should ask before deciding to use an RDBMS on a project:

- Will the data grow? Growing fixed format data is a good candidate for RDBMS. An ETL developer can build a pipeline to update the database as new data becomes available.

- Will data change? RDBMS excel at tracking historical changes. Developers can use temporal tables or slowly changing dimensions for value level changes. I've written in more detail about tracking historical changes here.

- Is the data bigger than what we can fit into memory? A 20 GB relational table is more accessible than a 20 GB flat file.

- Does the app need fast data processing? RDBMS can be faster at data processing than other system depending on the workload. Workloads sent to the database can use indexes and automatic parallelization for speed.

- Does my organizations have the proper infrastructure and skillets? Databases, especially on-premises deployments, need planning and maintenance. The degree of planning depends on the application workload requirements and existing infrastructure. A small commodity server can handle a database workload of 100 batches/sec under load. It's another story if the app is pushing 10K batches/sec under load. Database maintenance is another consideration. Are you performing back-ups, corruption checking, patching, index de-fragmentation, statistics updates? Cloud databases offload some of the infrastructure duties like backups and patching, but they are not maintenance free.

- Is the data sensitive? Database products offer a plethora of encryption, auditing, and alerting features. SQL Server, for example, offers data encryption, auditing, and alerting out of the box. It's possible to recreate a lot of these features without a database but as Codd put it, “at what cost?”

- Do I need transactions? A balance transfer of $100 from my savings to checking account is an atomic transaction made up of two database queries. One query debits $100 from the savings ledger and one query credits $100 to the checking ledger. These two queries are indivisible. If one query fails, then the entire transaction fails. If a system failure occurred between queries, then I would not want the $100 debit to succeed and the $100 credit to fail. Otherwise, I'd lose $100!

- How much flexibility do I need? Updating a relational schema is slow and cumbersome. If a project is in the exploratory phase, then a database will add unnecessary complexity. User requirements should drive the data model. The data model drives implementation details. Importing data into a database should not be the end but the means to some end.

- https://bookdown.org/msharkey3434/ShinyDB_Book/intro.html

What I Wish Someone Told Me About PostgreSQL
Lessons about PostgreSQL after using it in real life
https://challahscript.com/what_i_wish_someone_told_me_about_postgres

## Joins

https://www.edureka.co/blog/sql-joins-types

We assume the statement

    SELECT Table1.Column1,Table1.Column2,Table2.Column1,....
    FROM Table1
    XXX JOIN Table2
    ON Table1.Column1 = Table2.Column2

where `XXX JOIN` is any of the joins below.

---

Then, we have:

Legend: ------------: Table boundary; ============: Join boundary.

---

|------------|
| Table 1    |
|============|
| INNER JOIN |
|============|
| Table 2    |
|------------|

- Gets the intersection between the tables
- Both items must be non-NULL in order for them to be retrieved

---

|====================|
| Table 1            |
|--------------------|
| FULL (OUTER) JOIN  |
|--------------------|
| Table 2            |
|====================|

- Gets the reunion between the tables
- Items from at least one of the tables must be non-NULL in order for them to be retrieved

---

|====================|
| Table 1            |
|--------------------|
| LEFT (OUTER) JOIN  |
|====================|
| Table 2            |
|--------------------|

- Gets the intersection between the tables PLUS the remainder of Table 1

---

|--------------------|
| Table 1            |
|====================|
| RIGHT (OUTER) JOIN |
|--------------------|
| Table 2            |
|====================|

- Gets the intersection between the tables PLUS the remainder of Table 2

## Pagination

### Page-Based

With Node.JS, TypeOrm, and PostgreSQL, you would do something along the lines of:

    const queryBuilder = this.myEntityRepository
        .createQueryBuilder('entity')
        .where('entity.field = :field', { field: input.field });

    const data = await queryBuilder
        .skip(input.pageNumber)
        .take(input.pageSize)
        .orderBy('create_date', 'DESC')
        .getMany();

### Cursor-Based

With cursors, the above would change to something like this:

    const queryBuilder = this.myEntityRepository
        .createQueryBuilder('entity')
        .where('entity.field = :field', { field: input.field });

    const limit = isNaN(input.limit) ? DEFAULT_PAGE_SIZE : input.limit;
    this.logger.log(`Using limit ${limit} and cursor ${input.cursor}`);
    let paginatedQueryBuilder = queryBuilder.orderBy('create_date', 'DESC');
    if (input.cursor) {
        paginatedQueryBuilder = paginatedQueryBuilder.andWhere('entity.createDate < :cursorDate', { cursorDate: input.cursor });
    }
    const data = await paginatedQueryBuilder.take(limit).getMany();

But things are not that simple, as comparing `datetime` objects in PostgreSQL is complicated by timezone differences, and by the way PostgreSQL handles those objects.

A working query:

    SELECT * FROM entities e
    WHERE  e.tenant_id = '93d011dc-f34f-47ab-883a-c419d3c02567' AND e.create_date < '2024-10-08 18:53:35.651' ORDER BY create_date

But the above query relies on date strings, and the above string is not ISO 8601 compliant.  Variant with ISO 8601:

    SELECT * FROM entities e
    WHERE  e.tenant_id = '93d011dc-f34f-47ab-883a-c419d3c02567' AND e.create_date::date < '2024-10-08T18:53:35.651Z' ORDER BY create_date

But this one converts each DB field to `date`, which is costly.  Also, still suffers from timezone issues.

More resources:

Cursor pagination [PostgreSQL MySQL]
https://bun.uptrace.dev/guide/cursor-pagination.html#example

PostgreSQL - How to add parameter values to PGAdmin SQL query? - Stack Overflow
https://stackoverflow.com/questions/32995923/how-to-add-parameter-values-to-pgadmin-sql-query

How to Compare Dates in DateTime Fields in PostgreSQL | Delft Stack
https://www.delftstack.com/howto/postgres/postgresql-compare-dates-in-datetime-field/

## Diagrams

Entity Relationship Diagram (ERD)
https://www.databasestar.com/entity-relationship-diagram/

---
---
---
---
---
---
---
---
---
---
---
---

# In-Memory Database (IMDB)

IMDBs work by keeping all data in RAM. That is the medium in which data is stored in RAM versus disks or SSDs. IMDBs essentially replace the disk-accessing component of disk-based databases with RAM accesses. In some IMDBs, a disk-based component remains intact, but RAM is the primary storage medium. Since RAM is volatile (e.g., data is lost if the computer loses power), some IMDBs also store data on disk as a preventative measure to minimize the risk of data loss.

Most IMDBs also guard against data loss in a single data center (a capability known as “high availability”) by keeping copies (“replicas”) of all data records in multiple computers in a cluster. This data redundancy ensures that any data record will not be lost upon failure of any given computer.

An example use case would be a high traffic eCommerce site that needs to store shopping cart contents for hundreds of thousands of customers at any given time. Response times at that scale would be too slow for many disk-based databases, so IMDBs are used to keep up with the load and ensure a positive customer experience.

- https://hazelcast.com/glossary/in-memory-database/

---
---
---
---
---
---
---
---
---
---
---
---

# NoSQL Databases

## DynamoDB

- Schema-less.
- Supports single-table design.
- Puts data in Partitions based on the `Partition Key` which is part of the Primary Key.
- Partitions are automatically added as the data size increases.
- The DynamoDB API has three main types of actions:
	- Single-item requests (PutItem, GetItem, UpdateItem, and DeleteItem) that act on a single, specific item and require the full primary key;
	- Query, which can read a range of items and must include the partition key;
	- Scan, which can read a range of items but searches across your entire table.
- Secondary index - contains a subset of attributes from a table along with an alternate key to index on.  Supports Query operations via the `IndexName` parameter.
- Supports single-table design, where joins are "pre-made"

Sources:

Single-table design - https://www.alexdebrie.com/posts/dynamodb-single-table/

Partitions - https://www.alexdebrie.com/posts/dynamodb-partitions/

Query Data - https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GettingStarted.NodeJs.04.html

Indexes - https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/SecondaryIndexes.html

Single-Table Design with DynamoDB - https://www.alexdebrie.com/posts/dynamodb-single-table/

---
---
---
---
---
---
---
---
---
---
---
---

# ElasticSearch

Indexes data from multiple sources, stores those indexes and provides very fast search results.

Launch raw ElasticSearch queries: AWS -> ElasticSearch -> Choose your Role -> Dashboard -> Choose one of the available (and relevant) Domains -> Overview -> Link next to "Kibana" -> Dev Tools -> Console

To get all the available fields, use this query:

	GET /loans/_mapping

https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-get-mapping.html

Default query:

	GET /_search
	{
	  "query": {
	    "match_all": {}
	  }
	}

In one of my previous projects there was an index named `.kibana_1` which included a field `index-pattern.fields` that also included all available fields, this time with nice info such as whether this aggregates or not.
According to [this response](https://discuss.elastic.co/t/what-are-kibana-index-and-kibana-task-manager/208107/2), this index comes with [Kibana](https://www.elastic.co/guide/en/kibana/current/console-kibana.html), and starting with some version "the .kibana index has been transformed to .kibana_x (there is also a .kibana alias that points at the latest .kibana_x index) which is used to maintain backup when upgrading/migrating. For example if your .kibana_1 index is your main index and then you upgrade your Kibana instalation, Kibana will copy that index into .kibana_2, perform the migrations and then change the .kibana alias to point to .kibana_2."

---
---
---
---
---
---
---
---
---
---
---
---

