How to Optimize SQL Queries

Description: Description: 18201326.jpgThe purpose of this article is to tell
you how I am learning to understand the work of SQL server instead of giving
you a set of ready instructions.  Only through the understanding of how it
works, you can write the optimum query. When we rely on instructions, advice,
special means, or even guesses that tell us: “in such a case, do this” but
without understanding “why and how will it work?” – this is frightening because
it may work, yes, it may work! Though it will work only in the beginning but
later, when you will have added millions of records to the database, everything
will get stuck, as ill luck would have it, during the presentation to the
management.


It may seem surprising but to understand such a complicated thing as SQL server
I use…my imagination.

I imagine a query, or rather how it will work. How?  It is like searching in
the book or in several books if you have several tables. Also, I imagine
alphabetical indices and sheets of paper (for buffers and temporary tables).

That’s why it's hard for me to optimize a request written earlier - I have no
idea how it works. To imagine a query, I actually have to rewrite it again.
Explain Select does not help my imagination; on the contrary, it completely
kills it. I do not believe in the technique "write a query somehow, and then
look in Explain Select and optimize it” at least because the output of Explain
Select in case of small and big amount of data will differ, besides, it will
depend on particular values in the query.

So first of all I imagine how a query should work and I write it. Then I
compare it with Explain Select (if I’m not too lazy to do this) and check if my
imagination coincides with the reality or not. If not, this is the best variant
– then I learn something new and adjust my understanding.

Now let’s turn to the practice.

Let’s imagine that we want to find places with a certain term in a book and
analyze its context – the words used before and after the term.  What will I do
if the book does not have an alphabetical index for terms? Right, I would read
the book, page by page. The complexity of the task is easy to assess in the
imagination - it is proportional to the size of the book.

But what will I do if I find the term? I can either start analyzing its
“context”, that is in terms of DBMS – I would return the series to the client.
It’s ok, but there must be an understanding that the client receiving the first
series that meets the search criteria does not know how many results will be
received in the future, and sometimes we are so anxious to know it!

Is it possible to return to the previous term? No! Try to read a book from the
end – you won’t do it. At best, you can query again.

Eureka! There is another way: I can read the whole book, writing the found
terms together with their contexts at the paper and then give the whole piece
of paper to the client, let him choke on it. In this case we know the number of
results and can sort them out back and forth, but we have to wait until we read
the whole book (though maybe one or two of the results would have been enough?)
and waste the paper (memory).  Well, that makes you think: what is the size of
the paper (if it is limited, then would all the results fit into it and what
would happen if they would not) and what if the client would choke on a piece
of paper of this size?  This gives an understanding that saves from errors and
allows you to choose an optimum variant for the specific purpose, and most
importantly to know that this variant exists.  Do you know how it will work by
default in your case?

Another example.  Imagine that we want to find terms ending with "tion" and
sort them alphabetically. What will we do? We also will be leafing through the
book but we can not return the term as soon as we find it, we have to write it
down on the paper (I imagine it this way) and then start sorting all the terms
and return the piece of paper with the ordered terms.

Now everything seems quite simple, but what if we need not all the terms but
the very first one in alphabetical order (… ORDER BY term LIMIT 1)? We still
have to read the whole book but do we need to sort all the terms to find the
first one? No, we don’t. We can find the first term in alphabetical order
faster than to sort all the found terms, and it is evidently seen in the
imagination.  What has it to do with the optimization? In spite of the fact
that we choose only one term, we know how many terms have been found. Indeed,
for example, MySQL can give this number separately from the query, which allows
giving the first search result, and yet saying how many terms were found per a
single leafing through the book.

Now let’s turn to the interesting thing – the indices.

I imagine indices as an alphabetical index in the book: alphabetically sorted
terms with the page (record) numbers where the term is found, and the numbers
are given in ascending order, too.

Tell me, will the index help to find the terms ending with “mation”? I ask this
question at the interviews. I ask: “We have a Term column by which we make a
lot of queries like term like “%mation”, will you make an index for it?” It’s
hard to answer the question without imagining an alphabetical index. If we
imagine it, it becomes clear that it is convenient to search for the words with
a specific beginning, for example the words beginning with “auto” but not for
the words ending with “mation”.

However, one can argue that the search by an alphabetical index, even without
considering the fact that it is sorted, is faster than reading the whole large
book. This is an excellent question that arose thanks to our imagination
because it does not have a simple answer but if you asked yourself this
question, that means you are on the way of developing an optimum database
schema.

Okay, but what if I am looking for the information relating to the terms
beginning with "auto"? Then I will be leafing through the index until I find
the first term beginning with "auto", and I feel that I will do it quickly,
even if the index is large. I imagine how I found the first term, with a list
of pages (primary keys in DBMS) where the term is found on the right. So I
start leafing through to every indicated page (not forgetting to lick fingers)
and write the information out. Then I go to the next term and turn the pages of
the book further. It seems that turning the pages of the book in a chaotic
order is quite quickly but if most terms begin with “auto” and they are found
on a large number of pages, I will get tired of leafing through the book. It
would be better if I was reading it all page by page: from beginning to end.
Luckily, the database developers also understood this and developed the
automatic optimization that can always make a mistake, at the very right time.

It is important to understand the following: in this example, if I need some
information sorted by a certain term, I will get it without any problems
because I check an alphabetical index (of course if I use it) ordered by terms.
So adding an additional condition ORDER BY term will not lead to additional
costs. But what if I want to sort by the page number (by the primary key)?

I imagine the following index:

Automation: 10, 30, 117, 486 Authorization: 15, 24, 97, 248

If you choose one term by the condition term = “Automation”, then the results
will be selected by the page order, and if the condition is satisfied by a few
terms, the results will be ordered "by pieces," and we will have to sort them
on the paper.

As an exercise, try to understand how the query ColumnA = 10 and ColumnB = 15
will be executed if both columns are indexed, and what is a big difference with
the query: ColumnA < 10 and ColumnB > 15. If you understand, then Explain
Select will confirm if you are right or not. Just please don’t do the exercise
vice versa.

All the topic is really inexhaustible, and I can keep on writing about how to
imagine a certain SQL structure: inner&outer joins, aggregation,&grouping, etc.
but I think this is enough for the beginning. If you liked it, write your
comments, and I will continue.

I hope you understood my approach. Imagine the course of the query and then you
will write it correctly and you won’t have to optimize anything. Moreover, you
will feel how long this query will be executed in case of increasing amounts of
data, you will be able to cut super optimal killer queries, which will kill
your system in case of data growth in spite of its "optimality", replacing such
killer queries with fundamentally different approaches or NoSQL databases. By
the way, the approach described also works if you want to understand NoSQL
databases!

Viktor Pilipenko Senior developer Quality Center (Luxoft) 

