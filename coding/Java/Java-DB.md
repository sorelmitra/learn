# JPA

## Overview

https://www.infoworld.com/article/3379043/what-is-jpa-introduction-to-the-java-persistence-api.html

JPA stands for Java Persistence API, and is an Object Relational Mapping (ORM) for storing, accessing, and managing Java Objects in a datastore such as a database.

## Spring Data JPA

### `CrudRepository`

To use, first you annotate your data classes with things like:

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	@Column(name = "creation_time", nullable = false)
	@Type(type = "org.jadira.usertype.dateandtime.threeten.PersistentZonedDateTime")

Then you extend the `CrudRepository` interface like this:

	@Component
	public interface IntentActionJpaRepository extends CrudRepository<IntentAction, Long> {
		
	    Iterable<IntentAction> findByTenantAndAutomationExperienceAndIntent(String tenant, String automationExperience,
	            String intent);
				
	    List<IntentAction> findByTenantAndAutomationExperience(String tenant, String automationExperience);
	}

Finally you use your JPA repo like this:

	Iterable<IntentAction> mappings = repoInterface.findByTenantAndAutomationExperienceAndIntent(tenant,
	                automationExperience, intent);



# Hibernate

[Hibernate](https://hibernate.org) is a collection of libraries and tools for data handling.  It includes solutions for [ORM](https://hibernate.org/orm/what-is-an-orm/), search, validation, including a reactive model for DB access.

Hibernate ORM is concerned with providing a database layer over JDBC and JPA.  It enables you to develop persistent classes following natural Object-oriented idioms including inheritance, polymorphism, association, composition, and the Java collections framework.  It's highly performant, scalable, extensible.
