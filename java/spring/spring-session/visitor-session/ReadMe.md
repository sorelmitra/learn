Attempts to use Spring Session in the way that's not recommended in their docs: Use one of the SessionRepository implementations directly, with a target to be able to search for a custom Session field.

Apart from a fiew small classes, all of the relevant code is in the unit tests.

Result: I did find something that works, but it's not nice, as you have to extend from one of Spring's SessionRepository implementation with a class that resides in the same package, then iterate over a map to convert all repo's specific Session implementation to the Session interface itself.

Details below.

Research on Spring Session:

1) Have the ability to query by our own field: failure.

a, b) Find all sessions by attribute value: Succeeded with point iii), below.

JdbcOperationsSessionRepository offers a findByIndexNameAndIndexValue() that would be able to search by a single attribute designated to be "principal name". Still:

i) Cannot be done outside of JdbcOperationsSessionRepository, because its findByIndexNameAndIndexValue() returns a Map<String, JdbcSession>

ii) Don't bother to extend from JdbcOperationsSessionRepository, because they made sure JdbcSession isn't visible in extended classes either...

iii) Extend from class JdbcOperationsSessionRepository with a class that resides in the same package (thus it can see JdbcSession): It works, but it's not nice. You have to wrap the call to findByIndexNameAndIndexValue() in another method that would then iterate over the map and fill in another map with the same key and the value converted from JdbcSession to Session.

c) Extend from Session to add more fields to it: Cannot be done.
- Not possible to do it outside of JdbcOperationsSessionRepository, because the latter's Session implementation is in the inner class JdbcSession, which is not public, and thus cannot be extended.
- And just in case someone wanted to extend from JdbcOperationsSessionRepository and do this stuff in the extended class, they've made the inner class JdbcSession final... so you cannot extend from the session class to add more fields

2) Does the solution found at #1 work for the Redis Spring repo?

Solution 1) a) iii) should work for RedisOperationsSessionRepository, by the same mechanism. Only this time the culprit is RedisSession.

