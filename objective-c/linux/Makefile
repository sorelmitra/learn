all: a b

a: a.m
	gcc -o $@ -lobjc $?

b: b.mm
	g++ -o $@ -lobjc $?

clean:
	rm a b

