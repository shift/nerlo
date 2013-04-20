all:
	(cd java; mvn clean compile assembly:single)
	(cd erl;$(MAKE) all)

