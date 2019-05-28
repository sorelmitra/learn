all: write_msg read_msg

%.pb.cc: %.proto
	$(GPBC) $(GPBC_FLAGS) $?

OBJ_COMMON = addressbook.pb.o

OBJ_WRITE = write_msg.o \
	    $(OBJ_COMMON)

OBJ_READ = read_msg.o \
	    $(OBJ_COMMON)

write_msg: $(OBJ_WRITE) -lprotobuf

read_msg: $(OBJ_READ) -lprotobuf

clean:
	rm -fv *.o *.d *.a write_msg 

