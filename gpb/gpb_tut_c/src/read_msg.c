#include <stdio.h>
#include <stdlib.h>
#include "addressbook.pb-c.h"

#define MAX_MSG_SIZE 1024

static size_t
read_buffer (unsigned max_length, uint8_t *out)
{
  size_t cur_len = 0;
  uint8_t c;
  size_t nread;

  while ((nread=fread(out + cur_len, 1, max_length - cur_len, stdin)) != 0) {
    cur_len += nread;
    if (cur_len == max_length) {
      fprintf(stderr, "max message length exceeded\n");
      exit(1);
    }
  }

  return cur_len;
}


int main(int argc, char* argv[]) {
  Tutorial__AddressBook *tab;
  uint8_t buf[MAX_MSG_SIZE];
  size_t msg_len;
  int i;
  Tutorial__Person *pers = NULL;

  msg_len = read_buffer (MAX_MSG_SIZE, buf);

  // Unpack the message using protobuf-c.
  tab = tutorial__address_book__unpack(NULL, msg_len, buf);
  if (tab == NULL) {
    fprintf(stderr, "error unpacking incoming message\n");
    exit(1);
  }

  printf("Address Book contains %d persons:\n", tab->n_person);
  for (i = 0; i < tab->n_person; i++) {
    pers = tab->person[i];
    printf("person #%d: ID %d, %s, %s\n", i, pers->id,
        pers->name != NULL ? pers->name : "noname",
        pers->email != NULL ? pers->email : "no email");
  }
}

