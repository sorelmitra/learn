#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "addressbook.pb-c.h"

int main(int argc, char* argv[]) {
  Tutorial__AddressBook tab;
  Tutorial__Person *pers = NULL;
  int i;
  char *pack_buf;
  size_t pack_len;
  size_t res;

  tutorial__address_book__init(&tab);

  tab.n_person = 1;
  tab.person = (Tutorial__Person **) malloc(tab.n_person * sizeof (Tutorial__Person *));

  for (i = 0; i < tab.n_person; i++) {
    tab.person[i] = (Tutorial__Person *) malloc(sizeof (Tutorial__Person));
  }

  pers = tab.person[0];
  tutorial__person__init(pers);
  pers->id = 1;
  pers->name = "Sorel";
  pers->email = "sorelmitra@yahoo.com";

  pack_len = tutorial__address_book__get_packed_size(&tab);
  pack_buf = (char *) malloc(pack_len * sizeof(char));

  tutorial__address_book__pack(&tab, pack_buf);

  errno = 0;
  res = fwrite(pack_buf, sizeof(char), pack_len, stdout);
  if (res != pack_len) {
    fprintf(stderr, "Could not write %d chars to stdout: %s\n",
        pack_len, strerror(errno));
  } else {
    fprintf(stderr, "Wrote %d chars to stdout.\n", pack_len);
  }

  return 0;
}

