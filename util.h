#include "shared.h"

void* _memcpy(void* dest, const void* src, long unsigned n);
void _atoi(char* dest, long unsigned n);
void* lookup(struct MIDMap* map, ID key);
void insert(struct MIDMap* map, ID key, const void* val);
void assign(struct MIDMap* map, ID key, const void* val);
void delete(struct MIDMap* map, ID key);
