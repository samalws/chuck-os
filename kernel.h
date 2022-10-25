#include "shared.h"

enum IDType getIDType(ID id);

void* lookup(struct IDMap* map, ID key);
void insert(struct IDMap* map, ID key, const void* val);
void assign(struct IDMap* map, ID key, const void* val);
void delete(struct IDMap* map, ID key);

int kernelMain();
