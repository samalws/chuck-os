typedef char bool;

const bool false = 0;
const bool true = 1;

void memcpy(void* dest, const void* src, int n) {
  char* dest_ = dest;
  const char* src_ = src;
  while (n > 0) {
    *dest_ = *src_;
    dest_++;
    src_++;
    n--;
  }
}

typedef int ID;

enum IDType {
  PID = 0,
  MID = 1,
  TID = 2,
  IID = 3
};

enum IDType getIDType(ID id) {
  return id & 3;
}

// TODO all map functions are really inefficient
struct IDMap {
  int capacity;
  int size;
  int valSize;
  ID* keys;
  void* vals;
};

void* lookup(struct IDMap* map, ID key) {
  for (int i = 0; i < map->size; i++)
    if (map->keys[i] == key)
      return map->vals + i * map->valSize;
  return 0;
}

void insert(struct IDMap* map, ID key, const void* val);

void assign(struct IDMap* map, ID key, const void* val) {
  for (int i = 0; i < map->size; i++)
    if (map->keys[i] == key)
      memcpy(map->vals + i * map->valSize, val, map->valSize);
  insert(map, key, val);
}

void insert(struct IDMap* map, ID key, const void* val) {
  if (map->size + 1 == map->capacity) return;
  if (lookup(map, key) != 0) assign(map, key, val);
  map->keys[map->size] = key;
  memcpy(map->vals + map->size * map->valSize, val, map->valSize);
  map->size++;
}

void delete(struct IDMap* map, ID key) {
  bool found = false;
  for (int i = 0; i < map->size; i++)
    if (found || map->keys[i] == key) {
      found = true;
      map->keys[i] = map->keys[i+1];
      memcpy(map->vals + i * map->valSize, map->vals + (i+1) * map->valSize, map->valSize);
    }
  if (found)
    map->size--;
}

struct Program {
  bool sudo;
  int arity;
  void* inputs;
  void* startPoint;
};

struct IO {
  void* input;
  void* startPoint;
};

#define defineMap(name,capac,valType) \
ID name##Keys[capac]; \
valType name##Vals[capac]; \
struct IDMap name = { \
  .capacity = capac, \
  .size = 0, \
  .valSize = sizeof(valType), \
  .keys = name##Keys, \
  .vals = name##Vals \
};

#define defineSet(name,capac) \
ID name##Keys[capac]; \
struct IDMap name = { \
  .capacity = capac, \
  .size = 0, \
  .valSize = 0, \
  .keys = name##Keys, \
  .vals = (void*) 1 \
};

defineMap(programs, 100, struct Program);
defineMap(memBlks, 100, void*);
defineSet(tokens, 100);
defineMap(ios, 100, struct IO);
defineSet(linearIDs, 100);

int init() {
}
