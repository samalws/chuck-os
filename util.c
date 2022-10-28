#include "util.h"

void* _memcpy(void* dest, const void* src, long unsigned n) {
  char* dest_ = dest;
  const char* src_ = src;
  while (n > 0) {
    *dest_ = *src_;
    dest_++;
    src_++;
    n--;
  }
  return dest;
}

void _atoi(char* dest, long unsigned n) {
  if (n == 0) {
    dest[0] = '0';
    dest[1] = 0;
    return;
  }

  char* dest2 = dest;
  while (n != 0) {
    *dest2 = '0' + (char) (n%10);
    n /= 10;
    dest2++;
  }
  *dest2 = 0;
  dest2--;
  while (dest < dest2) {
    char tmp = *dest;
    *dest = *dest2;
    *dest2 = tmp;
    dest++;
    dest2--;
  }
}

void* lookup(struct MIDMap* map, ID key) {
  for (int i = 0; i < map->size; i++)
    if (map->keys[i] == key)
      return map->vals + i * map->valSize;
  return 0;
}

void insert(struct MIDMap* map, ID key, const void* val);

void assign(struct MIDMap* map, ID key, const void* val) {
  for (int i = 0; i < map->size; i++)
    if (map->keys[i] == key)
      _memcpy(map->vals + i * map->valSize, val, map->valSize);
  insert(map, key, val);
}

void insert(struct MIDMap* map, ID key, const void* val) {
  if (map->size + 1 == map->capacity) return;
  if (lookup(map, key) != 0) assign(map, key, val);
  map->keys[map->size] = key;
  _memcpy(map->vals + map->size * map->valSize, val, map->valSize);
  map->size++;
}

void delete(struct MIDMap* map, ID key) {
  bool found = false;
  for (int i = 0; i < map->size; i++)
    if (found || map->keys[i] == key) {
      found = true;
      map->keys[i] = map->keys[i+1];
      _memcpy(map->vals + i * map->valSize, map->vals + (i+1) * map->valSize, map->valSize);
    }
  if (found)
    map->size--;
}
